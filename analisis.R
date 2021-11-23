# Análisis y datos del artículo 
# Representaciones sociales del big data y la inteligencia artificial. Una exploración estructural.

# Librerías -------

library(tidyverse) # para manipular datos
library(widyr) # para grafos
library(ggraph) # para grafos
library(igraph) # para grafos
library(tidygraph) # para grafos
library(ggdendro) # para comparar diccionarios
library(broom)
library(ggfortify)
library(FactoMineR) # para factoriales de correspondencia
library(Factoshiny) # shiny para factoriales
library(gt) # para tablas con formato (parecido a) APA

# Carga de datos -------

sociodemograficos <- readr::read_csv("./data/sociodemograficos.csv")
terminos <- readr::read_csv("./data/terminos.csv") %>% tibble()

# Muestra -----

terminos %>% 
  inner_join(sociodemograficos) %>%
  filter(!is.na(estimulo)) %>% 
  group_by(estimulo) %>% count(id) %>% summarise(respuestas=n()) %>%
  inner_join(
    terminos %>% 
      inner_join(sociodemograficos) %>%
      filter(!is.na(estimulo))%>% 
      group_by(estimulo) %>% count(id) %>% summarise(asociaciones=sum(n))
  ) %>%
  inner_join(sociodemograficos %>%
               filter(!is.na(estimulo)) %>%
               group_by(estimulo) %>% summarise(edad_mean=mean(edad), edad_sd=sd(edad)) ) %>%
  
  inner_join(
    sociodemograficos %>%
      group_by(carrera2, estimulo) %>%
      summarise(n=n()) %>%
      pivot_wider(id_cols = estimulo, names_from = carrera2, values_from = n, 
                  # names_prefix = "car_"
      )
  ) %>%
  inner_join(
    sociodemograficos %>%
      group_by(sexo, estimulo) %>%
      summarise(n=n()) %>%
      pivot_wider(id_cols = estimulo, names_from = sexo, values_from = n)
  ) %>%
  arrange(desc(respuestas)) 

# RQ1. Analisis prototipico -----

crear_evok <- function( tabla, palabra, orden, valoracion, frecuencia_minima = 2) {
  
  stopifnot(is.data.frame(tabla))
  stopifnot(is.numeric(frecuencia_minima))
  
  palabra_column = enquo(arg = palabra)
  orden_column = enquo(arg = orden)
  valor_column = enquo(arg = valoracion)
  
  # frequency x rank table 
  freq_x_rank <- tabla %>% 
    group_by(!!palabra_column) %>% summarise(
      freq=n(), # frecuencia
      rank=mean(!!orden_column), # media de orden de evocacion
      assessm=mean(!!valor_column),  # media de valoracion
      .groups = 'drop_last'
    ) 
  
  # calculamos una frecuencia minima
  freq_x_rank <- freq_x_rank %>%
    group_by(!!palabra_column) %>%
    filter( freq >= frecuencia_minima)
  
  freq_cut <- mean(freq_x_rank$freq) # cut-off x frecuencias de evocacion
  rank_cut <- mean(freq_x_rank$rank) # cut-off x rango de evocacion
  message("frequency cut = ", freq_cut)
  message("rank cut = ", rank_cut)
  
  freq_x_rank <- freq_x_rank %>% mutate( q = case_when(
    freq >= freq_cut & rank < rank_cut ~ 1,
    freq >= freq_cut & rank >= rank_cut ~ 2,
    freq < freq_cut & rank < rank_cut ~ 3,
    freq < freq_cut & rank >= rank_cut ~ 4 
  )
  ) %>% arrange(q,desc(freq,rank))
  
  return(
    list(
      data = freq_x_rank ,
      parameters = list(
        "freq_cut" = freq_cut ,
        "rank_cut" = rank_cut ,
        "min_cut" = frecuencia_minima ,
        "dix_length" = nrow(freq_x_rank) ,
        "q1_length" = sum(freq_x_rank$q == 1) ,
        "q2_length" = sum(freq_x_rank$q == 2) ,
        "q3_length" = sum(freq_x_rank$q == 3) ,
        "q4_length" = sum(freq_x_rank$q == 4) 
      )
    )
  )
}

evok_bd <- crear_evok(tabla = terminos %>% filter(estimulo == "Big data"), 
                      palabra = palabra, orden = orden, valoracion = valoracion, 
                      frecuencia_minima = 5)

evok_ai <- crear_evok(tabla = terminos %>% filter(estimulo == "Inteligencia artificial"), 
                      palabra = palabra, orden = orden, valoracion = valoracion, 
                      frecuencia_minima = 4)

evok_ds <- crear_evok(tabla = terminos %>% filter(estimulo == "Ciencia de datos"), 
                      palabra = palabra, orden = orden, valoracion = valoracion, 
                      frecuencia_minima = 2)

evok_cc <- crear_evok(tabla = terminos %>% filter(estimulo == "Conocimiento"), 
                      palabra = palabra, orden = orden, valoracion = valoracion, 
                      frecuencia_minima = 2)

evok <- rbind( evok_bd$data %>% mutate(estimulo="Big data") ,
               evok_ai$data %>% mutate(estimulo="IA"), 
               evok_ds$data %>% mutate(estimulo="Ciencia de datos") ,
               evok_cc$data %>% mutate(estimulo="Conocimiento") 
)

stat_evok <- rbind(unlist(evok_bd$parameters), 
                   unlist(evok_ai$parameters), 
                   unlist(evok_ds$parameters), 
                   unlist(evok_cc$parameters))
rownames(stat_evok) <- c("Big data", "Inteligencia artificial", "Ciencia de datos", "Conocimiento")

data.frame(stat_evok) %>% # tabla de parametros usados para los analisis
  rownames_to_column(var = "Estimulo") %>% 
  select(Estimulo, freq_cut, rank_cut, min_cut, dix_length) 

evok_bd$data %>% ggplot(aes(x=freq,y=rank,label=palabra)) + # terminos x n x ofe
  scale_x_continuous(trans='log') +
  geom_hline(yintercept = evok_bd$parameters$rank_cut, linetype = 2) +
  geom_vline(xintercept = evok_bd$parameters$freq_cut, linetype = 2) +
  geom_point(aes(colour=assessm), show.legend = FALSE) +
  scale_colour_gradient(low = "red", high = "green", na.value = NA) +
  geom_text( aes(size=20, colour=assessm), fontface = "bold",
             show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
  labs(y="Orden de evocación", x = "Frecuencia (log2)") +
  theme_minimal() 

evok_ai$data %>% ggplot(aes(x=freq,y=rank,label=palabra)) + # terminos x n x ofe
  scale_x_continuous(trans='log') +
  geom_hline(yintercept = evok_ai$parameters$rank_cut, linetype = 2) +
  geom_vline(xintercept = evok_ai$parameters$freq_cut, linetype = 2) +
  geom_point(aes(colour=assessm), show.legend = FALSE) +
  scale_colour_gradient(low = "red", high = "green", na.value = NA) +
  geom_text( aes(size=20, colour=assessm), fontface = "bold",
             show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
  labs(y="Orden de evocación", x = "Frecuencia (log2)") +
  theme_minimal()


# RQ2. Relaciones entre diccionarios -----


# calculamos distancias entre diccionarios

ellegards_index <- function(dix1, dix2) {
  nc <- length(intersect(dix1,dix2)) 
  n1 <- length(dix1) 
  n2 <- length(dix2)
  return(nc / sqrt(n1*n2))
}
palabras_ai <- evok_ai$data %>% filter(q<4) %>% pull(palabra)
palabras_bd <- evok_bd$data %>% filter(q<4) %>% pull(palabra)
palabras_cd <- evok_ds$data %>% filter(q<4) %>% pull(palabra)
palabras_con <- evok_cc$data %>% filter(q<4) %>% pull(palabra)
dix1 <- palabras_ai
dix2 <- palabras_bd
dix3 <- palabras_cd
dix4 <- palabras_con
v1 <- paste0('dix', 1:4)
v1 <- c("palabras_ai","palabras_bd","palabras_cd","palabras_con")
distancia_dix <- outer(v1, v1, Vectorize(function(x, y) ellegards_index(get(x), get(y))))
dimnames(distancia_dix) <- list(v1, v1)
rm(dix1,dix2,dix3,dix4,v1,palabras_ai,palabras_bd,palabras_cd,palabras_con)

x<-evok_bd$data %>% filter(q<4) %>% ungroup() %>% 
  mutate(ranking=row_number()) %>% select(palabra, ranking, q)

y<-evok_ai$data %>% filter(q<4) %>% ungroup() %>% 
  mutate(ranking=row_number()) %>% select(palabra, ranking, q)

x %>% inner_join(y, by="palabra") 
rm(x,y)

# hacemos los graficos de los otros estimulos
evok %>%  filter(estimulo %in% c("Conocimiento", "Ciencia de datos")) %>%
  ggplot(aes(x=freq,y=rank,label=palabra)) + # terminos x n x ofe
  scale_x_continuous(trans='log') +
  scale_colour_gradient(low = "red", high = "green", na.value = NA) +
  geom_text( aes(size=10, colour=assessm),
             show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
  geom_point(aes(colour=assessm), show.legend = FALSE) +
  labs(y="Orden de evocación", x = "Frecuencia (log2)") +
  theme_minimal() +
  facet_wrap(~estimulo) +
  geom_vline(data = subset(evok, estimulo == "Ciencia de datos"),
             aes(xintercept = evok_ds$parameters$freq_cut), linetype = 2) +
  geom_vline(data = subset(evok, estimulo == "Conocimiento"),
             aes(xintercept = evok_cc$parameters$freq_cut), linetype = 2) +
  geom_hline(data = subset(evok, estimulo == "Ciencia de datos"),
             aes(yintercept = evok_ds$parameters$rank_cut), linetype = 2) +
  geom_hline(data = subset(evok, estimulo == "Conocimiento"),
             aes(yintercept = evok_cc$parameters$rank_cut), linetype = 2) 

row.names(distancia_dix) <- c("Inteligencia artificial", "Big data", "Ciencia de datos", "Conocimiento")
ggdendrogram(hclust(dist(distancia_dix)), rotate = FALSE, size = 1)

# RQ3. Redes de asociaciones -----

red_bd_cor <- terminos %>%
  inner_join(evok_bd$data) %>%
  filter(q<4) %>%
  select(id,palabra) %>%
  widyr::pairwise_cor(item = palabra, feature = id) %>%
  graph_from_data_frame( directed = FALSE ) %>% as_tbl_graph() %>%
  activate(edges) %>%
  filter(correlation>0) %>%
  activate(nodes) %>%
  left_join( evok_bd$data %>% rename(name=palabra) ) %>%
  mutate(community=as.factor(group_leading_eigen()))

red_ai_cor <- terminos %>%
  inner_join(evok_ai$data) %>%
  filter(q<4) %>%
  select(id,palabra) %>%
  widyr::pairwise_cor(item = palabra, feature = id) %>%
  graph_from_data_frame( directed = FALSE ) %>% as_tbl_graph() %>%
  activate(edges) %>%
  filter(correlation>0) %>%
  activate(nodes) %>%
  left_join( evok_ai$data %>% rename(name=palabra) ) %>%
  mutate(community=as.factor(group_leading_eigen()))

red_bd_cor %>%
  ggraph(layout = "fr" ) +  #drl, fr, dh
  geom_edge_link(aes(width = correlation, alpha = correlation), color="gray", show.legend = TRUE )+
  geom_node_point(aes(color=community), show.legend = TRUE) +
  geom_node_text(aes(label = name ), repel = TRUE, show.legend = FALSE)+
  theme_graph() 

red_ai_cor %>%
  ggraph(layout = "fr" ) +  #drl, fr, dh
  geom_edge_link(aes(width = correlation, alpha = correlation), color="gray", show.legend = TRUE )+
  geom_node_point(aes(color=community), show.legend = TRUE) +
  geom_node_text(aes(label = name ), repel = TRUE, show.legend = FALSE)+
  theme_graph()



# RQ4. Correspondencias carrera-fuentes -----


# tabla de fuentes consultadas por carrera
carrera_medios_bd %>% 
  dplyr::rename_all(funs(
    stringr::str_replace_all( ., "[[:punct:]]", "_" ) %>%
      stringr::str_replace_all( ., "medios_", "" )
  )) %>%
  inner_join(carrera_medios_ai %>% 
               dplyr::rename_all(funs(
                 stringr::str_replace_all( ., "[[:punct:]]", "_" ) %>%
                   stringr::str_replace_all( ., "medios_", "" ))), by ="carrera2")

# factoriales
carrera_medios_bd <- sociodemograficos %>% filter(estimulo == "Big data") %>% 
  group_by(carrera2) %>%
  summarise(n=n(), p=n() / nrow(sociodemograficos %>% filter(estimulo == "Big data"))) %>% 
  inner_join(
    sociodemograficos %>% filter(estimulo == "Big data") %>% 
      group_by(carrera2) %>%
      summarise_if( .predicate = is.logical, .funs = function(x) sum(x) / n() ) , by="carrera2") %>%
  select(-n,-p) 

carrera_medios_ai <- sociodemograficos %>% filter(estimulo == "Inteligencia artificial") %>% 
  group_by(carrera2) %>%
  summarise(n=n(), p=n() / nrow(sociodemograficos %>% filter(estimulo == "Inteligencia artificial"))) %>% 
  inner_join(
    sociodemograficos %>% filter(estimulo == "Inteligencia artificial") %>% 
      group_by(carrera2) %>%
      summarise_if( .predicate = is.logical, .funs = function(x) sum(x) / n() ) , by="carrera2") %>%
  select(-n,-p) 

facto_bd <- terminos %>%
  filter(estimulo == "Big data",
         palabra %in% (evok_bd$data %>% filter(q<5) %>% pull(palabra))) %>%
  inner_join(sociodemograficos) %>%
  group_by(palabra,carrera2) %>% summarize(n=n()) %>%
  pivot_wider(names_from = carrera2, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "palabra") %>% 
  as.matrix()

facto_bd2 <- FactoMineR::CA(X = facto_bd, graph = FALSE)

facto_ai <- terminos %>%
  filter(estimulo == "Inteligencia artificial",
         palabra %in% (evok_ai$data %>% filter(q<5) %>% pull(palabra))) %>%
  inner_join(sociodemograficos) %>%
  group_by(palabra,carrera2) %>% summarize(n=n()) %>%
  pivot_wider(names_from = carrera2, values_from = n, values_fill = 0) %>%
  column_to_rownames(var = "palabra") %>%
  as.matrix()

facto_ai2 <- FactoMineR::CA(X = facto_ai, graph = FALSE)

ellipseCA(facto_bd2,ellipse=c('col'),cex=0.9,cex.main=0.9,cex.axis=0.9,title="Big data")
ellipseCA(facto_ai2,ellipse=c('col'),cex=0.9,cex.main=0.9,cex.axis=0.9,title="Inteligencia artificial")