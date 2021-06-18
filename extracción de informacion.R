
library(tidyverse)
library(wordcloud)    # Nube de palabras
library(wordcloud2)   # Nube de palabras vs2
library(tm)           # tokenización
library(RColorBrewer) # Palaeta de colores para la nube de palabras
library(SnowballC)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)


fecha <- dir('data') %>%
  str_remove_all("[:alpha:]") %>% 
  str_remove_all("[:punct:]") %>% 
  as.Date("%Y%m%d") %>% 
  max()

load(paste0("data/RTarapaca_",fecha,".RData",sep = ""))


# ======================================================================================
# ANALISIS DE PALABRAS INDIVIDUALES ----
# ======================================================================================
# Función de limpieza ==================================================================
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- str_to_lower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  # nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Palabras a eliminar ==================================================================
lista_stopwords <- {c('a','al','algo','algunas','algunos','ante','antes','como','con','contra','cual','cuando',
                     'de','del','desde','donde','durante','e','el','ella','ellas','ellos','en','entre','era','erais','eran',
                     'eras','eres','es','esa','esas','ese','eso','esos', 'esta','estaba','estabais','estaban','estabas',
                     'estad','estada','estadas','estado','estados','estamos','estando','estar','estaremos','estará','estarán',
                     'estarás','estaré','estaréis','estaría','estaríais','estaríamos','estarían','estarías','estas','este',
                     'estemos','esto','estos','estoy','estuve','estuviera','estuvierais','estuvieran','estuvieras','estuvieron',
                     'estuviese','estuvieseis','estuviesen','estuvieses','estuvimos','estuviste','estuvisteis','estuviéramos',
                     'estuviésemos','estuvo','está','estábamos','estáis','están','estás','esté','estéis','estén','estés','fue',
                     'fuera','fuerais','fueran','fueras','fueron','fuese','fueseis','fuesen','fueses','fui','fuimos','fuiste',
                     'fuisteis','fuéramos','fuésemos','ha','habida','habidas','habido','habidos','habiendo','habremos','habrá',
                     'habrán','habrás','habré','habréis','habría','habríais','habríamos','habrían','habrías','habéis','había',
                     'habíais','habíamos','habían','habías','han','has','hasta','hay','haya','hayamos','hayan','hayas','hayáis',
                     'he','hemos','hube','hubiera','hubierais','hubieran','hubieras','hubieron','hubiese','hubieseis','hubiesen',
                     'hubieses','hubimos','hubiste','hubisteis','hubiéramos','hubiésemos','hubo','la','las','le','les','lo',
                     'los','me','mi','mis','mucho','muchos','muy','más','mí','mía','mías','mío','míos','nada','ni','no','nos',
                     'nosotras','nosotros','nuestra','nuestras','nuestro','nuestros','o','os','otra','otras','otro','otros','para',
                     'pero','poco','por','porque','que','quien','quienes','qué','se','sea','seamos','sean','seas','sentid','sentida',
                     'sentidas','sentido','sentidos','seremos','será','serán','serás','seré','seréis','sería','seríais','seríamos',
                     'serían','serías','seáis','siente','sin','sintiendo','sobre','sois','somos','son','soy','su', 'sus','suya',
                     'suyas','suyo','suyos','sí','también','tanto','te','tendremos','tendrá','tendrán','tendrás','tendré','tendréis',
                     'tendría','tendríais','tendríamos','tendrían','tendrías','tened','tenemos','tenga','tengamos','tengan','tengas',
                     'tengo','tengáis','tenida','tenidas','tenido','tenidos','teniendo','tenéis','tenía','teníais','teníamos',
                     'tenían','tenías','ti','tiene','tienen','tienes','todo','todos','tu','tus','tuve','tuviera','tuvierais','tuvieran',
                     'tuvieras','tuvieron','tuviese','tuvieseis','tuviesen','tuvieses','tuvimos','tuviste','tuvisteis','tuviéramos',
                     'tuviésemos','tuvo','tuya','tuyas','tuyo','tuyos','tú','un','una','uno','unos','vosotras','vosotros','vuestra',
                     'vuestras','vuestro','vuestros','y','ya','yo','él','éramos')}

# Transformación de la lista de requisitos =============================================
Trabajos$Requisitos <-  Trabajos %>% 
  select(Requisitos) %>%
  apply(MARGIN = 1, function(x){unlist(x) %>% str_c(collapse = " ")})

# Procesamientos de los requisitos =====================================================
Palabras <- Trabajos %>% 
  select(Requisitos) %>% 
  mutate(texto_tokenizado = map(.x = Requisitos, .f = limpiar_tokenizar)) %>%
  select(texto_tokenizado) %>% 
  unnest(cols = c(texto_tokenizado)) %>% 
  rename(token = texto_tokenizado) %>% 
  filter(!(token %in% lista_stopwords)) %>% 
  group_by(token) %>% 
  count(token) %>% 
  ungroup() %>% 
  arrange(desc(n))

# Visualización gráfica ================================================================

# Barplot 
Palabras %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(token,n), y = n),) +
  geom_col( fill = "#2BECEB", color = "black") +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip()

# Nube de palabras
wordcloud2(Palabras, size=2.1, minSize = 1)

# Correlación entre palabras


# ======================================================================================
# ======================================================================================
# ANALISIS DE DUPLAS DE PALABRAS  ----
# ======================================================================================
# Palabras a eliminar ==================================================================
lista_stopwords <- {c('a','al','algo','algunas','algunos','ante','antes','como','con','contra','cual','cuando',
                      'de','del','desde','donde','durante','e','el','ella','ellas','ellos','en','entre','era','erais','eran',
                      'eras','eres','es','esa','esas','ese','eso','esos', 'esta','estaba','estabais','estaban','estabas',
                      'estad','estada','estadas','estado','estados','estamos','estando','estar','estaremos','estará','estarán',
                      'estarás','estaré','estaréis','estaría','estaríais','estaríamos','estarían','estarías','estas','este',
                      'estemos','esto','estos','estoy','estuve','estuviera','estuvierais','estuvieran','estuvieras','estuvieron',
                      'estuviese','estuvieseis','estuviesen','estuvieses','estuvimos','estuviste','estuvisteis','estuviéramos',
                      'estuviésemos','estuvo','está','estábamos','estáis','están','estás','esté','estéis','estén','estés','fue',
                      'fuera','fuerais','fueran','fueras','fueron','fuese','fueseis','fuesen','fueses','fui','fuimos','fuiste',
                      'fuisteis','fuéramos','fuésemos','ha','habida','habidas','habido','habidos','habiendo','habremos','habrá',
                      'habrán','habrás','habré','habréis','habría','habríais','habríamos','habrían','habrías','habéis','había',
                      'habíais','habíamos','habían','habías','han','has','hasta','hay','haya','hayamos','hayan','hayas','hayáis',
                      'he','hemos','hube','hubiera','hubierais','hubieran','hubieras','hubieron','hubiese','hubieseis','hubiesen',
                      'hubieses','hubimos','hubiste','hubisteis','hubiéramos','hubiésemos','hubo','la','las','le','les','lo',
                      'los','me','mi','mis','mucho','muchos','muy','más','mí','mía','mías','mío','míos','nada','ni','no','nos',
                      'nosotras','nosotros','nuestra','nuestras','nuestro','nuestros','o','os','otra','otras','otro','otros','para',
                      'pero','poco','por','porque','que','quien','quienes','qué','se','sea','seamos','sean','seas','sentid','sentida',
                      'sentidas','sentido','sentidos','seremos','será','serán','serás','seré','seréis','sería','seríais','seríamos',
                      'serían','serías','seáis','siente','sin','sintiendo','sobre','sois','somos','son','soy','su', 'sus','suya',
                      'suyas','suyo','suyos','sí','también','tanto','te','tendremos','tendrá','tendrán','tendrás','tendré','tendréis',
                      'tendría','tendríais','tendríamos','tendrían','tendrías','tened','tenemos','tenga','tengamos','tengan','tengas',
                      'tengo','tengáis','tenida','tenidas','tenido','tenidos','teniendo','tenéis','tenía','teníais','teníamos',
                      'tenían','tenías','ti','tiene','tienen','tienes','todo','todos','tu','tus','tuve','tuviera','tuvierais','tuvieran',
                      'tuvieras','tuvieron','tuviese','tuvieseis','tuviesen','tuvieses','tuvimos','tuviste','tuvisteis','tuviéramos',
                      'tuviésemos','tuvo','tuya','tuyas','tuyo','tuyos','tú','un','una','uno','unos','vosotras','vosotros','vuestra',
                      'vuestras','vuestro','vuestros','y','ya','yo','él','éramos')}

# Transformación de la lista de requisitos =============================================
Trabajos$Requisitos <-  Trabajos %>% 
  select(Requisitos) %>%
  apply(MARGIN = 1, function(x){unlist(x) %>% str_c(collapse = " ")})


# Análisis de palabras por n-gram ======================================================

bigram <- Trabajos %>% 
  select(Requisitos) %>% 
  map_df(.f = str_trim) %>% 
  map_df(.f = function(x) x %>% str_replace_all("á", "a") %>% 
           str_replace_all("é", "e") %>% 
           str_replace_all("í", "i") %>% 
           str_replace_all("ó", "o") %>% 
           str_replace_all("ú", "u")) %>% 
  unnest_tokens(output = palabras,
                input = Requisitos,
                to_lower = TRUE,
                format = "text",
                token = "ngrams",
                n = 2)


bigram_separate <- bigram %>% 
  separate(palabras, c("words1","words2"), sep = " ")
 
bigram_filter <- bigram_separate %>% 
  filter(!words1 %in% lista_stopwords) %>% 
  filter(!words2 %in% lista_stopwords) %>% 
  filter(is.na(words1) == FALSE) %>% 
  filter(is.na(words2) == FALSE)


bigram_counts <- bigram_filter %>% 
  count(words1, words2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame(directed=TRUE)


set.seed(2020)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

x11()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 4) +
  theme_void()

# Análisis de palabras pro correlación =================================================

seccion_palabra <- Trabajos %>%
  select(Requisitos) %>% 
  map_df(.f = str_trim) %>% 
  map_df(.f = str_to_lower) %>% 
  map_df(.f = function(x){str_replace_all(x,"[[:punct:]]", " ")}) %>% 
  map_df(.f = function(x){str_replace_all(x,"http\\S*", "")}) %>% 
  map_df(.f = function(x){str_replace_all(x,"[\\s]+", " ")}) %>% 
  mutate(publicacion = row_number()) %>% 
  unnest_tokens(output = palabras,
                input = Requisitos,
                to_lower = TRUE,
                format = "text",
                token = "words") %>% 
  anti_join(y = tibble(palabras = lista_stopwords), by = "palabras")


palabras_pairs <- seccion_palabra %>%
  pairwise_count(palabras,publicacion, sort = TRUE)


palabras_corr <- seccion_palabra %>%
  group_by(palabras) %>%
  filter(n() >= 25) %>%
  pairwise_cor(palabras, publicacion, sort = TRUE)

set.seed(2020)
palabras_corr %>%
  filter(correlation > .30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# ======================================================================================
