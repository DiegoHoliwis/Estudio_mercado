options(warn = -1)

# Librerias
# Se realiza la carga de las librerias necesarias
suppressMessages(library(tidyverse))
suppressMessages(library(rvest))

print(paste0("Ejecución realizada a las ",Sys.time()))

# Se carga la ultima base de datos disponible
load("~/Proyectos/Análisis de mercado/RTarapaca.RData")

# Buscamos los anuncio
URL <- read_html("https://www.trabajando.cl/trabajo-tarapaca?order=3") %>% 
  html_nodes(xpath = "//a[(contains(@class, 'resultOfferLink'))]") %>% 
  html_attr("href")

# Creamos el nuevo vector de datos
Trabajos2 <- dplyr::tibble(Empresa           = character(0),
                           CategoriaE        = character(0),
                           Titulo            = character(0),
                           Fecha_Publicacion = character(0),
                           Fecha_Termino     = character(0),
                           Descripción       = list(0),
                           Tipo_cargo        = character(0),
                           Vacantes          = numeric(0),
                           Region            = character(0),
                           Comuna            = character(0),
                           Jornada           = character(0),
                           Requisitos        = list(0) , 
                           Experiencia       = character(0),
                           Estudios          = character(0),
                           URL               = character(0),
                           Descripción_C     = character(0))


# Extraemos de forma secuencial la información de los anuncios
for (i in 1:20) {
  Sys.sleep(2)
  df <- read_html(URL[i])
  
  # Existencia
  # Se comprueba que la publicación existe, ya que algunas pueden existir la URL, más no la publicación
  Existencia <- df %>% html_element(xpath = "//div[contains(@class,'col-md-12 bk_left detalle_oferta h2_sub')]") %>% 
    html_text2() %>% str_count("La oferta de empleo que buscas ya no existe")
  if(Existencia>0){next}
  
  # Se extrae el titlo de la publicación
  repeat{
    Titulo <- df %>% html_node(css = ".offerTitleText") %>% html_text2()
    if(identical(Titulo, character()) == FALSE & Titulo !=''){break}}
  
  # Se extrae la descripción de la publicación
  repeat{
    Descripción <- df %>% html_element(xpath = "//div[contains(@itemprop, 'description')]") %>% 
      html_text2() %>% 
      str_replace_all("[:punct:]"," ") %>% 
      str_replace_all("\n\n","\n") %>% 
      str_split_fixed("\n",str_count(.,"\n")+1) %>% 
      str_trim() %>% 
      list()
    
    Descripción_C <- Descripción %>% unlist() %>% str_c(collapse = " ")
    
    if(identical(Descripción, character()) == FALSE){break}}  
  
  # Saltamos el trabajo si este ya se encuentra en la data, para ello se comprueba si:
  # Se tiene una coincidencia en la descripción + titulo o URL
  
  if(Descripción_C %in% Trabajos$Descripción_C & Titulo %in% Trabajos$Titulo | URL[i] %in% Trabajos$URL){next}
  
  # Se extrae el nombre de la empresa
  repeat{
    Empresa <- df %>% html_node(css = ".nombre_empresa") %>% html_text2()
    if(identical(Empresa, character()) == FALSE & Empresa !=''){break}}
  
  # Se extrae la categoria de la empresa
  repeat{
    CategoriaE <- df %>% html_node(css = ".categoria_empresa") %>% html_text2()
    if(identical(CategoriaE, character()) == FALSE & CategoriaE !=''){break}}
  
  # Se extrae la fecha de publicación
  repeat{
    Fecha_I <- df %>% html_nodes(css = ".fecha_oferta") %>% .[1] %>% html_text2()
    if(identical(Fecha_I, character()) == FALSE & Fecha_I !=''){break}}
  
  # Se extrae la fecha de termino del empleo
  repeat{
    Fecha_T <- df %>% html_nodes(css = ".fecha_oferta") %>% .[2] %>% html_text2()
    if(identical(Fecha_T, character()) == FALSE & Fecha_T !=''){break}}  
  
  # Se extrae un conjunto de atributos desde un objeto
  datos <- data.frame(variable = df %>% 
                        html_elements(xpath = "//div[contains(@class, 'col-md-4')]") %>%  
                        html_text2(),
                      Descripción = df %>% 
                        html_elements(xpath = "//div[contains(@class, 'col-md-8 txt')]") %>% 
                        html_text2() %>%
                        .[(1:length(datos1))]) %>% 
    slice(1:17) 
  
  
  Tipo_cargo  <- datos %>% filter(Variable ==  "Tipo de Cargo:") %>% select(Descripción) %>% as.character()
  Vacantes    <- datos %>% filter(Variable ==  "Vacantes:") %>% select(Descripción) %>% as.numeric()
  Region      <- datos %>% filter(Variable ==  "Región:") %>% select(Descripción) %>% as.character() %>% str_remove("\n")
  Comuna      <- datos %>% filter(Variable ==  "Comuna:") %>% select(Descripción) %>% as.character() %>% str_remove("\n")
  Jornada     <- datos %>% filter(Variable ==  "Jornada") %>% select(Descripción) %>% as.character()
  
  Experiencia <- datos %>% filter(Variable ==  "Experiencia Mínima:") %>% select(Descripción) %>% as.character()
  Estudios    <- datos %>% filter(Variable ==  "Estudios mínimos:") %>% select(Descripción) %>% as.character()
  Reqiusitos  <- datos %>% filter(Variable ==  "Requisitos Minimos:") %>% select(Descripción) %>% as.character() %>%
    str_replace_all("[:punct:]"," ") %>% 
    str_split_fixed('\n',str_count(.,"\n")+1) %>% 
    str_trim() %>% 
    list()
  
  
  # Agregamos los datos al vector
  Trabajos2  <- Trabajos2 %>% 
    add_row(Empresa           = Empresa,
            CategoriaE        = CategoriaE,
            Titulo            = Titulo,
            Fecha_Publicacion = Fecha_I,
            Fecha_Termino     = Fecha_T,
            Descripción       = Descripción,
            Tipo_cargo        = Tipo_cargo,
            Vacantes          = Vacantes,
            Region            = Region,
            Comuna            = Comuna,
            Jornada           = Jornada,
            Requisitos        = Reqiusitos, 
            Experiencia       = Experiencia,
            Estudios          = Estudios,
            URL               = URL[i],
            Descripción_C     = Descripción_C)
  print(paste0("Oferta laboral agregada, puede verla en ",URL[i]))
}

# Se añaden las nuevas publicaciones a la data central
Trabajos <- Trabajos2 %>% add_row(Trabajos)

# Se actualiza la data central
nombre <- paste0("~/Proyectos/Análisis de mercado/RTarapaca.RData",sep = "")
save(Trabajos,file = nombre)

# Mensaje para conR
print(paste0("Se agregaron ", nrow(Trabajos2), " ofertas laborales", sep = ""))
print("")
print("")
rm(list = ls())
