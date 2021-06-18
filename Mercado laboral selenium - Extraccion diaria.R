# Se carga la data generada por el web scraping - extracción primera vez
load("RTarapaca_Selenium.RData")

# Librerias
library(tidyverse)
library(RSelenium)

# Se busca la ultima fecha de las publicaciones, para tener un sistema de parada
Ultima_fecha <- Trabajos$Fecha_Publicacion[1] %>%
  str_remove_all("[:alpha:]") %>% 
  str_remove_all(": ") %>% 
  as.Date("%d/%m/%Y")


# Incialización de Rselenium
# Se inician los procesos de Rselenium, en este caso configurado en el browser de Chrome
# Se debe haber instalado previamente los controladores necesarios
# link: https://chromedriver.chromium.org/downloads

remote_driver <- RSelenium::rsDriver(browser = "chrome",
                                     chromever =
                                       system2(command = "wmic",
                                               args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                               stdout = TRUE,
                                               stderr = TRUE) %>%
                                       stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                       magrittr::extract(!is.na(.)) %>%
                                       stringr::str_replace_all(pattern = "\\.",
                                                                replacement = "\\\\.") %>%
                                       paste0("^",  .) %>%
                                       stringr::str_subset(string =
                                                             binman::list_versions(appname = "chromedriver") %>%
                                                             dplyr::last()) %>%
                                       as.numeric_version() %>%
                                       max() %>%
                                       as.character()) %>% 
  .[["client"]]

# Forma 2 de ejecución, si se conoce el chromever
# driver <- RSelenium::rsDriver(browser = "chrome", 
#                               chromever = "89.0.4389.23",
#                               port = 4568L)



# Iniciar navegador
# Se ejecuta el navegador en la pagina deseada, en caso de que se cierre la pestaña, no se
# requiere que se fuelva a inicializar Rselenium, solo a abrir otra vez la pestaña
# remote_driver$open()

Sys.sleep(1)
remote_driver$navigate("https://www.trabajando.cl/trabajo-tarapaca")




# Seleccionar trabajo
# En caso de ejecutar trabajando.cl, desde el incio, se seleccionan el cargo y ubicación

# Sys.sleep(1)
# cargo = ""
# ubicación = ""
# 
# # Agregamos puesto
# address_element1 <- remote_driver$findElement(using = 'id', value = 'palabra')
# address_element1$sendKeysToElement(list(cargo))
# # Agregamos ubicación
# address_element2 <- remote_driver$findElement(using = 'id', value = 'location')
# address_element2$sendKeysToElement(list(ubicación))
# # Generamos busqueda
# button_element <- remote_driver$findElement(using = 'class', value = "search-btn")
# button_element$clickElement()





# ordenamos por fecha 
# Se utiliza el filtro fecha para ordenar por fecha de publicación
option <- remote_driver$findElement(using = 'xpath', "//*/option[@value = '3']")
option$clickElement()
Sys.sleep(7)

# Nos ubicamos en el primer anuncio
anuncio <- remote_driver$findElements(using = 'xpath', value = '//div[contains(@class,"col-md-12 resultado_busquedas_listado_oferta d-flex flex-column resultPage_")]')
anuncio[[1]]$clickElement()

# Se busca el scrol Y de los anuncios
webElem <- remote_driver$findElement("css", "body")

# Se baja hasta el final del scroll Y para cargar todos los anuncios
for(i in 1:50){
  webElem$sendKeysToElement(list(key = "end"))
  # Se deja un tiempo para que cargue
  Sys.sleep(0.5) 
}

# Se regresa al incio del scroll
webElem$sendKeysToElement(list(key = "home"))


# Cantidad de resultados
# Se lista la cantidad de publicaciones para luego iterar sobre esa cantidad
resultados <- remote_driver$findElement(using = "class", value = "resultado_busquedas_listado_oferta_resultado")$getElementText() %>% 
  str_remove_all("[:alpha:]") %>% 
  str_trim() %>% 
  as.numeric()

# Buscamos los anuncio
# Se obtienen las url de todos los anuncios cargados
anuncio <- remote_driver$findElements(using = 'xpath', value = '//div[contains(@class,"col-md-12 resultado_busquedas_listado_oferta d-flex flex-column resultPage_")]')

# Se crea una base de datos vacia a la cual se le agregaran los nuevos trabajos
Trabajos2 <- dplyr::tibble(Empresa          = character(0),
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


# Se comeinza el proceso iterativo para extraer la información de los anuncios

for (i in 1:resultados) {
  # Se selecciona el trabajo i
  anuncio[[i]]$clickElement()  # seleccionamos la pagina web
  Sys.sleep(2)
  
  # Existencia
  # Se comprueba que el anuncio aun este vijente, sucede con trabajos publicados hace mucho tiempo
  # que existe la url de la publicación, pero se indica que este trabajo ya no se encuentra disponible
  Existencia <- remote_driver$findElement("xpath", "//div[contains(@class,'col-md-12 bk_left detalle_oferta h2_sub')]")$getElementText() %>% 
    unlist() %>% str_count("La oferta de empleo que buscas ya no existe")
  if(Existencia>0){next}
  
  #  Se extrae el nombre de la empresa
  repeat{
    Empresa <- remote_driver$findElement(using = "class", value = "nombre_empresa")$getElementText() %>% unlist()
    if(identical(Empresa, character()) == FALSE & Empresa !=''){break}}
  
  # Se extrae la categoria de la empresa
  repeat{
    CategoriaE <- remote_driver$findElement(using = "class", value = "categoria_empresa")$getElementText() %>% unlist()
    if(identical(CategoriaE, character()) == FALSE & CategoriaE !=''){break}}
  
  # Se extrae el titulo del trabajo
  repeat{
    Titulo <- remote_driver$findElement(using = "class", value = "offerTitleText")$getElementText() %>% unlist()
    if(identical(Titulo, character()) == FALSE & Titulo !=''){break}}
  
  # Se extrae la fecha de publicación del trabajo
  repeat{
    Fecha_I <- remote_driver$findElements(using = "class", value = "fecha_oferta")[[1]]$getElementText() %>% unlist()
    if(identical(Fecha_I, character()) == FALSE & Fecha_I !=''){break}}
  fecha_altual <- Fecha_I %>%
    str_remove_all("[:alpha:]") %>% 
    str_remove_all(": ") %>% 
    as.Date("%d/%m/%Y")
  if(fecha_altual <= Ultima_fecha){break}
  
  # Se extrae la fecha termino del trabajo
  repeat{
    Fecha_T <- remote_driver$findElements(using = "class", value = "fecha_oferta")[[2]]$getElementText() %>% unlist()
    if(identical(Fecha_T, character()) == FALSE & Fecha_T !=''){break}}  
  
  # Se extrae la descripción del trabajo
  repeat{
    Descripción <- remote_driver$findElement(using = 'xpath', value = "//div[contains(@itemprop, 'description')]")$getElementText() %>% 
      unlist() %>% 
      str_replace_all("[:punct:]"," ") %>% 
      str_split_fixed("\n",str_count(.,"\n")+1) %>% 
      str_trim() %>% 
      list()
    
    Descripción_C <- Descripción %>% unlist() %>% str_c(collapse = " ")
    
    # Se comprueba que el campo descripción no este vacio
    if(identical(Descripción, character()) == FALSE){break}}   
  
  # Se extrae un listado de información (todos de la misma extracción, luego se separan)
  repeat{
    datos <- remote_driver$findElements(using = 'xpath', value = "//div[contains(@class, 'col-md-8 txt')]")
    
    Tipo_cargo  <- datos[[1]]$getElementText() %>% unlist()
    Vacantes    <- datos[[2]]$getElementText() %>% unlist() %>% as.numeric()
    Region      <- datos[[4]]$getElementText() %>% unlist()
    Comuna      <- datos[[5]]$getElementText() %>% unlist()
    Jornada     <- datos[[10]]$getElementText() %>% unlist()
    
    Experiencia <- datos[[12]]$getElementText() %>% unlist()
    Estudios    <- datos[[13]]$getElementText() %>% unlist()
    Reqiusitos  <- datos[[11]]$getElementText() %>% 
      unlist() %>%
      str_replace_all("[:punct:]"," ") %>% 
      str_split_fixed('\n',str_count(.,"\n")+1) %>% 
      str_trim() %>% 
      list()
    if(identical(Tipo_cargo, character())  == FALSE&
       identical(Vacantes, character())    == FALSE&
       identical(Region, character())      == FALSE&
       identical(Comuna, character())      == FALSE&
       identical(Jornada, character())     == FALSE&
       identical(Experiencia, character()) == FALSE&
       identical(Estudios, character())    == FALSE&
       identical(Reqiusitos, character())  == FALSE){break}}   
  
  # Se guarda la URL
  repeat{
  URL <- remote_driver$findElement(using = "class", value="row")$getCurrentUrl()[[1]]
    if(identical(URL, character()) == FALSE){break}}  
  
  # Se añaden los datos del empleo i a la data previamente creada
  Trabajos2 <-Trabajos2 %>% 
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
            URL               = URL,
            Descripción_C     = Descripción_C)
  print(i)
}

# Se añaden los trabajos nuevos a la data principal
Trabajos <- Trabajos2 %>% add_row(Trabajos)

# Se guarda la nueva base de datos
nombre <- paste0("RTarapaca_selenium.RData",sep = "")
save(Trabajos,file = nombre)

# Se eliminan todos los datos antes de finalizar el proceso
rm(list = ls())
