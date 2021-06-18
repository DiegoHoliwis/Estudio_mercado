# Librerias ===================================================================

# Se cargan las librerias correspondientes para el estudio

library(tidyverse)
library(RSelenium)

# Incialización de Rselenium ==================================================

# Se inicializa el Rselenium, en este se especifica
# Browser con el cual se ejecutará
# Versión del browser (busca y ejecuta la ultima versión disponible de Chrome)
# Número de puerto de salida
# Se requiere instalar el chromedriver, link: https://chromedriver.chromium.org/downloads

remote_driver <- RSelenium::rsDriver(browser = "chrome",
                                     port = 4568L,
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

# Forma de ejecución en caso de saber su versión de chrome y chromever

# driver <- RSelenium::rsDriver(browser = "chrome", 
#                               chromever = "89.0.4389.23",
#                               port = 4568L)



# Iniciar navegador ==================================================================================

# Se agrega un segundo de espera, estas solicitudes son necesarias para no ser
# bloqueado por la pagina web

Sys.sleep(1)
# remote_driver$open() # Se abre nuevamente en caso de cerrar la ventana (no se requiere ejecutar selenium nuevamente)
remote_driver$navigate("https://www.trabajando.cl/trabajo-tarapaca")



# Codigo en caso de partir desde el inicio de la pestaña (www.trabajando.cl)

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

# Extracción de la información ========================================================================

# Se solicita ordenar las ofertas laborales por fecha
option <- remote_driver$findElement(using = 'xpath', "//*/option[@value = '3']")
option$clickElement()
Sys.sleep(5)

# Se selecciona el primer anuncio
anuncio <- remote_driver$findElements(using = 'xpath', value = '//div[contains(@class,"col-md-12 resultado_busquedas_listado_oferta d-flex flex-column resultPage_")]')
anuncio[[1]]$clickElement()

# Se extrae la ubicación del scroll Y
webElem <- remote_driver$findElement("css", "body")

# Se mueve la pagina hasta el final 50 veces con el fin de cargar todos los anuncios
for(i in 1:50){
  webElem$sendKeysToElement(list(key = "end"))
  # Se deja un tiempo para que cargue
  Sys.sleep(0.5) 
}

# Se regresa al primer anuncio
webElem$sendKeysToElement(list(key = "home"))


# Cantidad de resultados
resultados <- remote_driver$findElement(using = "class", value = "resultado_busquedas_listado_oferta_resultado")$getElementText() %>% 
  str_remove_all("[:alpha:]") %>% 
  str_trim() %>% 
  as.numeric()

# Se extrae el listado de anuncios
anuncio <- remote_driver$findElements(using = 'xpath', value = '//div[contains(@class,"col-md-12 resultado_busquedas_listado_oferta d-flex flex-column resultPage_")]')

# Se crea una nueva base de datos bacia para llenar con la nueva información
Trabajos <- dplyr::tibble(Empresa           = character(0),
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
                          URL               = character(0))

# Se inicia el proceso iterativo por las ofertas laborales
for (i in 1:resultados) {
  anuncio[[i]]$clickElement()  # selección del empleo i
  Sys.sleep(2)
  
  # Se comprueba que el trabajo exista, puede estar presente la publicación, pero que el trabajo no exista (pasa con las puiblicaciones antiguas)
  Existencia <- remote_driver$findElement("xpath", "//div[contains(@class,'col-md-12 bk_left detalle_oferta h2_sub')]")$getElementText() %>% 
    unlist() %>% str_count("La oferta de empleo que buscas ya no existe")
  if(Existencia>0){next}
  
  # Se extrae el nombre de la empresa
  repeat{
    Empresa <- remote_driver$findElement(using = "class", value = "nombre_empresa")$getElementText() %>% unlist()
    if(identical(Empresa, character()) == FALSE & Empresa !=''){break}}
  
  # Se extrae la categoria de la empresa
  repeat{
    CategoriaE <- remote_driver$findElement(using = "class", value = "categoria_empresa")$getElementText() %>% unlist()
    if(identical(CategoriaE, character()) == FALSE & CategoriaE !=''){break}}
  
  # Se extrae el titulo de la oferta laboral
  repeat{
    Titulo <- remote_driver$findElement(using = "class", value = "offerTitleText")$getElementText() %>% unlist()
    if(identical(Titulo, character()) == FALSE & Titulo !=''){break}}
  
  # Se extrae la fecha de inicio del proceso
  repeat{
    Fecha_I <- remote_driver$findElements(using = "class", value = "fecha_oferta")[[1]]$getElementText() %>% unlist()
    if(identical(Fecha_I, character()) == FALSE & Fecha_I !=''){break}}
  
  # Se extrae la fecha de termino del proceso
  repeat{
    Fecha_T <- remote_driver$findElements(using = "class", value = "fecha_oferta")[[2]]$getElementText() %>% unlist()
    if(identical(Fecha_T, character()) == FALSE & Fecha_T !=''){break}}
  
  # Se extrae la descripción del proceso y se transforma a lista para almacenarlo
  repeat{
    Descripción <- remote_driver$findElement(using = 'xpath', value = "//div[contains(@itemprop, 'description')]")$getElementText() %>% 
      unlist() %>% 
      str_replace_all("[:punct:]"," ") %>% 
      str_split_fixed("\n",str_count(.,"\n")+1) %>% 
      str_trim() %>% 
      list()
    if(identical(Descripción, character()) == FALSE){break}}   
  
  # Se extrae una listado de elementos que van desde tipo de cargo hasta los requisitos de este
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
  
  # Se extrae la URL de la oferta laboral
  repeat{
    URL <- remote_driver$findElement(using = "class", value="row")$getCurrentUrl()[[1]]
    if(identical(URL, character()) == FALSE){break}}  
  
  # Se añade la información del empleo a la base de datos antes creada
  if(identical(Reqiusitos[[1]], character()) == FALSE ) {
    Trabajos  <- Trabajos %>% 
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
              URL               = URL)
    print(i)
  }
}

# Se guarda la base de datos como un formato .RData
save(Trabajos,file = "RTarapaca_Selenium.RData")

