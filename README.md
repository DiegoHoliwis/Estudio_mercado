# Extracción de ofertas laborales

<p align="justify"> 
Se presentan 3 script los cuales permiten realizar la extracción de información de las ofertas laborales de la región de Tarapacá, por medio de la plataforma
<A HREF="https://www.trabajando.cl/">Trabajando.cl</A>. Para ello se deben ejecutar los script en el siguiente orden:
</p>

## Mercado laboral Selenium - Extracción primera vez

<p align="justify"> 
Este script le permite realizar una barrida completa de la información de las ofertas laborales de Trabajando.cl, en particular, para la región de Tarapacá, fácilmente modificable para otras regiones. Este webscrapig, se basa en una estructura de JavaScript, ya que se ve con la necesidad de interactuar con la pagina web, haciendose pasar por un humano y extrayendo a la vez la información.
</p>

## Mercado laboral Selenium - Extracción diaria (opción 1)

<p align="justify"> 
Este script le permite complementar la base de datos obtenida por el script anterior, ya que cargará el .Rdata generado y añadirá todas las ofertas laborales que no existan en la base de datos. Esta opción al ser basada en Selenium, es requerido de un browser para poder interacturar con la pagina web, de otro modo genererará errores.
</p>

## Mercado laboral rvest - Extracción diaria (opción 2)

<p align="justify"> 
Este script al igual que el anterior le permite complementar la base de datos generada por la primera extracción, pero al estar basada en un sistema sin interacciones de JavaScript, solo podrá extraer la información de las ultimas 20 ofertas laborales, por lo cual, solo es de utilidad si se utiliza de forma periodica. Las ventajas es que al no estar basado en Selenium, no se requiere un browser para funcionar, por lo cual su implementación en sistemas cloud es más fácil.
</p>
