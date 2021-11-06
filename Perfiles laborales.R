library(tidyverse)
load('RTarapaca.RData')


Trabajos %>% 
  group_by(CategoriaE) %>% 
  count() %>% 
  openxlsx::write.xlsx(file = "Categorias.xlsx")


c('Textil', 'Pesquera / Cultivos Marinos', 'Consultoria / Asesoría', 'Consumo
masivo', 'Agroindustria', 'Ambiental', 'Automotriz', 'Logística /
Distribución', 'Alimentos', 'Minería')

# XXXX
c('Petroleo / Gas / Combustibles','Combustibles (Gas / Petróleo)','Agua / Obras
Sanitarias',)

# Tecnología
c('Informática / Tecnología', 'IngenierÃ­a','Telecomunicaciones',
  'Ingeniería','Tecnologías de Información','Electrónica de Consumo',)

# construcción
c('Energía / Electricidad / Electrónica','Maquinaria y Equipo','Construcción',
  'Industrial','Cemento y Materiales',)

# Comercio
c('Comercio Minorista','Ventas','Grandes Tiendas','Comercial','Retail','Comercio Mayorista','Hipermercados', )

# Administración
c('Poder ejecutivo y administración pública','Administración Pública','Organizaciones sin Fines de
Lucro','Recursos Humanos',)

# Financiera
c('Banca / Financiera','Servicios Financieros Varios','Inversiones (Soc / Cías / Holding)',
  'Inmobiliaria/Propiedades',  'Seguros / Previsión',)

# Otros
c('Seguridad','Otra Actividad','Servicios funerarios','Servicios Varios',
  'Educación / Capacitación','Transporte',)

# Quimica y salud
c('Química','Servicios de Salud','Medicina / Salud','Farmacéutica',)

# XXXX
c('Aeronaves / Astilleros', 'Naviera',)


