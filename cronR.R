library(cronR)

cmd <- cron_rscript(rscript = "/home/diego/Proyectos/Estudio mercado/Mercado laboral rverst - Extraccion diaria.R")
cron_add(command = cmd,
         frequency = '0 0,7,10,15 * * *',
         id = 'test1',
         description = 'Extracción de ofertas de trabajo')

# cron_njobs()
# cron_ls()
# cron_clear(ask=FALSE)

