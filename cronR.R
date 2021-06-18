library(cronR)

cmd <- cron_rscript(rscript = "/home/diego/Proyectos/Análisis de mercado/Mercado laboral diario.R")
cron_add(command = cmd, frequency = '0 */4 * * *', id = 'test1', description = 'Extracción de ofertas de trabajo', at = '21PM')

# cron_njobs()
# cron_ls()
# cron_clear(ask=FALSE)

