##############################
## Kjøring på mobilt kontor ##
##############################

# devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


devtools::install_github('Rapporteket/intensiv', ref = 'main_dev')
#remotes::install_github('Rapporteket/intensiv', ref = 'main')
setwd('c://Users/lro2402unn/RegistreGIT/intensiv')


Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)

dum <- intensiv::NIRRegDataSQL(datoFra = '2023-01-01')
rm('RegData')


