##############################
## Kjøring på mobilt kontor ##
##############################

# devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


devtools::install_github('Rapporteket/intensiv', ref = 'main_dev')
#remotes::install_github('Rapporteket/intensiv', ref = 'main')

setwd('../data')
sship::dec('')
# pakkut
# Åpne MySQL Command Line Client - Unicode,
# source c://Users/lro2402unn/RegistreGIT/data/regfil.sql;

setwd('c://Users/lro2402unn/RegistreGIT/intensiv')


Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)

dum <- intensiv::NIRRegDataSQL(datoFra = '2024-01-01')
RegData <- intensiv::NIRPreprosess(RegData = dum)
rm('RegData')


