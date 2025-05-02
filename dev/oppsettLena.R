
# Lena :
Sys.setenv(FALK_EXTENDED_USER_RIGHTS= "[{\"A\":106,\"R\":\"LU\",\"U\":706078},{\"A\":106,\"R\":\"SC\",\"U\":706078},{\"A\":106,\"R\":\"LU\",\"U\":700720},{\"A\":106,\"R\":\"LU\",\"U\":700720}]")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nger/data-raw/config")
Sys.setenv(MYSQL_DB_DATA="intensivregisterreportdatastaging")

Sys.setlocale(locale = 'nb_NO.UTF-8')
devtools::install_github('Rapporteket/intensiv', ref = 'main_dev')
#remotes::install_github('Rapporteket/intensiv', ref = 'main')
setwd('c://Users/lro2402unn/RegistreGIT/intensiv')

source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)

dum <- intensiv::NIRRegDataSQL(datoFra = '2023-01-01')
rm('RegData')

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

