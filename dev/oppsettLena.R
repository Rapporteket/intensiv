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

TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
Tilgangstre <- jsonlite::fromJSON(test)$AccessUnits
varTilg <- c("UnitId", "ParentUnitId", "HasDatabase", "ExternalId", "Title", "TitleWithPath","ExtraData")

RegDataRaa <- NIRRegDataSQL()
RegData <- merge(RegDataRaa, Tilgangstre[ ,varTilg],
                 by.x = 'ReshId', by.y = 'UnitId', suffixes = c('Int','Tilg'))
RegData$ExtraData <- RegData$NivaaTxt




dum <- intensiv::NIRRegDataSQL(datoFra = '2024-01-01')
RegData <- intensiv::NIRPreprosess(RegData = dum)
rm('RegData')


