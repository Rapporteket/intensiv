##############################
## Kjøring på mobilt kontor ##
##############################

# devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


devtools::install_github('Rapporteket/intensiv', ref = 'main_dev')
#remotes::install_github('Rapporteket/intensiv', ref = 'main')

setwd('../data')
sship::dec('')

setwd('c://Users/lro2402unn/RegistreGIT/intensiv')

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)


#Sys.setenv(MRS_ACCESS_HIERARCHY_URL="https://app.mrs.qa.nhn.no/intensivregisterservices/AccessHiearchyReport")
Sys.setenv(MRS_ACCESS_HIERARCHY_URL="https://app.mrs.nhn.no/intensivregisterservices/AccessHiearchyReport")
TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
Tilgangstre <- jsonlite::fromJSON(TilgJsn)$AccessUnits
grep('(1)', Tilgangstre$Title)
strsplit(Tilgangstre$Title, aplit= ' (' )
varTilg <- c("UnitId", "ParentUnitId", "HasDatabase", "ExternalId", "Title", "TitleWithPath","ExtraData")

RegDataRaa <- intensiv::NIRRegDataSQL(datoFra = '2000-01-01')
RegData <- NIRPreprosess(RegData = RegDataRaa)
# reshTilg <- sort(Tilgangstre$UnitId[Tilgangstre$HasDatabase==TRUE])
# reshAlle <- sort(unique(RegDataRaa$ReshId))
# resh <- intersect(reshTilg, reshAlle) # 100082 100085 100170 - fjernes/flyttes? 114

RegData <- merge(RegDataRaa, Tilgangstre[ ,varTilg],
                 by.x = 'ReshId', by.y = 'UnitId', suffixes = c('Int','Tilg'))
RegData <- dplyr::rename(RegData,
              Nivaa = ExtraData,
              ReshIdReg = ReshId,
              ReshId = ExternalId,
              ShNavnReg = ShNavn,
              ShNavn = Title) #newname = oldname
RegData <- NIRPreprosess(RegData = RegData)
#700720?
# Tromsø Intensiv (3) (700720)	0	0	238	15	456	709
# Tromsø Kir. int. (3) (700720)	459	446	218	0	2	1125

dum <- intensiv::NIRRegDataSQL(datoFra = '2024-01-01')
RegData <- intensiv::NIRPreprosess(RegData = dum)
rm('RegData')


