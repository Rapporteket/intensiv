##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install_github('Rapporteket/intensiv', ref = 'main_dev', )
remotes::install_github('Rapporteket/rapbase', ref = 'forenkl_take2')

setwd('../data')
sship::dec('intensiv12ac10e59.sql.gz__20260325_083639.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")
# source c://Users/lro2402unn/RegistreGIT/data/intensiv12ac10e59.sql;
setwd('c://Users/lro2402unn/RegistreGIT/intensiv')

source("dev/sysSetenv.R")
source("C:/Users/lro2402unn/RegistreGIT/intensiv/dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)

library(intensiv)
reshID <- 102026 #705577 #103948 #4205969 Med PREM: 102026

RegData <- intensiv::NIRRegDataSQL(datoFra = '2025-01-01')
RegData <- intensiv::NIRPreprosess(RegData = RegData)

    NIRFigAndeler(RegData = RegData, preprosess = 0, valgtVar = 'trakeostomi'
                  ,outfile = 'trakeostomiFordeling.pdf')
    NIRFigAndeler(RegData = RegData, preprosess = 0, valgtVar = 'komplikasjoner',
              outfile = 'komplFordeling.pdf')
test <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet='Halvaar')
test$tidtxt

# Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
#                 sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),


NIRFigAndelerGrVar(RegData = RegData, preprosess = 0, valgtVar = 'komplReg'
                   ,outfile = 'kompl_perEnhet.pdf')
NIRFigAndelTid(RegData = RegData, preprosess = 0, valgtVar = 'komplReg',
               tidsenhet = 'Halvaar', outfile = 'kompl_perHalvaar.pdf')

RegData <- RegData[RegData$ReshId==reshID,]

dataMRS <-   readxl::read_excel('../data/NIRdata_2025-11-25_1035.xlsx')
skjemaMangler <- setdiff(dataMRS$SkjemaGUID, tolower(dum$SkjemaGUID))
dataMangler <- dataMRS[which(skjemaMangler %in% dataMRS$SkjemaGUID), ]

knitr::knit2pdf('./inst/NIRluftveisinfek.Rnw')

  test <- NIRUtvalgEnh(RegData = RegData, luftvei = 3)


skjemaidMRS <- as.data.frame(readxl::read_excel('skjema_2025-11-24_0706.xlsx'))
avvik <- setdiff(sort(skjemaidMRS$SkjemaGUID), tolower(sort(dum$SkjemaGUID)))
write.csv2(avvik, file = 'ManglendeSkjemaID.csv', row.names = F)

class(dum$SkjemaGUID)




tabBelegg <- as.matrix('N<3')
xtable(tabBelegg, digits=0, align=c('l', rep('r', ncol(tabBelegg))),
       caption=paste0('Tal på opphald og liggedøger'), label='tab:RegEget')

test <- c('Alle',
  unique(RegData$RHF),
  unique(RegData$HF),
  unique(RegData$ShNavn))

#Sys.setenv(MRS_ACCESS_HIERARCHY_URL="https://app.mrs.qa.nhn.no/intensivregisterservices/AccessHiearchyReport")
# Sys.setenv(MRS_ACCESS_HIERARCHY_URL="https://app.mrs.nhn.no/intensivregisterservices/AccessHiearchyReport")
 TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
 Tilgangstre <- jsonlite::fromJSON(TilgJsn)$AccessUnits




