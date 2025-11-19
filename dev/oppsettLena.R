##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
# devtools::install(upgrade = FALSE)


devtools::install_github('Rapporteket/intensiv', ref = 'main_dev', )
remotes::install_github('Rapporteket/rapbase', ref = 'forenkl_take2')

setwd('../data')
sship::dec('intensiv1590fb768.sql.gz__20251017_104707.tar.gz',
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")
setwd('c://Users/lro2402unn/RegistreGIT/intensiv')


source("dev/sysSetenv.R")
intensiv::kjorIntensivApp(browser = TRUE)

library(intensiv)
reshID <- 706078 # 106271
dum <- intensiv::NIRRegDataSQL(datoFra = '2025-01-01')
RegData <- intensiv::NIRPreprosess(RegData = dum)
RegData <- RegData[RegData$ReshId==reshID,]

test <- tabBelegg(RegData=RegData, tidsenhet='Mnd', enhetsUtvalg=2, datoTil=Sys.Date(), reshID = 3)

knitr::knit2pdf('./inst/NIRluftveisinfek.Rnw')

test <- NIRUtvalgEnh(RegData = RegData, luftvei = 3)

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




