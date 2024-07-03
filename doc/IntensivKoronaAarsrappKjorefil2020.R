#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

rm(list=ls())
library(intensiv)
datoFra <- '2020-01-01'
datoTil <- '2020-12-31'	#
datoFra1aar <- '2020-01-01'
#------Klargjøre årsrapportfil-------------
RegData <- read.table('A:/Intensiv/NIRAarsrapp2019_2020-09-09.csv', sep=';', stringsAsFactors=FALSE,
                      header=T, encoding = 'UTF-8')
 indDato <- intersect(which(as.Date(RegData$DateAdmittedIntensive) >= as.Date(datoFra, tz= 'UTC')),
                         which(as.Date(RegData$DateAdmittedIntensive) <= as.Date(datoTil, tz= 'UTC')))
RegData <- RegData[indDato,]
save(RegData, file = 'A:/Intensiv/NIRaarsrapp2019.RData')
RegDataAarCSV <- NIRPreprosess(RegData)
write.table(RegDataAarCSV, file = 'A:/Intensiv/NIRaarsrapp2019.csv', row.names = F, col.names=T,
            fileEncoding = 'UTF-8', sep = ';')
#-----------
setwd('/home/rstudio/intensiv/aarsrappOff/CovidPas')

IntData <- NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil)
#Covid-skjema:
qCovid <- paste0('SELECT SkjemaGUID, HovedskjemaGUID, FormStatus, Diagnosis
                  FROM ReadinessFormDataContract')
CovidData <- rapbase::loadRegData(registryName= "nir", query=qCovid, dbType="mysql")

CovidData$HovedskjemaGUID <- toupper(CovidData$HovedskjemaGUID)
CovidData$Bekreftet <- 0
CovidData$Bekreftet[which(CovidData$Diagnosis %in% 100:103)] <- 1
test <- table(CovidData$HovedskjemaGUID)
skjemaDBL <- names(test)[which(test>1)]
ind <- which(CovidData$HovedskjemaGUID %in% skjemaDBL)
CovidData <- CovidData[-ind[3:4], ]

RegData <- merge(IntData, CovidData[ ,-which(names(CovidData) == 'Diagnosis')], suffixes = c('','Cov'),
                 by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)


#Innkomstmåte (egen fig.) reg/sentLok
NIRFigInnMaate(RegData=RegData, datoFra=datoFra1aar, datoTil = datoTil,
               grType=1,velgDiag = 1, outfile='Cov_InnMaateLokSen.pdf')
NIRFigInnMaate(RegData=RegData, valgtVar='InnMaate', datoFra=datoFra1aar, datoTil = datoTil,
               grType=3,velgDiag = 1, outfile='Cov_InnMaateReg.pdf')

#--------------------------------------- Fordelinger ----------------------------------

#NIRFigAndeler(RegData=NIRRegDataSQL(), valgtVar='komplikasjoner', enhetsUtvalg = 0)
variable <- c('OrganDonationCompletedReasonForNoStatus', 'CerebralCirculationAbolishedReasonForNo',
              'frailtyIndex', 'inklKrit','liggetid','InnMaate','komplikasjoner',
              'NEMS24', 'Nas24','respiratortidNonInv',
                   'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak') #, 'respiratortidInvMoverf')
for (valgtVar in variable) {
   outfile <- paste0('Cov_', valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                 outfile=outfile,velgDiag = 1)
}


NIRFigAndeler(RegData=RegData, valgtVar='liggetid', dodInt=1, datoFra=datoFra1aar, datoTil=datoTil,
              velgDiag = 1, outfile='Cov_liggetidDod_ford.pdf')
NIRFigAndeler(RegData=RegData, valgtVar='spesTiltak', datoFra=datoFra1aar, datoTil=datoTil, grType = 3,
              velgDiag = 1, outfile='Cov_spesTiltak_ford.pdf')
#--------------------------------------- AndelGrVar ----------------------------------
# Reinnleggelser reg/sentlok
# Død innen 30 dager reg/sentLok
# Trakeostomi reg/lokSent


variable <- c('OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
              'dod30d', 'dodeIntensiv', 'trakeostomi','reinn', 'komplReg', 'invasivVent')
variable <- 'frailtyIndex'
for (grType in 2:3) {
      for (valgtVar in variable) {
            outfile <- paste0('Cov_',valgtVar, grType, 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, Ngrense=10,
                               velgDiag = 1, datoTil=datoTil, grType=grType, outfile=outfile)
      }
}

#---------------------AndelTid----------------------------------------------

variable <- c('dod30d', 'invasivVent', 'liggetidDod')

for (valgtVar in variable){
      outfile <- paste0('Cov_',valgtVar, '_AndelTid.pdf')
      NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                     tidsenhet = 'Kvartal',
                     velgDiag = 1, outfile=outfile)
}

#---------------------GjsnTid----------------------------------------------
# Alder, hele landet
# Liggetid, hele landet
# Liggetid, død
# SAPSII

valgtMaal <- 'Med'
variable <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')


for (valgtVar in variable) {
      outfile <- paste0('Cov_', valgtVar, 'MedTid.pdf')
      NIRFigGjsnTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                    velgDiag = 1, valgtMaal=valgtMaal, outfile=outfile)
}
NIRFigGjsnTid(RegData=RegData, valgtVar='liggetid', datoFra=datoFra, datoTil=datoTil,
              velgDiag = 1, valgtMaal=valgtMaal, dodInt=1, outfile='Cov_liggetidDod_MedTid.pdf')

#--------------------------------------- SENTRALMÅL per enhet----------------------------------
# Alder reg/lokSen
# Liggetid reg/lokSen
# Liggetid reg/lokSen, døde
# NEMS per opphold reg/Lok/sent
# NEMS/døgn reg/lokSent
# Nas per døgn, alle
# Ventilasjonstid, åpen maske reg/lokSen
# Invasiv ventilasjon (inkl. overf.) reg/lokSen
# SAPSII, reg/lokSent


valgtMaal <- 'Med'
#variable <- 'respiratortidNonInv'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas24'
#Nye: respiratortidInvMoverf, respiratortidInvUoverf, respiratortidNonInv
variable <- c('alder', 'liggetid', 'respiratortid','NEMS', 'NEMS24', 'Nas24',
              'respiratortidInvMoverf', 'respiratortidNonInv', 'SAPSII',
              'respiratortidInvUoverf')
for (grType in 2:3) {
      for (valgtVar in variable){ #
            outfile <- paste0('Cov_',valgtVar,grType, '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal,
                            velgDiag = 1, grType=grType, datoFra=datoFra1aar, datoTil=datoTil, outfile=outfile)
      }
      NIRFigGjsnGrVar(RegData=RegData, valgtVar='SMR', grType=grType,
                      velgDiag = 1, datoFra=datoFra1aar, datoTil=datoTil, outfile=paste0('Cov_',valgtVar,grType, '_PrSh.pdf'))
      NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1,
                      velgDiag = 1, grType=grType, datoFra=datoFra1aar, datoTil=datoTil,
                      outfile=paste0('Cov_','liggetidDod',grType,'_MedPerSh.pdf'))
}
NIRFigGjsnGrVar(RegData=RegData, valgtVar='SMR',velgDiag = 1,
                datoFra=datoFra1aar, datoTil=datoTil, outfile='Cov_SMR_PrSh.pdf')

NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1,velgDiag = 1,
                grType=3, datoFra=datoFra1aar, datoTil=datoTil, outfile='Cov_liggetidDod3_MedPerSh.pdf')

# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar. Figurer per sykehus
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Med',velgDiag = 1,
                datoFra=datoFra1aar, datoTil=datoTil, outfile='Cov_respiratortidInvMoverf_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Gjsn',velgDiag = 1,
                datoFra=datoFra1aar, datoTil=datoTil, outfile='Cov_respiratortidInvMoverf_GjsnPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidNonInv', valgtMaal='Med',velgDiag = 1,
                datoFra=datoFra1aar, datoTil=datoTil, outfile='Cov_respiratortidNonInv_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',velgDiag = 1,
                datoFra=datoFra1aar, datoTil=datoTil, outfile='Cov_respiratortidNonInv_GjsnPrSh.pdf')

#-------------------------Tabeller----------------------
#Nøkkeltall
#Risikofaktorer
library(xtable)
library(intensivberedskap)
BeredIntDataRaa <- NIRberedskDataSQL(datoFra = '2020-01-01', datoTil = '2020-12-31', kobleInt = 1)
RegData <- BeredIntDataRaa
BeredIntData <- NIRPreprosessBeredsk(RegData = BeredIntDataRaa, kobleInt = 1, aggPers = 0)

BeredDataRaa <- NIRberedskDataSQL(datoFra = '2020-01-01', datoTil = '2020-12-31', kobleInt = 0)
BeredData <- NIRPreprosessBeredsk(RegData = BeredDataRaa, kobleInt = 0, aggPers = 0)
#RegData <- BeredDataRaa

#Hvor mange mangler intensivskjema?
manglerInt <- ManglerIntSkjema(datoTil = '2020-12-31')


tab <- oppsumFerdigeRegTab(RegData=BeredData)$Tab
xtable(tab, align=c('l', 'r', 'r', 'c', 'r', 'r'))

#Nøkkeltall
#Evt. legg til andel som får nyreerstattende behandling og tid med nyreerstattende behandling.
#Skill på intermitterende og kontinuerlig
RegData <- BeredIntData

N <- dim(RegData)[1]
##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
AntBruktECMO <- sum(RegData$ECMOTid>0, na.rm=T)
Liggetid <- summary(RegData$Liggetid, na.rm = T)
RespTid <- summary(RegData$RespTid, na.rm = T)
ECMOtid <- summary(RegData$ECMOTid, na.rm = T)
Alder <- summary(RegData$Alder, na.rm = T)
AntDod <- sum(RegData$DischargedIntensiveStatus==1, na.rm=T)

NyreKont <- summary(RegData$KontinuerligDays, na.rm = T)
AntKont <- sum(RegData$KontinuerligDays>0, na.rm = T)
NyreInter <- summary(RegData$IntermitterendeDays, na.rm = T)
AntInter <- sum(RegData$IntermitterendeDays>0, na.rm = T)


med_IQR <- function(x){
  c(sprintf('%.1f',x[4]), sprintf('%.1f',x[3]), paste(sprintf('%.1f',x[2]), sprintf('%.1f',x[5]), sep=' - '))
}
TabFerdigeReg <- rbind(
  'ECMO-tid (døgn)' = c(med_IQR(ECMOtid), AntBruktECMO*(c(1, 100/N))),
  'Respiratortid (døgn)' = c(med_IQR(RespTid), AntBruktResp*(c(1, 100/N))),
  'Nyreerstattenede beh., kont.' = c(med_IQR(NyreKont), AntKont*(c(1, 100/N))),
  'Nyreerstattenede beh., int.' = c(med_IQR(NyreInter), AntInter*(c(1, 100/N))),
  'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
  'Alder (år)' = c(med_IQR(Alder), N, ''),
  'Døde' = c('','','',AntDod, paste0(sprintf('%.f',100*AntDod/N),'%'))
)
#TabFerdigeReg[TabFerdigeReg==NA]<-""
colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall opphold', 'Andel opphold')
TabFerdigeReg[c(1:4),'Andel opphold'] <-
  paste0(sprintf('%.1f', as.numeric(TabFerdigeReg[c(1:4),'Andel opphold'])),'%')
xtable::xtable(TabFerdigeReg,
               digits=0,
               align = c('l','r','r','c', 'r','r'),
               caption='IQR (Inter quartile range) - 50% av pasientene er i dette intervallet.')

length(unique(RegData$PasientID))
#------Risikofaktorer
tabRisiko <- RisikofaktorerTab(RegData = BeredData)
xtable(tabRisiko$Tab, align=c('l', 'r', 'r'))
tabRisiko$utvalgTxt
AntPas <- length(unique(BeredData$PasientID))
