#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

# Sys.setenv(R_RAP_INSTANCE="QAC")
# Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nger/data-raw/config")
# Sys.setenv(MYSQL_DB_LOG="db_log")
# Sys.setenv(MYSQL_DB_AUTOREPORT="db_autoreport")
# Sys.setenv(R_RAP_INSTANCE="QAC")
# Sys.setenv(R_RAP_CONFIG_PATH="data-raw/config") # "/home/rstudio/nakke/data-raw/config")
# Sys.setenv(SHINYPROXY_USERNAME="ttsk")
# Sys.setenv(SHINYPROXY_USERGROUPS="pilot")
# Sys.setenv(USERORGID="pilot")

Sys.setenv(MYSQL_HOST="localhost") # "db")
Sys.setenv(MYSQL_USER="root")
Sys.setenv(MYSQL_PASSWORD="root")
Sys.setenv(MYSQL_DB_DATA="intensivregisterreportdatastaging")
Sys.setlocale(locale = 'nb_NO.UTF-8')

test <- NIRRegDataSQL(datoFra = '2025-01-01')


rm(list=ls())
library(intensiv)
aarsrappAar <- 2024
datoFra <- '2011-01-01'
datoTil <- paste0(aarsrappAar, '-12-31')
datoFra1aar <- paste0(aarsrappAar, '-01-01')
RegDataRaa <- NIRPreprosess(NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil))
#Registrert på feil resh:
RegDataRaa$ReshId[RegDataRaa$ReshId == 100132] <- 102026

#Tabell med navn og nivåer
EnhNivaa <- read.csv2(file = 'C:/Users/lro2402unn/RegistreGIT/intensiv/data/AlleReshShNavnKategori.csv',
                      encoding = 'latin1') #, sep = ';', row.names = FALSE)
#unique(EnhNivaa[,c('Nivaa', 'NivaaTxt')])
#ekstraResh <- setdiff( sort(unique(EnhNivaa$ReshId)), sort(unique(RegDataRaa$ReshId)))

RegData <- merge(RegDataRaa, EnhNivaa[,-which(names(EnhNivaa)=='ShNavnInt')], by.x = 'ReshId', by.y = 'ReshId', suffixes = c('Int',''))
#table(RegData$ShNavn, RegData$Aar)

#Fjerner Helse Bergen HF og RH samlet - kodet med ShNavn "Fjernes":
RegData <- RegData[-which(RegData$ShNavn == 'Fjernes'), ]

RegData$EnhNivaa <- RegData$NivaaTxt
RegData1aar <- NIRUtvalgEnh(RegData = RegData, datoFra = datoFra1aar)$RegData

setwd('../Aarsrapp/Intensiv')

# ------------------------- FIGURER UTEN inndeling I enhetsNIVÅ----------------------------------
#--------------------------------------- Fordelinger
variabler <- c('OrganDonationCompletedReasonForNoStatus',
               'CerebralCirculationAbolishedReasonForNo',
               'frailtyIndex', 'inklKrit','liggetid','InnMaate',
              'NEMS24', 'Nas24', 'regForsinkelse', 'respiratortidNonInv',
              'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak')

for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                 outfile=outfile)
}


#------------ Andelsh
NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='OrganDonationCompletedCirc',
                   Ngrense=10, outfile='OrganDonationCompletedCircPrSh.pdf')

#---------------------GjsnTid
variabler <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')

for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, 'MedTid.pdf')
  NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                valgtMaal='Med', tidsenhet= 'Aar', outfile=outfile)
}

#KvalInd:
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='SMR'
                ,outfile='SMR_PrSh.pdf')

#KvalInd:
#årsrapp 22: Etter litt fram og tilbake endte vi på 'respiratortidInvUoverf'.
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidInvUoverf', valgtMaal='Med',
                outfile='respiratortidInvUoverf_MedPrSh.pdf')


# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar.
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Med',
                outfile='respiratortidNonInv_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                outfile='respiratortidNonInv_GjsnPrSh.pdf')


# ------------------------- FIGURER som skiller på enhetsNIVÅ----------------------------------
nivaa <- 1:6
nivaaKort <- c('1a', '1b', '2b', '3', '3b', '3c')
nivaaTxt <- c('Overvåk', 'Postop', 'Gen <50','Gen >50', 'Spesial',  'Barn')

#------------Fordelingsfigurer
variabler <- c('komplikasjoner', 'frailtyIndex')
for (nivaa in 1:6) {
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, '_',nivaaKort[nivaa], 'Ford.pdf')
    NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                  Ngrense=10, nivaa=nivaa, outfile=outfile)
  }
}

#-------------- AndelGrVar
# #Organdonorer av døde: OrganDonationCompletedStatus
# #Organdonorer, av alle med opphevet intrakran. sirk.': 'OrganDonationCompletedCirc',
variabler <- c('dod30d', 'frailtyIndex', 'komplReg',
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi')

for (nivaa in 1:6) {
      for (valgtVar in variabler) {
            outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                               Ngrense=10, nivaa=nivaa, outfile=outfile)
      }
}

#------------ SENTRALMÅL per enhet

variabler <- c('alder', 'NEMS24', 'Nas24',
              'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
for (nivaa in 1:6) {
      for (valgtVar in variabler){ #
            outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                            nivaa=nivaa, outfile=outfile)
      }
}

#Nye, 2024
#   •	‘EcmoEcla’ – andel TRUE per enhet. En figur for hver av kategoriene Gen.int >50, postop
# •	‘EcmoEclaDager’ [EcmoEcla=TRUE] – median per enhet. En figur for hver av kategoriene Gen.int >50, postop
# •	‘Iabp’– andel TRUE per enhet. En figur for hver av kategoriene Gen.int.<50 og Gen.int >50
# •	‘Impella’– andel TRUE per enhet. En figur for hver av kategoriene Gen.int.<50 og Gen.int >50.


for (nivaa in c(2,4)) {
    (outfile <-  paste0('EcmoEclaDager', '_',nivaaKort[nivaa], '_MedPrSh.pdf'))
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='EcmoEclaDager', valgtMaal='Med',
                    nivaa=nivaa, outfile=outfile)
  }

  for (valgtVar in c('EcmoEcla', 'Iabp', 'Impella')) {
    outfile <-  paste0(valgtVar, '_',3, 'PrSh.pdf') #Gen.int. >50
    NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                       Ngrense=10, nivaa=4, outfile=outfile)
  }

  for (valgtVar in c('Iabp', 'Impella')) {
    outfile <-  paste0(valgtVar, '_','2b', 'PrSh.pdf')
    NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                       Ngrense=10, nivaa=3, outfile=outfile)
  }

#tidstrender på alder, liggetid, invasiv respiratortid, og saps fordelt på kategoriene

variabler <- c('alder', 'NEMS24', 'Nas24',
               'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
for (nivaa in 1:6) {
  for (valgtVar in variabler){
    outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], '_MedTid.pdf')
    NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                  tidsenhet= 'Aar', nivaa=nivaa, outfile=outfile)
  }
}


#--------------Tilleggsbestilling, 2023, overordnede grupper (enhetskategorier) ------------------
# Jeg har lagt til en ‘label’ på hver enhet, som her har fått navnet ‘Niva’. Med verdi 1-3.
# Ønsket er at du kjører følgende figurer ut fra disse «kategoriene», altså nivåinndelingene.
# 1a, 1b, 2a, 2b, 3


# - Median Invasiv ventilasjonsbehandling
# - Median non-invasiv ventilasjonsbehandling
# - Median liggetid
# - Median SAPS
# - Median NEMS

#RegData$ShNavn <- GruppeDef$niva[match(RegData$ReshId, GruppeDef$resh_id)]

RegData1aar$ShNavn <- RegData1aar$NivaaTxt

# 1.runde: Skill på overførte og ikke overførte.
variabler <- c( 'liggetid','NEMS','respiratortidInv','respiratortidNonInv','SAPSII',  'SMR')
for (valgtVar in variabler){ #
  for (overf in 1:2) {
    overfTxt <- c('Uoverf','Overf')[overf]
    outfile <- paste0(valgtVar, overfTxt, '_MedNivaa.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    overfPas = overf, outfile=outfile)
  }
}


# 2. runde:

variabler <- c('dod30d', 'frailtyIndex', 'komplReg', #'komplikasjoner',
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi')
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, '_PrNivaa.pdf')
    NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                       Ngrense=10, outfile=outfile)
  }

variabler <- c('alder', 'frailtyIndex', 'NEMS24', 'Nas24',
               'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
  for (valgtVar in variabler){ # variabler <- 'frailtyIndex'
    outfile <- paste0(valgtVar, '_MedPrNivaa.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    outfile=outfile)
  }

#KvalInd:
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='SMR'
                ,outfile='SMR_PrNivaa.pdf')

NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidInvUoverf', valgtMaal='Med',
                outfile='respiratortidInvUoverf_MedPrNivaa.pdf')


# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar.
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                outfile='respiratortidNonInv_GjsnPrNivaa.pdf')


#-------------------------------Tabeller--------------------------------
#Belegg
library(intensiv)
library(xtable)

#Antall opphold
library(lubridate)
xtable::xtable(table(RegData1aar$ShNavn), align=c('l','r'), #row.names=F,
               caption = paste0('Intensivopphald per eining i ', aarsrappAar, '.'))



#Fordeling av kjønn per sykehustype og år
tabShTypeAar <- table(RegData$Aar, RegData$EnhNivaa)
tabKj <- table(RegData[RegData$erMann==1 , c('Aar', 'NivaaTxt')])
kjLandet <- prop.table(table(RegData[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
AndelMenn <- rbind(AndelMenn,
                   'Alle år' = 100*c(prop.table(table(RegData[ , c("erMann",'ShType')]),2)[2,],
                                     prop.table(table(RegData[ , "erMann"]))[2])
)
colnames(AndelMenn)[7] <- 'Hele landet'
xtable::xtable(AndelMenn, digits=1, align=c('l', rep('r', ncol(AndelMenn))),
       caption='Del (prosent) av intensivopphald som er menn.', label='tab:KjonnAar')


#Belegg
tabBeleggN <- rbind(
  'Ferdigstilte intensivopphald' = tapply(RegData1aar$PasientID, RegData1aar$ShNavn, FUN=length),
  'Registrerte pasientar' = tapply(RegData1aar$PasientID, RegData1aar$ShNavn,
                                   FUN=function(x) length(unique(x))),
  'Tal intensivdøger' = round(as.numeric(tapply(RegData1aar$liggetid, RegData1aar$ShNavn, sum, na.rm=T)),0),
  'Respiratordøger, \ntotalt' = tapply(RegData$respiratortid[indRespt], RegData$ShNavn[indRespt],
                                       FUN=sum, na.rm=T),
  'Gjennomsnittleg liggjetid' = round(tapply(RegData1aar$liggetid, RegData1aar$ShNavn, mean, na.rm=T),1)
)
tabBeleggNtot <- cbind(tabBeleggN,
                       c(rowSums(tabBeleggN)[1:4],
                         round(mean(RegData1aar$liggetid, na.rm=T),1)))
colnames(tabBeleggNtot)[7] <- 'Hele landet'

xtable::xtable(tabBeleggNtot, digits=0, align=c('l', rep('r', ncol(tabBeleggNtot))),
               caption= paste0('Antal opphald og liggedøger i ',aarsrappAar, '.'), label='tab:RegKat')



#tabNokkeltall <- tabNokkeltall(RegData=RegData1aar)
RegData <- RegData1aar
indFrail <- which(RegData$FrailtyIndex %in% 1:9)
indLigget <- which(RegData$liggetid>0)
indNyre <- which(RegData$KidneyReplacingTreatment ==1)
indRespt <- which(RegData$respiratortid>0)
indRespInv <- which(RegData$InvasivVentilation >0)
indRespNIV <- which(RegData$NonInvasivVentilation>0)
indSAPS <- which(RegData$SAPSII > 0 & RegData$Alder > 16)
RegData$SAPSuAld <- ifelse(RegData$Age >-1, RegData$SAPSII-RegData$Age, RegData$SAPSII)
indVaso <- which(RegData$VasoactiveInfusion %in% 1:2)
indNEMS <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))
indTrak <- which(RegData$Trakeostomi %in% 1:3)
RegDataReinn <- NIRVarTilrettelegg(RegData=RegData, valgtVar = 'reinn', figurtype = 'andelGrVar')$RegData
ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
RegData$Ut1708 <- 0
RegData$Ut1708[ind1708]<-1

tabNokkeltall <- rbind(
  'Antall opphold' = tapply(RegData$PasientID, RegData$EnhNivaa, FUN=length),
  'Antall pasienter' = tapply(RegData$PasientID, RegData$EnhNivaa,
                              FUN=function(x) length(unique(x))),
  'Pasienter >80 år' = tapply(RegData$Alder > 80, RegData$EnhNivaa,
                            FUN=function(x) round(sum(x, na.rm=T)/length(x)*100, 1)),
  'Liggedøgn (median)' = tapply(RegData$liggetid[indLigget], RegData$EnhNivaa[indLigget], FUN=median, na.rm=T),
  'Mekanisk ventilasjonsstøtte (%)' = tapply(RegData$respiratortid>0, RegData$EnhNivaa,
                                               FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),

  'Respiratordøgn, samlet (median)' = tapply(RegData$respiratortid[indRespt], RegData$EnhNivaa[indRespt],
                                               FUN=median, na.rm=T),
  'Respiratordøgn, invasiv (median)' = tapply(RegData$InvasivVentilation[indRespInv], RegData$EnhNivaa[indRespInv],
                                                FUN=median, na.rm=T),
  'Respiratordøgn, non-invasiv (median)' = tapply(RegData$NonInvasivVentilation[indRespNIV], RegData$EnhNivaa[indRespNIV],
                                                    FUN=median, na.rm=T),
  'SAPSII (median)' = tapply(RegData$SAPSII[indSAPS], RegData$EnhNivaa[indSAPS], FUN=median, na.rm=T),
  'SAPSII u/alder (median)' = tapply(RegData$SAPSuAld[indSAPS], RegData$EnhNivaa[indSAPS], FUN=median, na.rm=T),
  'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                           RegData$EnhNivaa[indNEMS], FUN=sum, na.rm=T),
  'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                 RegData$EnhNivaa[indNEMS], FUN=median, na.rm=T),
  'Reinnleggelser, <72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$EnhNivaa,
                                        FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Utskrevet kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$EnhNivaa,
                                       FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Skrøpelighet (median)' = tapply(RegData$FrailtyIndex[indFrail],
                                   RegData$EnhNivaa[indFrail], FUN=median, na.rm=T),
  'Perkutan trakeostomi (%)' = tapply(RegData$Trakeostomi==2, RegData$EnhNivaa,
                                    FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Kont. hemofiltr. v/nyreerstat. (%)' = tapply(RegData$Kontinuerlig[indNyre], RegData$EnhNivaa[indNyre],
                                               FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Kont. hemofiltr. beh.tid (median)' =  tapply(RegData$KontinuerligDays[indNyre], RegData$EnhNivaa[indNyre],
                                              FUN=median, na.rm=T),
  'Intermitt. hemodia. v/nyreerstat.' = tapply(RegData$Intermitterende[indNyre], RegData$EnhNivaa[indNyre],
                                                  FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Intermitt. hemodia. beh.tid (median)' = tapply(RegData$IntermitterendeDays[indNyre], RegData$EnhNivaa[indNyre],
                                               FUN=median, na.rm=T),
  'Mottatt pressor' = tapply(RegData$VasoactiveInfusion[indVaso]==1, RegData$EnhNivaa[indVaso],
                             FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$EnhNivaa,
                      FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1))
)
xtable::xtable(tabNokkeltall, digits= 2, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
               label = 'tab:nokkelKat',
               caption = paste0('Nøkkeltal og aktivitet i norsk intensivmedisin, ', aarsrappAar))


# Legge til:
#OK	Median, SAPS II uten alderspoeng
# o	Nyreerstattende behandling: hemofiltrasjon. For eksempel som andel og median dager. (‘Kontinuerlig’ og  ‘KontinuerligDays’)
# KidneyReplacingTreatment ==1
#OK	Utført tracheostomi på intensiv (ikke bruk kirurgisk/på operasjonsavd) (Bruk: ‘Trakeostomi’ med verdi ‘2’)
#OK o	Median Fraility skår




# -Alder og kjønn. Helst med kategoriene, om mulig.
#Alder:
  round(tapply(RegData1aar$Alder, INDEX = RegData1aar$ShNavn, FUN = mean),1) #, na.rm=T)
  Kategori 1a Kategori 1b Kategori 2a Kategori 2b  Kategori 3
  69.9        61.0        66.1        31.2        57.5
  tapply(RegData1aar$Alder, INDEX = RegData1aar$ShNavn, FUN = median)
  Kategori 1a Kategori 1b Kategori 2a Kategori 2b  Kategori 3
  73.1        66.8        71.1        18.2        63.3

  tapply(RegData1aar$Alder, INDEX = RegData1aar$ShNavn, FUN = length)
  Kategori 1a Kategori 1b Kategori 2a Kategori 2b  Kategori 3
  1585        3307        9476         967        5743


  #Fordeling av kjønn per sykehustype og år
tabShTypeAar <- table(RegData$Aar, RegData$EnhNivaa)
tabKj <- table(RegData[RegData$erMann==1 , c('Aar', 'ShNavn')])
kjLandet <- prop.table(table(RegData[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
AndelMenn <- rbind(AndelMenn,
                   'Alle år' = 100*c(prop.table(table(RegData[ , c("erMann",'ShNavn')]),2)[2,],
                                     prop.table(table(RegData[ , "erMann"]))[2]))

colnames(AndelMenn)[6] <- 'Hele landet'
xtable::xtable(AndelMenn, digits=1, align=c('l', rep('r', ncol(AndelMenn))),
       caption='Del (prosent) av intensivopphald som er menn.', label='tab:KjonnAarKat')



#---------Barn <16 år ------------------------------
#RegData <- NIRUtvalgEnh(RegData = RegData, datoFra = '2015-01-01')$RegData
nivaa <- 1:6
nivaaKort <- c('1a', '1b', '2b', '3', '3b', '3c')
nivaaTxt <- c('Overvåk', 'Postop', 'Gen <50','Gen >50', 'Spesial',  'Barn')
over <- c('IkkeOverf', 'Overf')

for (overfPas in 1:2) {
  #nivaa <- 6
  nivaa <- 1:5

  #Fordeling

  for (valgtVar in c('PIMsanns', 'liggetid','InnMaate', 'inklKrit')) {
    outfile <- paste0(valgtVar,'_',paste0(nivaaKort[nivaa], collapse = ""), '_',over[overfPas], '_Ford0_15aar.pdf')
    NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                  minald = 0, maxald = 15, overfPas = overfPas, nivaa = nivaa, outfile=outfile)
  }

  #Enhetsnivå
  for (valgtVar in c('PIMsanns', 'liggetid','alder',
                     'respiratortidInvMoverf',  'respiratortidNonInv')){ #
    outfile <- paste0(valgtVar, '_', paste0(nivaaKort[nivaa], collapse = ""), '_',over[overfPas], '_MedPrSh0_15aar.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    minald = 0, maxald = 15, overfPas = overfPas, nivaa = nivaa, outfile=outfile)
  }

  #Tidstrend

  for (valgtVar in c('PIMsanns', 'liggetid','alder', 'respiratortid',
                     'respiratortidInvMoverf',  'respiratortidNonInv')){
    outfile <- paste0(valgtVar, '_', paste0(nivaaKort[nivaa], collapse = ""), '_',over[overfPas], 'MedTid0_15aar.pdf')
    NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar,
                  minald = 0, maxald = 15, datoFra = '2015-01-01',
                  valgtMaal='Med', tidsenhet= 'Aar', overfPas = overfPas, nivaa = nivaa,
                  outfile=outfile)
  }

  NIRFigAndelTid(RegData = RegData, preprosess = 0, valgtVar = 'dodeIntensiv',
                 minald = 0, maxald = 15, datoFra = '2015-01-01', overfPas = overfPas, nivaa = nivaa,
                 outfile = paste0('dodeIntensivAndelTid_', paste0(nivaaKort[nivaa], collapse = ""),
                                  '_', over[overfPas],'0_15aar.pdf'))
}


#--------------------------------------Data til offentlig visning (SKDE, Behandlingskvalitet)-------------------------------------
setwd('~/Aarsrappresultater/NETTsider/')
library(intensiv)
library(magrittr)
NIRData <- NIRPreprosess(RegData = NIRRegDataSQL(datoFra = '2016-01-01'))
indUShNavn <- which(NIRData$ShNavn =='')
NIRData$ReshId[indUShNavn]
unique(NIRData$Aar[indUShNavn])
table(NIRData[which(NIRData$ReshId == 4210053), 'ShNavn'])
#NIRData <- NIRData[-which(NIRData$ShNavn ==''), ]
#nov2024: Kun Bodø som mangler sykehusnavn og den aktuelle reshid'en er mappet til orgnr.


tab <- unique(NIRData[order(NIRData$ShNavn) ,c("ShNavn", "ReshId")])
indFlereResh <- which(NIRData$ShNavn %in% names(table(tab$ShNavn)[table(tab$ShNavn)>1]))
sort(table(tab$ShNavn))
#Har to Sykehusnavn: 4210053 dvs. Bodø mangler sykehusnavn
tab <- unique(NIRData[indFlereResh, c("ShNavn", "ReshId")])
tab[order(tab$ShNavn),  c("ShNavn", "ReshId")]

'respiratortidInvUoverf'
# RegData <- NIRData
# ind <- which(RegData$InvasivVentilation>0) %i%
#   which(RegData$InnDato>=as.Date('2015-01-01', tz='UTC')) %i% which(RegData$Overf ==1)
# RegData <- RegData[ind,]
#
# table(RegData[RegData$ShNavn %in% c('Mosjøen', 'Haraldplass') ,c('Aar', 'ShNavn')])


nyResh <- setdiff(unique(NIRData$ReshId), names(nyID))
unique(NIRData[which(NIRData$ReshId %in% nyResh),c("ShNavn", "ReshId", "Aar")])

#Okt 2024:
# ShNavn   ReshId
# Gjøvik  4212166
# KalnesØstf.  4208977
# Førde   701577
# Lovisenberg 42088921
# Skien   102428
# KalnesØstf.  4208976
# Hamar   108827
# Sandnessjøen  4210742

ind1 <- dataTilOffVisning(RegData = NIRData, valgtVar='reinn',
                                 indID = 'intensiv_innlegg_72t', filUt = 'innlegg_72t')

ind2 <- dataTilOffVisning(RegData = NIRData, valgtVar='respiratortidInvUoverf', # respiratortidInvMoverf
                                 indID = 'intensiv_inv_vent', filUt = 'inv_vent')

NIRindFraReg <- rbind(ind1, ind2)

write.table(NIRindFraReg, file = 'NIRindFraReg.csv', sep = ';', row.names = F)
#tapply(DataTilSKDE$var, INDEX = DataTilSKDE$year, FUN = mean)



#----Kvalitetsindikatorer på enhetsnivå ("manuelle" indikatorer)
setwd('C:/ResultattjenesteGIT/Aarsrapp/NETTsider/') #Fra åpen harddisk C:\ResultattjenesteGIT\Aarsrapp\NETTsider
#  KvalIndManuellNy <- read.table(file = 'KvalIndNIR2023manuelle.csv', fileEncoding = 'UTF-8', sep = ';', header = TRUE) #, row.names = FALSE)
KvalIndManuellNy <- readxl::read_excel('IntensivKvalIndManuell2023_2024_RAA.xlsx')
TidligereKvalIndReg <- read.table(file = 'IntensivKvalIndPublManuell2017_22.csv', fileEncoding = 'UTF-8', sep = ';', header = TRUE)
names(table(TidligereKvalIndReg$ind_id))

indKIfraReg <- which(TidligereKvalIndReg$ind_id %in% c('intensiv_innlegg_72t', 'intensiv_inv_vent', 'intensiv_dg') )
TidligereKvalIndReg <- TidligereKvalIndReg[-indKIfraReg, ]
table(TidligereKvalIndReg$year)
names(TidligereKvalIndReg)

#nye <- setdiff(unique(as.character(KvalIndManuellNy$resh_id)), names(nyID))
#NIRData[which(NIRData$ReshId %in% nye), c("resh_id", "namn")]

#Dataomorganisering
names(KvalIndManuellNy)
RegData <- KvalIndManuellNy[, c("resh_id", "tverrfagleg_gjennomgang", "rutinenotat", "primarvakt", "data_nir", "year")]
RegData$primarvakt <- dplyr::recode(RegData$primarvakt, '2' = 1, '3' = 0, .default = RegData$primarvakt) #1-ja, 2-nei Innh: -1,1,2,3
#RegData$primarvakt <- dplyr::case_match(RegData$primarvakt, 2 ~ 1, 3 ~ 0) #1-ja, 2-nei Innh: -1,1,2,3
variabler <- c( "tverrfagleg_gjennomgang", "rutinenotat",  "data_nir")
RegData[ , variabler][RegData[,variabler] == 2] <- 0
RegData$orgnr <- as.character(nyID[as.character(RegData$resh_id)])
table(RegData$orgnr, useNA = 'a')

RegDataUt <- tidyr::pivot_longer(
  data = RegData[,-which(names(RegData)=='resh_id')],
  cols = c("tverrfagleg_gjennomgang", "rutinenotat", "primarvakt", "data_nir"),
  names_to = 'ind_id'
  ,values_to = 'var'
)
table(RegDataUt$var, useNA = 'a')
#RegDataUt <- RegDataUt[-which(RegDataUt$var == -1), ]

RegDataUt$ind_id <- paste0('intensiv_', RegDataUt$ind_id)
RegDataUt$denominator <- 1
#RegDataUt$year <- 2023
RegDataUt$context <- 'caregiver'
head(RegDataUt)
head(TidligereKvalIndReg)
KvalIndManuellAlleAar <- rbind(RegDataUt, TidligereKvalIndReg[ ,names(RegDataUt)])
write.table(KvalIndManuellAlleAar, file = 'IntensivKvalIndEnhNivaa.csv', sep = ';', row.names = F)

