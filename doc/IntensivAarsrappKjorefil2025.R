#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister


rm(list=ls())
library(intensiv)
setwd('../Aarsrapp/Intensiv')
aarsrappAar <- 2025
datoFra <- '2015-01-01'
datoTil <- paste0(aarsrappAar, '-12-31')
datoFra1aar <- paste0(aarsrappAar, '-01-01')
source("C:/Users/lro2402unn/RegistreGIT/intensiv/dev/sysSetenv.R")
RegData <- NIRPreprosess(NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil))
shNivaaTxt <- c('Overvåk', 'Postop', '≤50% kat3','>50% kat3', 'Barn') #teksten kommer fra UtvEnh
RegData$ShNivaa <- shNivaaTxt[RegData$NivaaNum]

RegData1aar <- NIRUtvalgEnh(RegData = RegData, datoFra = datoFra1aar)$RegData

#----------------Nye bestillinger for 2025------------------
# Legg til figurer
- median-tid for ikke-invasiv ventilasjon for kategori:  overvåkningsenh.

Organdonasjon:
  Må inkludere:
  Andel/Frekvens av gjennomførte organdonasjoner, fordelt på metode (cDCD og DBD)


Spørsmål om ny analyse:
  Hvordan stiller du deg til for eksempel en tabell som fremstiller spesifikke behandlinger som gis på intensiv?
  Her eksemplifisert med en tabell:

  Variabel	Menn	Kvinner	Totalt
  CRRT, % (n)
Alder (median) (p25 -p75)
Dager (median) (p25 - p75)
Død på intensiv % (n)
  ECMO, % (n)
Alder (median) (p25 -p75)
ECMO, dager, median (p25 -p75)
Død på intensiv % (n)
  ICP, % (n)
Alder (median) (p25 -p75)
ICP, dager, median  (p25 -p75)
Død på intensiv % (n)
  Impella, % (n)
Alder (median) (p25 -p75)
Impella, dager, median  (p25 -p75)
Død på intensiv % (n)
  IABP, % (n)
Alder (median) (p25 -p75)
IABP, dager, median  (p25 -p75)
Død på intensiv % (n)




# ------------------------- FIGURER UTEN inndeling I enhetsNIVÅ----------------------------------
NIRFigInnMaate(RegData1aar, preprosess=0, outfile='TypeOpph.pdf')

#--------------------------------------- Fordelinger
variabler <- c('OrganDonationCompletedReasonForNoStatus',
               'CerebralCirculationAbolishedReasonForNo',
               'frailtyIndex', 'inklKrit','liggetid','InnMaate',
               'komplikasjoner',
              'NEMS24', 'Nas24', 'regForsinkelse', 'respiratortidNonInv',
              'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak')

for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                 outfile=outfile)
}

#NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=PIMdod,


#------------ Andelsh
variabler <- c('komplReg', 'OrganDonationCompletedCirc', 'regForsinkelse', 'reinn')

for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_PrSh.pdf')
  NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                outfile=outfile)
}

#---------------------GjsnTid
variabler <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')

variabler <- 'Nas24'  # 'respiratortidInvUoverf' # 'respiratortidInvMoverf'
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_MedTid.pdf')
  NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                valgtMaal='Med', tidsenhet= 'Aar', outfile=outfile)
}

#-------------GjsnGrVar

NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='SMR'
                ,outfile='SMR_PrSh.pdf')

NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='PIMdod',
                outfile='PIMdod_PrSh.pdf')

NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                outfile='respiratortidNonInv_GjsnPrSh.pdf')

variabler <- c('Nas24', 'NEMS', 'respiratortidInvMoverf', 'respiratortidInvUoverf', 'respiratortidNonInv')
for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_MedPrSh.pdf')
  NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                valgtMaal='Med', outfile=outfile)
}



# ------------------------- FIGURER som skiller på enhetsNIVÅ----------------------------------
#Endringsønske 12.juni 2025:
#slå sammen spesialiserte enheter (3b) og <50 % kategori 3 senger (2b) -> < 50 % kategori 3-senger (2b)
# Dvs. 3b -> 2b, nivå 6->5
# 1. Overvakingseiningar
# 2. Postoperative einingar
# 3. Generelle intensiveiningar med ≤ 50 % kategori 3-senger.
# 4. Generelle intensiveiningar med < 50 % kategori 3-senger
# 5. Barneintensiv

#Nivaa: 1a    1b    2b     3    3c
#NivaaNum: 1:5
#shNivaaTxt <- c('Overvåk', 'Postop', '≤50% kat3','>50% kat3', 'Barn') teksten kommer fra UtvEnh

nivaaKort <- c('1a', '1b' ,'2b' ,'3', '3c')
#------------Fordelingsfigurer
variabler <- c('komplikasjoner', 'frailtyIndex')
for (nivaa in 1:5) {
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, '_',nivaaKort[nivaa], 'Ford.pdf')
    NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                  Ngrense=10, nivaa=nivaa, outfile=outfile)
  }
}

variabler <- c('nyreBehTid','nyreBeh')
for (nivaa in 3:4){
  for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, '_',nivaaKort[nivaa], 'Ford.pdf')
  NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                Ngrense=10, nivaa=nivaa, outfile=outfile)
}}

#-------------- AndelGrVar
# #Organdonorer av døde: OrganDonationCompletedStatus
# #Organdonorer, av alle med opphevet intrakran. sirk.': 'OrganDonationCompletedCirc',
variabler <- c('dod30d', 'frailtyIndex', 'komplReg',
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus', # - bare 3:4 aktuell
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi')
variabler <- 'OrganDonationCompletedStatus'
for (nivaa in 3:4) {
      for (valgtVar in variabler) {
            outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                               Ngrense=10, nivaa=nivaa, outfile=outfile)
      }
}

variabler <- c('nyreBehTid','nyreBeh')
for (nivaa in 3:4){
  for (valgtVar in variabler) {
    outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], 'PrSh.pdf')
    NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                       Ngrense=10, nivaa=nivaa, outfile=outfile)
  }
}

#------------ SENTRALMÅL per enhet

variabler <- c('alder', 'NEMS24', # 'NAS24',
              'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
variabler <- 'liggetid'
for (nivaa in 1:5) {
      for (valgtVar in variabler){ #
            outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                            nivaa=nivaa, outfile=outfile)
      }
}

for (nivaa in 2:5) {
    outfile <-  paste0('NAS24_',nivaaKort[nivaa], '_MedPrSh.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='NAS24', valgtMaal='Med',
                    nivaa=nivaa, outfile=outfile)
}


#Nye, 2024
#   •	‘EcmoEcla’ – andel TRUE per enhet. En figur for hver av kategoriene Gen.int >50, postop
# •	‘EcmoEclaDager’ [EcmoEcla=TRUE] – median per enhet. En figur for hver av kategoriene Gen.int >50, postop
# •	‘Iabp’– andel TRUE per enhet. En figur for hver av kategoriene Gen.int.<50 og Gen.int >50
# •	‘Impella’– andel TRUE per enhet. En figur for hver av kategoriene Gen.int.<50 og Gen.int >50.


for (nivaa in c(2,3)) {
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

variabler <- c('alder', 'NEMS24', 'NAS24',
               'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
for (nivaa in 1:5) {
  for (valgtVar in variabler){
    outfile <-  paste0(valgtVar, '_',nivaaKort[nivaa], '_MedTid.pdf')
    NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                  tidsenhet= 'Aar', nivaa=nivaa, outfile=outfile)
  }
}


#--------------Overordnede nivå som enhetskategorier ------------------
# Jeg har lagt til en ‘label’ på hver enhet, som her har fått navnet ‘Niva’. Med verdi 1-3.
# Ønsket er at du kjører følgende figurer ut fra disse «kategoriene», altså nivåinndelingene.
# 1a, 1b, 2a, 2b, 3

#Endrer til Nivå:

RegData1aar$ShNavn <- RegData1aar$ShNivaa

# Skill på overførte og ikke overførte.
variabler <- c( 'liggetid','NEMS','respiratortidInv','respiratortidNonInv','SAPSII',  'SMR')
for (valgtVar in variabler){ #
  for (overf in 1:2) {
    overfTxt <- c('Uoverf','Overf')[overf]
    outfile <- paste0(valgtVar, overfTxt, '_MedNivaa.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    grVar = 'ShNivaa', overfPas = overf, outfile=outfile)
  }
}

variabler <- c('dod30d', 'frailtyIndex', 'komplReg', #'komplikasjoner',
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi',
               'nyreBeh' )
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, '_PrNivaa.pdf')
    NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                       grVar = 'ShNivaa', Ngrense=10, outfile=outfile)
  }

variabler <- c('alder', 'liggetid', 'frailtyIndex', 'NEMS24', 'NAS24',
               'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
  for (valgtVar in variabler){ # variabler <- 'frailtyIndex'
    outfile <- paste0(valgtVar, '_MedPrNivaa.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    grVar = 'ShNivaa', outfile=outfile)
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
# OK 2.apr  2026


#Fordeling av kjønn per sykehustype og år
#RegData$EhnNivaa <-
tabShTypeAar <- table(RegData$Aar, RegData$ShNivaa)
tabKj <- table(RegData[RegData$erMann==1 , c('Aar', 'ShNivaa')])
kjLandet <- prop.table(table(RegData[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
AndelMenn <- rbind(AndelMenn,
                   'Alle år' = 100*c(prop.table(table(RegData[ , c("erMann",'ShType')]),2)[2,],
                                     prop.table(table(RegData[ , "erMann"]))[2])
)
colnames(AndelMenn)[6] <- 'Hele landet'
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
  'Antal opphald' = tapply(RegData$PasientID, RegData$EnhNivaa, FUN=length),
  'Antal pasientar' = tapply(RegData$PasientID, RegData$EnhNivaa,
                              FUN=function(x) length(unique(x))),
  'Alder (median)' = tapply(RegData$Alder, RegData$EnhNivaa, FUN=median, na.rm=T),
  'Pasientar >80 år' = tapply(RegData$Alder > 80, RegData$EnhNivaa,
                            FUN=function(x) round(sum(x, na.rm=T)/length(x)*100, 1)),
  'Liggedøger (median)' = tapply(RegData$liggetid[indLigget], RegData$EnhNivaa[indLigget], FUN=median, na.rm=T),
  'Mekanisk ventilasjonsstøtte (%)' = tapply(RegData$respiratortid>0, RegData$EnhNivaa,
                                               FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),

  'Respiratordøger, samla (median)' = tapply(RegData$respiratortid[indRespt], RegData$EnhNivaa[indRespt],
                                               FUN=median, na.rm=T),
  'Respiratordøger, invasiv (median)' = tapply(RegData$InvasivVentilation[indRespInv], RegData$EnhNivaa[indRespInv],
                                                FUN=median, na.rm=T),
  'Respiratordøger, non-inv. (median)' = tapply(RegData$NonInvasivVentilation[indRespNIV], RegData$EnhNivaa[indRespNIV],
                                                    FUN=median, na.rm=T),
  'SAPSII (median)' = tapply(RegData$SAPSII[indSAPS], RegData$EnhNivaa[indSAPS], FUN=median, na.rm=T),
  'SAPSII u/alder (median)' = tapply(RegData$SAPSuAld[indSAPS], RegData$EnhNivaa[indSAPS], FUN=median, na.rm=T),
  # 'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
  #                         RegData$EnhNivaa[indNEMS], FUN=sum, na.rm=T),
  'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                 RegData$EnhNivaa[indNEMS], FUN=median, na.rm=T),
  'Reinnleggingar, <72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$EnhNivaa,
                                        FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Utskrivne kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$EnhNivaa,
                                       FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Skrøpeligheit (median)' = tapply(RegData$FrailtyIndex[indFrail],
                                   RegData$EnhNivaa[indFrail], FUN=median, na.rm=T),
  'Perkutan trakeostomi (%)' = tapply(RegData$Trakeostomi==2, RegData$EnhNivaa,
                                    FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Kont. hemofiltr. (%)' = tapply(RegData$Kontinuerlig[indNyre], RegData$EnhNivaa[indNyre],
                                               FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Kont. hemofiltr. beh.tid (median)' =  tapply(RegData$KontinuerligDays[indNyre], RegData$EnhNivaa[indNyre],
                                              FUN=median, na.rm=T),
  'Intermitt. hemodialyse' = tapply(RegData$Intermitterende[indNyre], RegData$EnhNivaa[indNyre],
                                                  FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Intermitt. hemodia. beh.tid (median)' = tapply(RegData$IntermitterendeDays[indNyre], RegData$EnhNivaa[indNyre],
                                               FUN=median, na.rm=T),
  'Fått vasoaktiv med.' = tapply(RegData$VasoactiveInfusion[indVaso]==1, RegData$EnhNivaa[indVaso],
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
over <- c('IkkeOverf', 'Overf')
nivaa <- 1
for (overfPas in 1:2) {

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
source("dev/sysSetenv.R")
setwd('../Aarsrapp')
library(intensiv)
library(magrittr)
NIRData <- NIRPreprosess(RegData = NIRRegDataSQL(datoFra = '2016-01-01'))

indUShNavn <- which(NIRData$ShNavn =='')
NIRData$ReshId[indUShNavn]
unique(NIRData$Aar[indUShNavn])


tab <- unique(NIRData[order(NIRData$ShNavn) ,c("ShNavn", "ReshId")])
indFlereResh <- which(NIRData$ShNavn %in% names(table(tab$ShNavn)[table(tab$ShNavn)>1]))
sort(table(tab$ShNavn))
#Har to Sykehusnavn: 4210053 dvs. Bodø mangler sykehusnavn
tab <- unique(NIRData[indFlereResh, c("ShNavn", "ReshId")])
tab[order(tab$ShNavn),  c("ShNavn", "ReshId")]


nyResh <- setdiff(unique(NIRData$ReshId), names(nyID))
unique(NIRData[which(NIRData$ReshId %in% nyResh),c("ShNavn", "ReshId", "Aar")])

# Juni 2025:
# ShNavn  ReshId  Aar
# Helse Førde HF  100085 2025
# SNR Intensiv 4209729 2025

# Fjerner nye resh i denne publiseringa (juli 2025):
NIRData <- NIRData[-which(NIRData$ReshId %in% nyResh), ]


max(NIRData$DateAdmittedIntensive) # "2025-06-21 23:32:00

ind1 <- dataTilOffVisning(RegData = NIRData, valgtVar='reinn',
                                 indID = 'intensiv_innlegg_72t', filUt = 'innlegg_72t')

ind2 <- dataTilOffVisning(RegData = NIRData, valgtVar='respiratortidInvUoverf', # respiratortidInvMoverf
                                 indID = 'intensiv_inv_vent', filUt = 'inv_vent')

NIRindFraReg <- rbind(ind1, ind2)

write.table(NIRindFraReg, file = 'NIRindFraReg.csv', sep = ';', row.names = F)
#tapply(DataTilSKDE$var, INDEX = DataTilSKDE$year, FUN = mean)



#----Kvalitetsindikatorer på enhetsnivå ("manuelle" indikatorer)
setwd('../Aarsrapp')
# KvalIndManuellNy <- read.table(file = 'IntensivKvalIndManuell2024_RAA.csv', fileEncoding = 'UTF-8', sep = ';',
#                               header = TRUE, row.names = FALSE)
KvalIndManuellNy <- readxl::read_excel('IntensivKvalIndManuell2024_RAA.xlsx')
TidligereKvalIndReg <- read.table(file = 'IntensivKvalIndPublManuell2017_23.csv', fileEncoding = 'UTF-8', sep = ';', header = TRUE)
#TidligereKvalIndReg <- readxl::read_excel('IntensivKvalIndPublManuell2017_23.xlsx')
names(table(TidligereKvalIndReg$ind_id))

# indKIfraReg <- which(TidligereKvalIndReg$ind_id %in% c('intensiv_innlegg_72t', 'intensiv_inv_vent', 'intensiv_dg') )
# TidligereKvalIndReg <- TidligereKvalIndReg[-indKIfraReg, ]
# table(TidligereKvalIndReg$year)
# names(TidligereKvalIndReg)


#Dataomorganisering
names(KvalIndManuellNy)
KvalIndManuellNy$year <- 2024
RegData <- KvalIndManuellNy[, c("resh_id", "tverrfagleg_gjennomgang", "rutinenotat", "primarvakt", "data_nir", "year")]
RegData$primarvakt <- dplyr::replace_values(RegData$primarvakt, '2' = 1, '3' = 0, .default = RegData$primarvakt) #1-ja, 2-nei Innh: -1,1,2,3
variabler <- c( "tverrfagleg_gjennomgang", "rutinenotat",  "data_nir")
RegData[ , variabler][RegData[,variabler] == 2] <- 0
RegData$orgnr <- as.character(nyID[as.character(RegData$resh_id)])

#Sjekk
# table(RegData$orgnr, useNA = 'a')
# resh <- RegData$resh_id[which(is.na(RegData$orgnr))]
# tabSjekk <- KvalIndManuellNy[which(KvalIndManuellNy$resh_id %in% resh), ]

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
RegDataUt$context <- 'caregiver'
head(RegDataUt)
head(TidligereKvalIndReg)
KvalIndManuellAlleAar <- rbind(RegDataUt, TidligereKvalIndReg[ ,names(RegDataUt)])
write.table(KvalIndManuellAlleAar, file = 'IntensivKvalIndEnhNivaa.csv', sep = ';', row.names = F)

