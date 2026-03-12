#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

rm(list=ls())
library(intensiv)
aarsrappAar <- 2023
datoFra <- '2011-01-01'
datoTil <- paste0(aarsrappAar, '-12-31')
datoFra1aar <- paste0(aarsrappAar, '-01-01')
setwd('~/Aarsrappresultater/NiPar23') #/intensiv
RegData <- NIRPreprosess(NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil))
#Registrert på feil resh:
RegData$ReshId[RegData$ReshId == 100132] <- 102026
RegData1aar <- NIRPreprosess(NIRRegDataSQL(datoFra=datoFra1aar, datoTil=datoTil))

table(RegData$ShNavn, RegData$Aar)
test <- unique(RegData[ ,c('ShNavn', 'ReshId')])
test[order(test$ShNavn),]
table(test$ShNavn)


#--------------------------------------- Fordelinger ----------------------------------

#Fjernet apr. -22, 'respiratortidInvMoverf',  #'CerebralCirculationAbolishedReasonForNo', 'frailtyIndex', 'komplikasjoner',


variabler <- c('OrganDonationCompletedReasonForNoStatus',
               'frailtyIndex', 'inklKrit','liggetid','InnMaate',
              'NEMS24', 'Nas24', 'regForsinkelse', 'respiratortidNonInv',
              'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak')

for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                 outfile=outfile)
}

NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar='CerebralCirculationAbolishedReasonForNo',
              outfile='CerebralCirculationAbolishedReasonForNo_Ford.pdf')

#Lagt til mai -23
variabler <- 'komplikasjoner' #c('frailtyIndex', )
for (grType in 2:3) {
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, '_', grType, 'Ford.pdf')
    NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, 
                       Ngrense=10, grType=grType, outfile=outfile)
  }
}

  # NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar='spesTiltak', grType = 3,
  #             outfile='spesTiltak_ford.pdf')

#------ AndelTid-ingen ---------------------------------------------

#---------------------GjsnTid----------------------------------------------
# Alder, hele landet
# Liggetid, hele landet
# Liggetid, død
# SAPSII

variabler <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')

for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, 'MedTid.pdf')
  NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                valgtMaal='Med', tidsenhet= 'Aar', outfile=outfile)
}


#--------------------------------------- AndelGrVar ----------------------------------
#Ny mai 2023: variabler <- 'komplReg' 'frailtyIndex', ''potDonor'
variabler <- c('dod30d', 'frailtyIndex', 'komplReg', #'komplikasjoner', 
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi')
variabler <- 'komplReg'
for (grType in 2:3) {
      for (valgtVar in variabler) {
            outfile <- paste0(valgtVar, grType, 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, 
                               Ngrense=10, grType=grType, outfile=outfile)
      }
}

NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='OrganDonationCompletedCirc', 
                     Ngrense=10, outfile='OrganDonationCompletedCircPrSh.pdf')

NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='regForsinkelse', 
                   Ngrense=10)

# #Organdonorer av døde: OrganDonationCompletedStatus
# #Organdonorer, av alle med opphevet intrakran. sirk.': 'OrganDonationCompletedCirc',

#--------------------------------------- SENTRALMÅL per enhet----------------------------------

variabler <- c('alder',  'NEMS24', 'Nas24',
              'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
  #Lagt til mai -23: variabler <- 'frailtyIndex'
for (grType in 2:3) {
      for (valgtVar in variabler){ #
            outfile <- paste0(valgtVar,grType, '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                         grType=grType, outfile=outfile)
      }
}


#KvalInd:
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='SMR'
                ,outfile='SMR_PrSh.pdf')

#KvalInd:
#årsrapp 22: Etter litt fram og tilbake endte vi på 'respiratortidInvUoverf'. 
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidInvUoverf', valgtMaal='Med',
                outfile='respiratortidInvUoverf_MedPrSh.pdf')




# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar. 
#Figurer per sykehus
#Tar ut apr02: NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Med',
#                 datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidInvMoverf_MedPrSh.pdf')
#Tar ut apr02: NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Gjsn',
#                 datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidInvMoverf_GjsnPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Med',
                outfile='respiratortidNonInv_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                outfile='respiratortidNonInv_GjsnPrSh.pdf')


#--------------Tilleggsbestilling, 2023, overordnede grupper (kategorier) ------------------
# Jeg har lagt til en ‘label’ på hver enhet, som her har fått navnet ‘Niva’. Med verdi 1-3.
# Ønsket er at du kjører følgende figurer ut fra disse «kategoriene», altså nivåinndelingene.
# 1a, 1b, 2a, 2b, 3
 

# - Median Invasiv ventilasjonsbehandling
# - Median non-invasiv ventilasjonsbehandling
# - Median liggetid
# - Median SAPS
# - Median NEMS

GruppeDef <- read.csv2(file = '/home/rstudio/intensiv/data/NIRenheter_nivaa.csv')
GruppeDef <- GruppeDef %>%
  dplyr::mutate(niva = paste('Kategori', niva))
ind <- match(RegData1aar$ReshId, GruppeDef$resh_id)
#table(RegData1aar$ShNavn[which(is.na(ind))])
RegData1aar$ShNavn <- GruppeDef$niva[ind] 
#RegData1aar <- RegData1aar[!is.na(RegData1aar$ShNavn), ]
RegData$ShNavn <- GruppeDef$niva[match(RegData$ReshId, GruppeDef$resh_id)]


# 1.runde: Skill på overførte og ikke overførte.
variabler <- c( 'liggetid','NEMS','respiratortidInv','respiratortidNonInv','SAPSII',  'SMR') 
for (valgtVar in variabler){ #
  for (overf in 1:2) {
    overfTxt <- c('Uoverf','Overf')[overf]
    outfile <- paste0(valgtVar, overfTxt, '_MedSh.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    overfPas = overf, outfile=outfile)
  } 
}


# 2. runde:

variabler <- c('dod30d', 'frailtyIndex', 'komplReg', #'komplikasjoner', 
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi')
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, 'PrKat.pdf')
    NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, 
                       Ngrense=10, outfile=outfile)
  }

variabler <- c('alder', 'frailtyIndex', 'NEMS24', 'Nas24',
               'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
  for (valgtVar in variabler){ # variabler <- 'frailtyIndex'
    outfile <- paste0(valgtVar, '_MedPrKat.pdf')
    NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                    outfile=outfile)
  }

#KvalInd:
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='SMR'
                ,outfile='SMR_PrKat.pdf')

NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidInvUoverf', valgtMaal='Med',
                outfile='respiratortidInvUoverf_MedPrKat.pdf')


# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar. 
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                outfile='respiratortidNonInv_GjsnPrKat.pdf')


#-----3.runde:

# --Inklusjonskriterier –Om du får til krysstabell, så er det fint. Visst ikke så tar vi nasjonal inklusjonskriterier. 
# OK -- Fordeling type opphold (‘InnMaate’) 
#  NIRFigInnMaate(RegData1aar, preprosess=0, valgtVar='InnMaate', grVar='ShNavn', 
#                outfile='InnMaateKat.pdf')
    
# Tabeller:
 
# -Dialyse – andel Leverdialyse? Nei. Det er denne med kategorier. Krysstabell? Poenget er å visualisere hvilke intensivenheter som tilbyr denne behandlingen, og omfanget.
#    Se Figur - nyreerstattende behandling -> krysstabell for kategorier?


#tabNokkeltall <- tabNokkeltall(RegData=RegData1aar)
RegData <- RegData1aar
indLigget <- which(RegData$liggetid>0)
indRespt <- which(RegData$respiratortid>0)
indRespInv <- which(RegData$InvasivVentilation >0)
indRespNIV <- which(RegData$NonInvasivVentilation>0)
indSAPS <- which(RegData$SAPSII > 0)
indNEMS <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))
RegDataReinn <- NIRVarTilrettelegg(RegData=RegData, valgtVar = 'reinn', figurtype = 'andelGrVar')$RegData
ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
RegData$Ut1708 <- 0
RegData$Ut1708[ind1708]<-1

tabNokkeltall <- rbind(
  'Antall opphold' = tapply(RegData$PasientID, RegData$ShNavn, FUN=length), 
  'Antall pasienter' = tapply(RegData$PasientID, RegData$ShNavn,
                              FUN=function(x) length(unique(x))),
  'Liggedøgn (totalt)' = tapply(RegData$liggetid[indLigget], RegData$ShNavn[indLigget], FUN=sum, na.rm=T),
  'Liggedøgn (median)' = tapply(RegData$liggetid[indLigget], RegData$ShNavn[indLigget], FUN=median, na.rm=T),
  'Mekanisk \nventilasjonsstøtte (%)' = tapply(RegData$respiratortid>0, RegData$ShNavn,
                                               FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Respiratordøgn, \nsamlet (totalt)' = tapply(RegData$respiratortid[indRespt], RegData$ShNavn[indRespt],
                                               FUN=sum, na.rm=T),
  'Respiratordøgn, \nsamlet (median)' = tapply(RegData$respiratortid[indRespt], RegData$ShNavn[indRespt],
                                               FUN=median, na.rm=T),
  'Respiratordøgn, \ninvasiv (median)' = tapply(RegData$InvasivVentilation[indRespInv], RegData$ShNavn[indRespInv],
                                                FUN=median, na.rm=T),
  'Respiratordøgn, \nnon-invasiv (median)' = tapply(RegData$NonInvasivVentilation[indRespNIV], RegData$ShNavn[indRespNIV], 
                                                    FUN=median, na.rm=T),
  'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$ShNavn[indSAPS], FUN=median, na.rm=T),
  'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                           RegData$ShNavn[indNEMS], FUN=sum, na.rm=T),
  'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                 RegData$ShNavn[indNEMS], FUN=median, na.rm=T),
  'Reinnleggelser, \n<72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$ShNavn,
                                        #tapply(RegData$Reinn[indReinn]==1, RegData$ShNavn[indReinn],
                                        FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Utskrevet \n kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$ShNavn,
                                       FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
  'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$ShNavn,
                      FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1))
)
xtable::xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
               label = 'tab:nokkelKat',
               caption = paste0('Samla tal på intensivopphald og aktivitet i NIR, ', aarsrappAar))

  # -tabell intensivopphald per eining?  
#Belegg
tabBeleggN <- rbind(
  'Ferdigstilte intensivopphald' = tapply(RegData1aar$PasientID, RegData1aar$ShNavn, FUN=length),
  'Registrerte pasientar' = tapply(RegData1aar$PasientID, RegData1aar$ShNavn,
                                   FUN=function(x) length(unique(x))),
  'Tal intensivdøger' = round(as.numeric(tapply(RegData1aar$liggetid, RegData1aar$ShNavn, sum, na.rm=T)),0),
  'Gjennomsnittleg liggjetid' = round(tapply(RegData1aar$liggetid, RegData1aar$ShNavn, mean, na.rm=T),1)
)
tabBeleggNtot <- cbind(tabBeleggN, c(rowSums(tabBeleggN)[1:3], round(mean(RegData1aar$liggetid, na.rm=T),1)))
colnames(tabBeleggNtot)[6] <- 'Hele landet'

xtable::xtable(tabBeleggNtot, digits=0, align=c('l', rep('r', ncol(tabBeleggNtot))),
       caption= paste0('Antal opphald og liggedøger i ',aarsrappAar, '.'), label='tab:RegKat')


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
tabShTypeAar <- table(RegData$Aar, RegData$ShNavn)
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



#---------Tilleggsbestilling, 2023, barn <16 år ------------------------------
RegData <- NIRUtvalgEnh(RegData = RegData, datoFra = '2015-01-01')$RegData

# PIM3
# Fordeling, 2023
# Median, Enhetsnivå for 2023
# Median, Tidstrend fra 2015 – 2023
# 
# Liggetid
# Fordeling, 2023
# Median, enhetsnivå for 2023
# Median, Tidstrend fra 2015 – 2023
# 
# Invasiv ventilasjon 
# Overført + Ikke-overført - mener du en for hver eller samlet
# Median, Tidstrend fra 2015 – 2023
# 
# Non-invasiv ventilasjon
# Overført + Ikke-overført
# Median, enhetsnivå for 2023
# 
# Primærårsak intensivopphold
# Fordeling, 2023
# 
# Inklusjon
# Fordeling, 2023
# 
# Alder
# Fordeling - ny figur som viser fordeling av de under 16 år?
# Median, Enhetsnivå, 2023
# Median, tidstrend fra 2015-2023
# 
# Døde på intensiv
# Andel døde, Tidstrend 2015 – 2023

#Fordeling
variabler <- c('PIMsanns', 'liggetid','InnMaate', 'inklKrit')

for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, '_Ford0_15aar.pdf')
   NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                 minald = 0, maxald = 15, outfile=outfile)
}

#Enhetsnivå
variabler <- c('PIMsanns', 'liggetid','alder', 
               'respiratortidInvMoverf',  'respiratortidNonInv')
for (valgtVar in variabler){ #
   outfile <- paste0(valgtVar, '_MedPrSh0_15aar.pdf')
   NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                   minald = 0, maxald = 15, outfile=outfile)
}

#Tidstrend
variabler <- c('PIMsanns', 'liggetid','alder', 
               'respiratortidInvMoverf',  'respiratortidNonInv',
               'respiratortid')

for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, 'MedTid0_15aar.pdf')
   NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, 
                 minald = 0, maxald = 15, 
                 valgtMaal='Med', tidsenhet= 'Aar', outfile=outfile)
}

#tapply(RegData$PIM_Probability, INDEX = RegData$Aar, FUN = 'mean', na.rm=T)

NIRFigAndelTid(RegData = RegData, preprosess = 0, valgtVar = 'dodeIntensiv',
               minald = 0, maxald = 15, outfile = 'dodeIntensivAndelTid0_15aar.pdf')



#-------------------------------Tabeller--------------------------------
#Belegg
library(intensiv)
library(xtable)
#RegData1aar <- NIRPreprosess(RegData1aar)

tabBeleggN <- rbind(
            'Ferdigstilte intensivopphald' = tapply(RegData1aar$PasientID, RegData1aar$ShType, FUN=length),
            'Registrerte pasientar' = tapply(RegData1aar$PasientID, RegData1aar$ShType,
                                             FUN=function(x) length(unique(x))),
            'Tal intensivdøger' = round(as.numeric(tapply(RegData1aar$liggetid, RegData1aar$ShType, sum, na.rm=T)),0),
            'Gjennomsnittleg liggjetid' = round(tapply(RegData1aar$liggetid, RegData1aar$ShType, mean, na.rm=T),1)
      )
tabBeleggNtot <- cbind(tabBeleggN, c(rowSums(tabBeleggN)[1:3], round(mean(RegData1aar$liggetid, na.rm=T),1)))
colnames(tabBeleggNtot) <- c('lokal-/sentral', 'region', 'alle')

xtable(tabBeleggNtot, digits=0, align=c('l', rep('r', ncol(tabBeleggNtot))),
       caption= paste0('Antal opphald og liggedøger i ',aarsrappAar, '.'), label='tab:RegEget')



#Antall opphold
library(lubridate)
xtable(table(RegData1aar$ShNavn), align=c('l','r'), #row.names=F,
       caption = paste0('Intensivopphald per eining i ', aarsrappAar, '.'))




#Fordeling av kjønn per sykehustype og år
tabShTypeAar <- table(RegData$Aar, RegData$ShType)
tabKj <- table(RegData[RegData$erMann==1 , c('Aar', 'ShType')])
kjLandet <- prop.table(table(RegData[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
#AndelMennShType <- prop.table(table(RegData[ , c("erMann",'ShType')]),2)[2,]
AndelMenn <- rbind(AndelMenn,
                 'Alle år' = 100*c(prop.table(table(RegData[ , c("erMann",'ShType')]),2)[2,],
                            prop.table(table(RegData[ , "erMann"]))[2])
)

colnames(AndelMenn) <- c('Lok./Sentral', 'Region', 'Hele landet')
xtable(AndelMenn, digits=1, align=c('l', rep('r', ncol(AndelMenn))),
       caption='Del (prosent) av intensivopphald som er menn.', label='tab:KjonnAar')


#Aktivitet/Nøkkeltall

#Legge til nivå
Nivaa <- c(
   '4205696' = '1', # Akershus univ.sykehus, Postoperativ
   '108897' = '1', # Diakonhjemmet sykehus, Postop/intensivavd
   '100180' = '1', # Haraldsplass diakonale sykehus, Med intensiv postoperativ
   '106271' = '1', # Haukeland, KSK Postoperativ
   '107717' = '1', # Haukeland, Lunge 1,Respiratorisk Overvakingseining (ROE)
   '4208892' = '1', # Lovisenberg Diakonale, MIO
   '705757' = '1', # OUS, Radiumhospitalet - Postop og intensivavd
   '109779' = '1', # OUS, Ullevål - Nevrointensiv
   '109778' = '1', # OUS, Ullevål - Postoperativ
   '4205574' = '1', # St. Olavs Hospital - Medisin- og lungeovervåkning
   '102090' = '2', # Akershus univ.sykehus, Intensiv (generell)
   '105282' = '2', # Førde sentralsjukehus
   '101858' = '2', # Hammerfest Sykehus, Akuttmed avd, Intensiv
   '100273' = '2', # Haugesund sjukehus, Intensiv
   '109363' = '2', # Haukeland, Brannskadeavdelinga
   '106285' = '2', # Haukeland, KSK Thoraxkirurgisk intensiv (TIO)
   '105048' = '2', # Haukeland, Medisinsk intensiv og overvaking (MIO)
   '103015' = '2', # Helgelands. Mo i Rana
   '103141' = '2', # Helgelands. Mosjøen
   '103149' = '2', # Helgelands.Sandnessjøen
   '102250' = '2', # Helse NT Levanger
   '105893' = '2', # Helse NT Namsos
   '101830' = '2', # Kirkenes sykehus, Akuttmed avd, Intesniv
   '706078' = '2', # Kristiansund sykehus, Intensiv (felles)
   '706079' = '2', # Molde sykehus, Intensiv (felles)
   '4210053' = '2', # Nordlandssykehuset, Bodø - Intensivavdelingen (generell)
   '110867' = '2', # Nordlandssykehuset, Vesterålen - Intensiv Stokmarknes
   '111487' = '2', # OUS, Aker - Postoperativ og intensiv
   '111449' = '2', # OUS, Ullevål - Barneintensiv
   '109877' = '2', # OUS, Ullevål - Hjertekirurgisk postoperativ
   '4205969' = '2', # OUS, Ullevål - Hjertemedisinsk intensiv
   '109870' = '2', # OUS, Ullevål - Medisinsk intensiv
   '106572' = '2', # St. Olavs Hospital - Hjertemedisinsk intensiv
   '114282' = '2', # Stavanger univ.sjkehus - Intensiv 2M (generell)
   '4208715' = '2', # Sykehuset i Kongsberg, Intensiv
   '103948' = '2', # Sykehuset i Vestfold, Tønsberg, Intensivmed. seksjon 
   '108609' = '2', # Sykehuset Innlandet, Akuttmed. Elverum
   '108618' = '2', # Sykehuset Innlandet, Akuttmed. Gjøvik
   '108610' = '2', # Sykehuset Innlandet, Akuttmed. Hamar,
   '108626' = '2', # Sykehuset Innlandet, Akuttmed. Lillehammer
   '102026' = '2', # Sykehuset Telem. Skien, Akuttmed avd, Intensv
   '4209889' = '2', # Sykehuset Østfold Kalnes, Intensiv
   '104450' = '2', # Sørlandet s. Arendal, Intensivenheten
   '114240' = '2', # Sørlandet s. Kristiansand, Intensivenheten
   '700617' = '2', # UNN, Harstad - Intensiv
   '700618' = '2', # UNN, Narvik
   '601302' = '2', # UNN, Tromsø Medisinsk intensiv- og hjerteoppvåkning
   '103090' = '2', # Vestre Viken HF, Bærum sykehus, Int.
   '103620' = '2', # Vestre Viken HF, Drammen sykeh. Avd. for anest. og int.med
   '103539' = '2', # Vestre Viken, HF, Ringerike sykeh. Intensivavdelingen
   '4209764' = '2', # Volda sjukehus, Intensiv (felles)
   '108308' = '2', # Ålesund sjukehus Kir.int.
   '102673' = '2', # Ålesund sjukehus Med.int.
   '112044' = '3', # Haukeland, KSK Intensiv (Generell)
   '705576' = '3', # OUS, Rikshospitalet - Barneintensiv
   '706929' = '3', # OUS, Rikshospitalet - Generell Intensiv. 2
   '705577' = '3', # OUS, Rikshospitalet - Generell Intensiv 1
   '109773' = '3', # OUS, Ullevål - Generell intensiv
   '4201313' = '3', # St Olavs Hospital - Hovedintensiv
   '700720' = '3' # UNN, Tromsø - Intensivavdelingen (generell intensiv)
)

RegData1aar$Nivaa <- as.character(Nivaa[as.character(RegData1aar$ReshId)])

tabNokkeltall <- tabNokkeltall(RegData=RegData1aar, datoTil=datoTil) #, tidsenhet='Mnd'
xtable::xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
               label = 'tab:nokkel',
               caption = paste0('Samla tal på intensivopphald og aktivitet i NIR, ', aarsrappAar))


for (nivaa in 1:3) {
   nivaa
   RegDataNivaa <- RegData1aar[RegData1aar$Nivaa == nivaa, ]
   tabNokkeltall <- tabNokkeltall(RegData=RegDataNivaa, datoTil=datoTil) #, tidsenhet='Mnd'
   print(xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
          caption = paste0('Samla tal på intensivopphald og aktivitet i NIR, einingar nivå ', nivaa)))
}
#Legg til  \resizebox{\columnwidth}{!}{ \begin{tabular}... }



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

