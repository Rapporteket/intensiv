#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

rm(list=ls())
library(intensiv)
datoFra <- '2011-01-01'	
datoTil <- '2018-12-31'	#
datoFra1aar <- '2018-01-01'
#------Klargjøre årsrapportfil-------------
RegData <- read.table('A:/Intensiv/MainFormDataContract2019-09-24.csv', sep=';', stringsAsFactors=FALSE, 
                      header=T, encoding = 'UTF-8')	
 indDato <- intersect(which(as.Date(RegData$DateAdmittedIntensive) >= as.Date(datoFra, tz= 'UTC')), 
                         which(as.Date(RegData$DateAdmittedIntensive) <= as.Date(datoTil, tz= 'UTC')))
# indDato <- intersect(which(RegData$DateAdmittedIntensive >= datoFra), 
#                      which(RegData$DateAdmittedIntensive <= datoTil))
RegData <- RegData[indDato,]
save(RegData, file = 'A:/Intensiv/NIRaarsrapp2018.RData')
RegDataAarCSV <- NIRPreprosess(RegData)
write.table(RegDataAarCSV, file = 'A:/Intensiv/NIRaarsrapp2018.csv', row.names = F, col.names=T, 
            fileEncoding = 'UTF-8', sep = ';')


load("A:/Intensiv/NIRaarsrapp2018.Rdata")
PaarorDataH <- PaarorDataH2018
load('A:/Intensiv/PaarorRegData2018.RData')
setwd('P:/Registerinfo og historie/intensiv/aarsrappOff')

#Innkomstmåte (egen fig.) reg/sentLok
NIRFigInnMaate(RegData=RegData, datoFra=datoFra1aar, datoTil = datoTil, 
               grType=1, outfile='InnMaateLokSen.pdf')
NIRFigInnMaate(RegData=RegData, valgtVar='InnMaate', datoFra=datoFra1aar, datoTil = datoTil, 
               grType=3, outfile='InnMaateReg.pdf')

#--------------------------------------- Andeler ----------------------------------

variable <- c('OrganDonationCompletedReasonForNoStatus', 'CerebralCirculationAbolishedReasonForNo',
              'inklKrit','liggetid','InnMaate','NEMS24', 'Nas24','respiratortidNonInv',
                   'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak') #, 'respiratortidInvMoverf')
for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_Ford.pdf')
      NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil, 
                 outfile=outfile)
}

NIRFigAndeler(RegData=RegData, valgtVar='liggetid', dodInt=1, datoFra=datoFra1aar, datoTil=datoTil, 
              outfile='liggetidDod_ford.pdf') 
NIRFigAndeler(RegData=RegData, valgtVar='spesTiltak', datoFra=datoFra1aar, datoTil=datoTil, grType = 3,
              outfile='spesTiltak_ford.pdf') 
#Pårørende
NIRFigAndeler(RegData=PaarorDataH2018, valgtVar='BehandlingHoeflighetRespektMedfoelelse', datoFra=datoFra1aar, datoTil=datoTil, 
              outfile='BehandlingHoeflighetRespektMedfoelelse_Ford.pdf') 
#--------------------------------------- AndelGrVar ----------------------------------
# Reinnleggelser reg/sentlok
# Død innen 30 dager reg/sentLok
# Trakeostomi reg/lokSent


variable <- c('OrganDonationCompletedCirc', 'OrganDonationCompletedStatus', 
              'dod30d', 'dodeIntensiv', 'trakeostomi','reinn')
for (grType in 2:3) {
      for (valgtVar in variable) {
            outfile <- paste0(valgtVar, grType, 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, Ngrense=10,
                            datoTil=datoTil, grType=grType, outfile=outfile)
      }
}
#---------------------AndelTid----------------------------------------------

variable <- c('dod30d', 'liggetidDod')
for (valgtVar in variable){
      outfile <- paste0(valgtVar, '_AndelTid.pdf')
      NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                     outfile=outfile)	
}

#---------------------GjsnTid----------------------------------------------
# Alder, hele landet
# Liggetid, hele landet
# Liggetid, død
# SAPSII

valgtMaal <- 'Med'
variable <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')
               

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, 'MedTid.pdf')
      NIRFigGjsnTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    valgtMaal=valgtMaal, outfile=outfile)
}
NIRFigGjsnTid(RegData=RegData, valgtVar='liggetid', datoFra=datoFra, datoTil=datoTil, 
              valgtMaal=valgtMaal, dodInt=1, outfile='liggetidDod_MedTid.pdf')

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
variable <- 'respiratortidInvUoverf'
variable <- c('alder', 'liggetid', 'respiratortid','NEMS', 'NEMS24', 'Nas24', 
              'respiratortidInvMoverf', 'respiratortidNonInv', 'SAPSII',
              'respiratortidInvUoverf')
for (grType in 2:3) {
      for (valgtVar in variable){ # 
            outfile <- paste0(valgtVar,grType, '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, 
                         grType=grType, datoFra=datoFra1aar, datoTil=datoTil, outfile=outfile) 
      }
      NIRFigGjsnGrVar(RegData=RegData, valgtVar='SMR', grType=grType, 
                      datoFra=datoFra1aar, datoTil=datoTil, outfile=paste0('SMR',grType, '_PrSh.pdf'))
      NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1, 
                      grType=grType, datoFra=datoFra1aar, datoTil=datoTil, 
                      outfile=paste0('liggetidDod',grType,'_MedPerSh.pdf'))
}



NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1, 
                grType=3, datoFra=datoFra1aar, datoTil=datoTil, outfile='liggetidDod3_MedPerSh.pdf')




#-------------------------------Tabeller--------------------------------
#Belegg 
library(xtable)
RegData1aar <- NIRPreprosess(RegData)
Utvalg <- NIRUtvalgEnh(RegData1aar, datoFra = datoFra1aar, datoTil = datoTil)
RegData1aar <- Utvalg$RegData

tabBeleggN <- rbind(
            'Ferdigstilte intensivopphald' = tapply(RegData1aar$PasientID, RegData1aar$ShType, FUN=length), 		
            'Registrerte pasientar' = tapply(RegData1aar$PasientID, RegData1aar$ShType, 
                                             FUN=function(x) length(unique(x))),	
            'Tal intensivdøger' = round(as.numeric(tapply(RegData1aar$liggetid, RegData1aar$ShType, sum, na.rm=T)),0)	
      )
tabBeleggNtot <- cbind(tabBeleggN, rowSums(tabBeleggN))
colnames(tabBeleggNtot) <- c('lokal-/sentral', 'region', 'alle')

xtable(tabBeleggNtot, digits=0, align=c('l', rep('r', ncol(tabBeleggNtot))), 
       caption='Antal opphald og liggedøger i 2018.', label='tab:RegEget')

NIRFigGjsnTid(RegData = RegData1aar, valgtVar = 'NEMS', tidsenhet = 'Mnd', valgtMaal = 'Med', outfile = 'NEMStest.pdf')
library(lubridate)
#Antall opphold
tabDum <- tabAntOpphShMnd(RegData=RegData1aar, datoTil=datoTil, antMnd=12)
table(RegData1aar$ShNavn) #[ ,c('ShNavn' ,'Aar')])
RegData1aar$ShNavn
xtable(table(RegData1aar$ShNavn), align=c('l','r'), #row.names=F,
       caption = 'Tal på registrerte opphald')


#Aktivitet/Nøkkeltall
tabNokkeltall <- tabNokkeltall(RegData=RegData1aar, datoTil=datoTil) #, tidsenhet='Mnd' 
xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
       caption = 'Samla tal på intensivopphald og aktivitet i NIR 2018')



#Fordeling av kjønn per sykehustype og år
RegDataPre <- NIRPreprosess(RegData)
tabShTypeAar <- table(RegDataPre$Aar, RegDataPre$ShType)
tabKj <- table(RegDataPre[RegDataPre$erMann==1 , c('Aar', 'ShType')])
kjLandet <- prop.table(table(RegDataPre[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
colnames(AndelMenn) <- c('Lok./Sentral', 'Region', 'Hele landet')
xtable(AndelMenn, digits=1, align=c('l', rep('r', ncol(AndelMenn))), 
       caption='Andel (prosent) av oppholdene som er menn.', label='tab:KjonnAar')


#--------------------------------------OFFENTLIGGJØRING, 2016-------------------------------------


NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra=datoFra1aar, 
                datoTil=datoTil, grType=1, outfile='Respiratortid_loksent.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra=datoFra1aar, valgtMaal = 'Gjsn',
                datoTil=datoTil, grType=3, outfile='test.png') #Respiratortid_region_Fig2bNy.pdf')

NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra=datoFra1aar, 
                   datoTil=datoTil, grType=1, outfile='Reinnlegging_loksent.pdf')








