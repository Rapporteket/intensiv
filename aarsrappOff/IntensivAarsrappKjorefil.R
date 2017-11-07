

#----------------------Figurer til Årsrapport 2016---------------------------
rm(list=ls())
library(intensiv)
datoFra <- '2012-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2016-12-31'	# standard: 3000-01-01
RegData <- read.table('A:/Intensiv/MainFormDataContract2017-11-07.csv', sep=';', stringsAsFactors=FALSE, 
                      header=T, encoding = 'UTF-8')	
setwd('C:/ResultattjenesteGIT/intensiv/aarsrappOff/')



#Innkomstmåte (egen fig.) reg/sentLok
NIRFigInnMaate(RegData=RegData, datoFra='2016-01-01', datoTil = datoTil, 
               grType=1, outfile='InnMaateLokSen.pdf')
NIRFigInnMaate(RegData=RegData, valgtVar='InnMaate', datoFra='2016-01-01', datoTil = datoTil, 
               grType=3, outfile='InnMaateReg.pdf')

#--------------------------------------- Andeler ----------------------------------
#Nyreerstattende behandling (sjekk aksetittel)
nyreBeh
respiratortidInv

variable <- c('inklKrit','liggetid','InnMaate','NEMS24', 'Nas24','respiratortidNonInv','respiratortidInv',
                  'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak', 'respiratortidInvMoverf')
for (valgtVar in 'respiratortidInvMoverf') {
      outfile <- paste0(valgtVar, '_Ford.pdf')
      NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra='2016-01-01', datoTil=datoTil, 
                 outfile=outfile)
}
NIRFigAndeler(RegData=RegData, valgtVar='liggetid', dodInt=1, datoFra='2016-01-01', datoTil=datoTil, 
              outfile='liggetidDod_ford.pdf') 


#--------------------------------------- AndelGrVar ----------------------------------
# Reinnleggelser reg/sentlok
# Død innen 30 dager reg/sentLok
# Trakeostomi reg/lokSent


variable <- c('dod30d', 'dodeIntensiv', 'trakeostomi')
for (grType in 2:3) {
      for (valgtVar in variable) {
            outfile <- paste0(valgtVar, grType, 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra='2016-01-01', 
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
variable <- c('alder', 'liggetid', 'SAPSII')		
for (valgtVar in variable) {
      outfile <- paste0(valgtVar, 'MedTid.pdf')
      NIRFigGjsnTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    valgtMaal=valgtMaal, outfile=outfile)
}
NIRFigGjsnTid(RegData=RegData, valgtVar='liggetid', datoFra=datoFra, datoTil=datoTil, 
              dodInt=dodInt, valgtMaal=valgtMaal, dodInt=1, outfile='liggetidDod_MedTid.pdf')

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
valgtVar <- 'alder'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas24'
#Nye: respiratortidInvMoverf, respiratortidInvUoverf, respiratortidNonInv
variable <- c('alder', 'liggetid', 'respiratortid','NEMS', 'NEMS24', 'Nas24', 
              'respiratortidInvMoverf', 'respiratortidNonInv', 'SAPSII')
for (grType in 2:3) {
      for (valgtVar in variable){ # 
            outfile <- paste0(valgtVar,grType, '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, 
                         grType=grType, datoFra=datoFra, datoTil=datoTil, outfile=outfile) 
      }
}

NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1, 
                grType=2, datoFra=datoFra, datoTil=datoTil, outfile='liggetidDod2_MedPerSh.pdf')

NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1, 
                grType=3, datoFra=datoFra, datoTil=datoTil, outfile='liggetidDod3_MedPerSh.pdf')




#--------------------------------------OFFENTLIGGJØRING, 2016-------------------------------------


NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', 
                datoTil='2016-12-31', grType=1, outfile='Respiratortid_loksent_Fig2aNy.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', valgtMaal = 'Gjsn',
                datoTil='2016-12-31', grType=3, outfile='test.png') #Respiratortid_region_Fig2bNy.pdf')

NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra='2016-01-01', 
                   datoTil='2016-12-31', grType=1, outfile='Reinnlegging_loksent_Fig3aNy.pdf')
NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra='2011-01-01', 
                   datoTil='2017-12-31', grType=0, outfile='test.png') #Reinnlegging_region_Fig3bNy.pdf')








#-------------------- Årsrapport, 2015---------------------
rm(list=ls())
library(intensiv)
reshID=112044
datoFra <- '2011-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2015-12-31'	# standard: 3000-01-01
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
enhetsUtvalg <- 0	#0-5
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
RegData <- read.table('C:/Registre/NIR/data/Main2016-09-27.csv', sep=';', header=T, encoding = 'UTF-8')	
setwd('C:/ResultattjenesteGIT/intensiv/Aarsrapp/')

#--------------------------------------- Andeler ----------------------------------
alder, alle, 2015
InnMaate, alle, 2015
liggetid, 2015
liggetid, døde, 2015
liggetid, levende, 2015
Nas, 2015
NEMS/døgn, 2015
NEMS/døgn, levende, 2015
NEMS/døgn, døde, 2015
respiratortid, 2015
respiratortid, døde, 2015
respiratortid, levende, 2015
SAPSII, 2015
SAPSII, døde, 2015
SAPSII, levende, 2015

datoFra <- '2015-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2015-12-31'	# standard: 3000-01-01
for (dodInt in c('',0,1)) {
	for (valgtVar in c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS', 'Nas', 'InnMaate')) {
	outfile <- paste(valgtVar, dodInt, 'Fordeling2015.pdf', sep='')
	NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
		datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
		reshID=reshID, enhetsUtvalg=enhetsUtvalg)
}}

#--------------------------------------- AndelGrVar ----------------------------------
respStotte, lok/sentral, 2015
respStotte, region, 2015
innMaate, lok/sentral, 2015
innMaate, region, 2015
dodeIntensiv, lok/sentral, 2015
dodeIntensiv, region, 2015
dod30d, lok/sentral, 2015
dod30d, region, 2015
alder_over80, lok/sentral, 2015
alder_over80, region, 2015
alder_u18, lok/sentral, 2015
alder_u18, region, 2015
reinn, lok/sentral, 2015
reinn, region, 2015

dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
datoFra <- '2015-01-01'	 
datoTil <- '2015-12-31'	
valgtVar <- 'innMaate'
grType <- 1

for (valgtVar in c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'innMaate', 
				'respStotte', 'reinn')) {
	for (grType in  c(1,3)) {
	outfile <- paste0(valgtVar, 'ShGr', grType, 'PerEnhet2015.pdf')
      NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
      	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
      	grType=grType)
}}
#---------------------AndelTid----------------------------------------------
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
datoFra <- '2011-01-01'
datoTil <- '2015-12-31'
for (valgtVar in c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'liggetidDod', 
                   'respiratortidDod', 'respStotte', 'reinn')) {
outfile <- paste0(valgtVar, 'AndelTid_alle.pdf')
NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt, 
		reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg)
}
#---------------------GjsnTid----------------------------------------------
Gjsn alder, 11-15
Median alder, 11-15
Gjsn liggetid, 11-15
Median liggetid, 11-15
Gjsn respiratortid, 11-15
Median respiratortid, 11-15
Gjsn SAPSII, 11-15
Gjsn liggetid, 11-15, døde
Gjsn respiratortid, alle 11-15, døde

dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
enhetsUtvalg <- 0	#0-5

for (valgtVar in c('alder', 'liggetid', 'respiratortid', 'SAPSII')) {
      for (valgtMaal in c('Med', 'Gjsn')) {
            outfile <- paste0(valgtVar, valgtMaal, 'Tid.pdf')
            NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
              erMann=erMann, InnMaate=InnMaate, dodInt=dodInt,
              valgtMaal=valgtMaal,enhetsUtvalg=enhetsUtvalg, reshID=reshID)
      }
}
dodInt <- 1	# 0-i live, 1 -død, standard: alle (alle andre verdier)
valgtMaal <- 'Gjsn'
for (valgtVar in c('liggetid', 'respiratortid')) {
            outfile <- paste0(valgtVar, 'Tilst',dodInt, 'SentrMaalTid.pdf')
            NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                          erMann=erMann, InnMaate=InnMaate, dodInt=dodInt,
                          valgtMaal=valgtMaal,enhetsUtvalg=enhetsUtvalg, reshID=reshID)
      }

#--------------------------------------- SENTRALMÅL per enhet----------------------------------
alder, median lok/sentral, 2015
alder, median, region, 2015
alder, gjsn lok/sentral, 2015
alder, gjsn, region, 2015
liggetid, median lok/sentral, 2015
liggetid, median, region, 2015
liggetid, gjsn lok/sentral, 2015
liggetid, gjsn, region, 2015
Nas, gjsn, 2015
Nas, median, 2015
NEMS24, gjsn lok/sentral, 2015
NEMS24, gjsn, region, 2015
NEMS24, median lok/sentral, 2015
NEMS24, median, region, 2015
NEMS/opph, gjsn lok/sentral, 2015
NEMS/opph, gjsn, region, 2015
NEMS/opph, median lok/sentral, 2015
NEMS/opph, median, region, 2015
respiratortid, median lok/sentral, 2015
respiratortid, median, region, 2015
respiratortid, gjsn lok/sentral, 2015
respiratortid, gjsn, region, 2015
SAPSII, gjsn lok/sentral, 2015
SAPSII, gjsn, region, 2015
SMR, alle, 2015
SMR, lok/sentral, 2015
SMR, region, 2015

dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
datoFra <- '2015-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2015-12-31'	# standard: 3000-01-01
for (valgtVar in c('SMR', 'alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS', 'NEMS24','Nas')) {
      for (grType in  c(1,3,99)) {
            for (valgtMaal in c('Med', 'Gjsn')) {
            outfile <- paste0(valgtVar, 'ShGr', grType, valgtMaal,'2015.pdf')
            NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
                            grType=grType, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
                            erMann=erMann, outfile=outfile) 
            }}}
