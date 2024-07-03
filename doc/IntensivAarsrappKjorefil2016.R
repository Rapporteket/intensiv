

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






