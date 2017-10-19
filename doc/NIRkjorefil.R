#Til analysebok:
#Liggetid og respiratortid for pasienter som ikke er overført mellom sykehus.

#Alle off.farger:

      #c6dbef #6baed6 #4292c6 #2171b5 #084594 #000059 #FF7260 #4D4D4D #737373 #A6A6A6 #DADADA
blaa <- c('#084594','#2171b5','#4292c6','#6baed6','#c6dbef')  #Mørk til lys																# Fem blaatoner
graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
kontrast <- '#FF7260'; moerkeblaa <- '#000059'                																# Spesialfarger

#--------------------------------------Kvalitetskontroll - ikke operativ-----------------------------------
rm(list=ls())
library(knitr)
setwd('C:/registre/NIR/trunk/KvalKtrAvData') 

aggregate(NIRdata$PatientInRegistryKey, by=list(NIRdata$ShNavn, NIRdata$ShTypeTxt), FUN=length)
aggregate(NIRdata$DaysAdmittedIntensiv, by=list(NIRdata$ShNavn, NIRdata$ShTypeTxt), FUN=length)

#--------------------------------------SAMLERAPPORT-----------------------------------
rm(list=ls())
library(knitr)
library(intensiv)
library(tools)	#texi2pdf

setwd('C:/ResultattjenesteGIT/intensiv/inst/') 
reshID <- 112044 #102090 Ahus, 112044 Haukeland
#knit(input, output = NULL, tangle = FALSE, text = NULL, envir = parent.frame())
NIRdata <- RegData
knit('NIRSamleRapp.Rnw')
texi2pdf(file='NIRSamleRapp.tex')


#NIRSamleRapp for hver enkelt enhet (alle)
#for (reshID in AlleResh ) {
#	knit('NIRSamleRapp.Rnw')
#	texi2pdf(file='NIRSamleRapp.tex')
#	file.rename('NIRSamleRapp.pdf', paste0('NIRSamleRapp', reshID, '.pdf'))	#list.files(pattern="water_*.img", paste0("water_", 1:700))
#}


#-------------------------------------LASTE DATA-----------------------------------------------
rm(list=ls())

dato <- '2017-09-18'
dataKat <- 'A:/Intensiv/'
fil <- paste0(dataKat,'MainFormDataContract',dato)
#NIRdata <- read.table(file=paste0(fil,'.csv'), header=T, stringsAsFactors=FALSE, sep=';',encoding = 'UTF-8')
#RegData <- NIRdata
load(paste0(fil,".Rdata")) #RegData 2017-09-18
#save(RegData, file=paste0(fil,'.Rdata'))
#RegData <- RegData[which(as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d")>= '2014-01-01'), ]
#RegData <- NIRdata[sample(1:dim(NIRdata)[1],10000),]
#save(RegData, file=paste0(dataKat,'NIRdata10000.Rdata'))
load(paste0(dataKat,"NIRdata10000.Rdata")) #RegData, juli 2017
library(intensiv)



#-----------------------------------Lage datasett til kvalitetsindikatorer---------
library(intensiv)

valgtVar <- 'respiratortidInv'  #reinn, respiratortidInv
datoFra <- '2016-01-01'
datoTil <- '2016-12-31'
tilleggsVar <- c('Aar', 'Kvartal', 'ShNavn', 'ShType', 'Alder')
rand <- 1
RegData01Off(RegData, valgtVar=valgtVar, datoFra = datoFra, datoTil, tilleggsVar=tilleggsVar, 
             hentData=0, rand=rand)

#-------------------------------Resultater for off.kval.ind.----------------------------------------
datoFra <- '2011-01-01'
datoTil <- '2017-12-31'
aar <- 0
grType <- 99
grVar <- 'ShNavn'
InnMaate <- 99
aldGr  <- 0
tidsenhet <- 'Kvartal'
outfile <- ''
valgtVar <- 'respiratortidInv'  #reinn, respiratortidInv
outfile <- '' #paste0('OffRand', valgtVar, '.pdf')
#Laste offdata
filnavn <- paste0('NIRdata01', valgtVar)
load(paste0(dataKat, filnavn, '.Rdata'))
RegData <- NIRdata01reinn #NIRdata01respiratortid #NIRdata01reinn


DataTilbake <- NIRAndelerGrVar(RegData=RegData, valgtVar=valgtVar, aar=aar, grType=grType, 
                               grVar='ShNavn', InnMaate=InnMaate, erMann=erMann, hentData=0, 
                               outfile=outfile, lagFig=1, offData=1) #aldGr=aldGr, 

DataTilbake <- NIRAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                           tidsenhet = tidsenhet,minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, 
                           dodInt=dodInt, reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg, 
                           lagFig = 1, offData=1)	
#aar=0, grType=grType )

ind <- which(RegData$InvasivVentilation>0)
Resp <- tapply(RegData$respiratortid[ind],RegData$Aar[ind], FUN=sum)
Inv <- tapply(RegData$InvasivVentilation[ind],RegData$Aar[ind], FUN=sum)
NonInv <- tapply(RegData$NonInvasivVentilation[ind],RegData$Aar[ind], FUN=sum)

#-------------------------------------- Parametre ----------------------------------------------------
library(intensiv)
setwd("c:/ResultattjenesteGIT/Intensiv/")
reshID=112044
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
valgtMaal = 'Gjsn' #'Med' = median. 'Gjsn' = gjennomsnitt. Alt annet gir gjennomsnitt
datoFra <- '2016-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2016-12-31'	# standard: 3000
aar <- 0
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
overfPas <- ''    #Overført under pågående intensivbehandling?	1 = Nei, 2 = Ja
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
enhetsUtvalg <- 0	#0-5
grVar <- 'ShNavn'
tidsenhet <- 'Aar'
enhetsUtvalg <- 0	#0-5
medKI <- 0
offData <- 0
#Parameter for evt. kvalitetsmål? angis i Tilrettelegging


NIRFigInnMaate (RegData=RegData, valgtVar='InnMaate', minald=0, maxald=130, datoTil = datoTil,
                           grType=99, grVar='ShNavn', InnMaate=99, dodInt='', outfile='')

#--------------------------------------- Ny struktur basert på grVar? ----------------------------------
#Prioriter kvalitetsindikatorene: reinn, SMR, median innleggelse (se årsrapport)
#Median respiratortid < 2,5 døger -> Kan vi vise andel med respiratortid <2,5døgn og sette grense på 50%?
#Standardisert mortalitetsratio (SMR) < 0,7 (etter ikkje-justert alvorsskåre) 
#Andel reinnlegging til intensiv i løpet av 72 timar < 4% av opphalda (def. endret 2016)
#Alle disse vises per sykehus for et gitt tidsintervall (siste 12 mnd?)
#I tillegg kanskje vi skal vise utvikling over tid for valgt sykehus og sykehustype?


#--------------------------------------- Andeler ----------------------------------
valgtVar <- 'alder'	#'alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS24', 'Nas24', 'InnMaate'
                              #Nye: PrimaryReasonAdmitted, inklKrit, respiratortidNonInv, respiratortidInv
                              #nyreBeh, nyreBehTid, ExtendedHemodynamicMonitoring, isolering, isoleringDogn, 
                              #spesTiltak
outfile <- paste0(valgtVar,'_Ford', '.png')
grType <- 0
enhetsUtvalg <- 3

NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
                     datoTil=datoTil, grType=grType, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
                     hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg, lagFig=1)
# Utdata <- NIRAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
#                         datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
#                         hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg, lagFig=1)

variable <- c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS24', 'Nas24', 'InnMaate')
variable <- c('PrimaryReasonAdmitted', 'inklKrit', 'respiratortidNonInv', 'respiratortidInv', 'nyreBeh',
              'nyreBehTid', 'ExtendedHemodynamicMonitoring', 'isolering', 'isoleringDogn')
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_Ford.png')
	NIRAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	                     datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	                     hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg, lagFig=1)
}

#--------------------------------------- AndelGrVar ----------------------------------
grVar <- 'ShNavn'
valgtVar <- 'respiratortidInvMoverf'	#alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'innMaate', 
                        #respiratortid, 'respStotte', 'reinn
                        #Nye: trakeostomi, trakAapen, respiratortidInv, nyreBeh, ExtendedHemodynamicMonitoring,
                        #ExtendedHemodynamicMonitoringPA, isolering
outfile <- '' #paste0(valgtVar, 'Reinnlegging_region_Fig3bNy.pdf')

#NIRAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
#                datoTil=datoTil, aar=0, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
#                grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1, offData = offData)

NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
                datoTil=datoTil, aar=0, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
                grType=3, grVar=grVar, hentData=0, preprosess=1, lagFig=1, offData = offData)

variable <- c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', #'innMaate', 
      'respStotte', 'reinn')
variable <- c('trakeostomi', 'trakAapen', 'respiratortidInv', 'nyreBeh', 'ExtendedHemodynamicMonitoring',
                        'ExtendedHemodynamicMonitoringPA', 'isolering')
for (valgtVar in variable) {
		outfile <- paste0(valgtVar, 'PrSh.pdf')
		NIRAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
		                datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
		                grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1)
                  }

#---------------------AndelTid----------------------------------------------
tidsenhet <- 'Aar'
valgtVar <- 'alder_over80'	#'alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'liggetidDod', 
                        #'respiratortidDod', 'respStotte', 'reinn',
                        #'UT: respiratortid, 
                        #Ny: respiratortidInvMoverf, respiratortidInvUoverf
outfile <- '' #paste0(valgtVar, '.pdf')

NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil='2017-12-01', tidsenhet = tidsenhet,
		minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt, 
		reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg, lagFig = 1, offData=offData)	
#aar=0, grType=grType )

variable <- c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'liggetidDod', 
              'respiratortidDod', 'respStotte', 'reinn', 'SMR')
for (valgtVar in variable){
      outfile <- paste0(valgtVar, '_AndelTid.png')
      NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                     minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt, 
                     reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg)	
}

#---------------------GjsnTid----------------------------------------------
valgtVar <- 'respiratortidInv'	#'alder', 'liggetid', 'respiratortid', 'SAPSII', 
                        #Nye: respiratortidInv, respiratortidNonInv
outfile <- paste0(valgtVar, 'GjsnTid.pdf')

NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
		              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)

#NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
#                    erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
#		              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
		
variable <- c('alder', 'liggetid', 'respiratortid', 'SAPSII')		

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, 'GjsnTid.png')
      NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
              erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
}


#--------------------------------------- SENTRALMÅL per enhet----------------------------------

valgtMaal <- 'Gjsn'
valgtVar <- 'SMR'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas24'
                        #Nye: respiratortidInvMoverf, respiratortidInvUoverf, respiratortidNonInv
grType <- 0
outfile <- '' #paste0(valgtVar, 'Respiratortid_sentr_Fig2aNy.pdf')#,grType
NIRFigGjsnGrVar(RegData=RegData, medKI = 1 ,valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
             grType=grType, grVar=grVar, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
             erMann=erMann, outfile=outfile) 
 # NIRGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
 #              grType=grType, grVar=grVar, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
 #              erMann=erMann, outfile=outfile) 


for (valgtVar in c('alder', 'liggetid', 'respiratortid','NEMS' ,'SAPSII', 'SMR')){ # 
      outfile <- paste0(valgtVar, 'GjsnGrVar.pdf')
      NIRGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
                   grType=grType, grVar=grVar, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
                   erMann=erMann, outfile=outfile) 
      #NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, grType=grType, 
       #               InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, 
        #              outfile=outfile) 
}


#--------------------------------------OFFENTLIGGJØRING-------------------------------------


NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', 
                datoTil='2016-12-31', grType=1, outfile='Respiratortid_loksent_Fig2aNy.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', 
                                datoTil='2016-12-31', grType=3, outfile='Respiratortid_region_Fig2bNy.pdf')
                                
NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra='2016-01-01', 
                   datoTil='2016-12-31', grType=1, outfile='Reinnlegging_loksent_Fig3aNy.pdf')
NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra='2016-01-01', 
                   datoTil='2016-12-31', grType=3, outfile='Reinnlegging_region_Fig3bNy.pdf')










#--------------------------------------- FORDELING - tatt vekk ----------------------------------

Fordeling(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald, grType=grType, 
          InnMaate=InnMaate, erMann=erMann, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, outfile=outfile) 
#valgtVar in c('alder', 'liggetid', 'respiratortid', 'SAPSII', 'NEMS')) {

#-----------------------------------------TESTING--------------------------------
#Sjekk av sammefallende sykehusnavn: 
#names(table(RegData$PatientTransferredFromHospitalName))[is.na(match(names(table(RegData$PatientTransferredFromHospitalName)), 
#                                                       names(table(RegData$ShNavn))))]
#  [1] ""                      "Annet sykehus i Norge" "Haukel. Brannsk "      "RH Postop "            "RH Thorax 1 "         
#[6] "RH Thorax 2 "          "St. Olav Barneint "    "St. Olav Thorax "      "Tromsø Postop "        "Utlandet" 

From <- names(table(RegData$PatientTransferredFromHospital))
To <- names(table(RegData$PatientTransferredToHospital))
Resh <- names(table(RegData$ReshID))
From[-c(0,which(From %in% Resh))]
To[-c(0,which(To %in% Resh))]

table(RegData$PatientTransferredFromHospital)[
      which(names(table(RegData$PatientTransferredFromHospital)) %in% From[-which(From %in% c(0,Resh))])]
table(RegData$PatientTransferredToHospital)[
      which(names(table(RegData$PatientTransferredToHospital)) %in% To[-which(To %in% c(0,Resh))])]
