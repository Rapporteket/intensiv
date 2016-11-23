
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
knit('NIRSamleRapp.Rnw')
texi2pdf(file='NIRSamleRapp.tex')


#NIRSamleRapp for hver enkelt enhet (alle)
#for (reshID in AlleResh ) {
#	knit('NIRSamleRapp.Rnw')
#	texi2pdf(file='NIRSamleRapp.tex')
#	file.rename('NIRSamleRapp.pdf', paste0('NIRSamleRapp', reshID, '.pdf'))	#list.files(pattern="water_*.img", paste0("water_", 1:700))
#}


#Ta med med/uten overføringer som valg? JA

#-------------------------------------LASTE DATA-----------------------------------------------
rm(list=ls())

NIRdata <- read.table(file='C:/Registre/NIR/data/Main2016-09-27.csv', header=T, sep=';',encoding = 'UTF-8')
#RegData <- NIRdata[sample(1:dim(NIRdata)[1],10000),]
#save(RegData, file='C:/Registre/NIR/data/NIRdata10000.Rdata')
load("C:/Registre/NIR/data/NIRdata10000.Rdata") #RegData

#-------------------------------------- Parametre ----------------------------------------------------
library(intensiv)
setwd("c:/ResultattjenesteGIT/Intensiv/")
reshID=112044
minald <- 10 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
valgtMaal = '' #'Med' = median. Alt annet gir gjennomsnitt
datoFra <- '2011-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2015-12-31'	# standard: 3000
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
overfPas <- ''    #Overført under pågående intensivbehandling?	1 = Nei, 2 = Ja
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
enhetsUtvalg <- 1	#0-5

#--------------------------------------- Andeler ----------------------------------
variable <- c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS', 'Nas', 'InnMaate')
valgtVar <- 'alder'	#'alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS', 'Nas', 'InnMaate'
outfile <- ''	#paste('Ford_',valgtVar, '.pdf', sep='')

NIRAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg)
#NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
#	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
#	hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg)


for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_and.png')
#figurfunksjon
	}

#--------------------------------------- AndelGrVar ----------------------------------
valgtVar <- 'dod30d'	#alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'innMaate', 
                        #'respStotte', 'reinn
outfile <- paste0(valgtVar, 'GrVar.png')

NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	grType=grType)


variable <- c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'innMaate', 
      'respStotte', 'reinn')
for (valgtVar in variable) {
		outfile <- paste0(valgtVar, 'GrVar3.png')
		NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
		                   datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
		                   grType=grType)
                  }

		
		
#---------------------AndelTid----------------------------------------------
valgtVar <- 'liggetidDod'	#'alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'liggetidDod', 
                        #'respiratortidDod', 'respStotte', 'reinn', 'SMR'
outfile <- paste0(valgtVar, '.png')

NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt, 
		reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg)	

variable <- c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'liggetidDod', 
              'respiratortidDod', 'respStotte', 'reinn', 'SMR')
for (valgtVar in variable){
      outfile <- paste0(valgtVar, '_AndelTid.png')
      NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                     minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt, 
                     reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg)	
}

#---------------------GjsnTid----------------------------------------------
valgtVar <- 'SAPSII'	#'alder', 'liggetid', 'respiratortid', 'SAPSII', 
outfile <- ''	#paste0(valgtVar, '.png')

NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
		              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
		
variable <- c('alder', 'liggetid', 'respiratortid', 'SAPSII')		

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, 'GjsnTid.png')
      NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
              erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
}


#--------------------------------------- SENTRALMÅL per enhet----------------------------------

valgtVar <- 'liggetid'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas'
outfile <- paste0(valgtVar, 'MM.png')#,grType

NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
                grType=grType, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
                erMann=erMann, outfile=outfile) 



for (valgtVar in c('SMR', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS')){
      outfile <- paste0(valgtVar, 'GjsnGrVar.pdf')
      NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, grType=grType, 
                      InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, 
                      outfile=outfile) 
}

#--------------------------------------- FORDELING - tatt vekk ----------------------------------

Fordeling(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald, grType=grType, 
          InnMaate=InnMaate, erMann=erMann, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, outfile=outfile) 
#Liggetid og respiratortid for pasienter som ikke er overført mellom sykehus.
#valgtVar in c('alder', 'liggetid', 'respiratortid', 'SAPSII', 'NEMS')) {


