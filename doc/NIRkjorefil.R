#Til analysebok:
#Liggetid og respiratortid for pasienter som ikke er overført mellom sykehus.



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

NIRdata <- read.table(file='C:/Registre/NIR/data/Main2017-03-29.csv', header=T, sep=';',encoding = 'UTF-8')
RegData <- NIRdata
#save(RegData, file='C:/Registre/NIR/data/Main2017-03-29.Rdata')
load("C:/Registre/NIR/data/Main2017-03-29.Rdata") #RegData
#RegData <- NIRdata[sample(1:dim(NIRdata)[1],10000),]
#save(RegData, file='C:/Registre/NIR/data/NIRdata10000.Rdata')
load("C:/Registre/NIR/data/NIRdata10000.Rdata") #RegData

#-----------------------------------Datasett til kvalitetsindikatorer---------
library(intensiv)

datoFra <- '2016-01-01'
tilleggsVar <- c('Aar', 'Kvartal', 'erMann', 'ShNavn', 'ShType', 'Alder')
valgtVar <- 'reinn'  #reinn, respiratortid
RegData01Off(RegData, valgtVar=valgtVar, datoFra = datoFra, tilleggsVar=tilleggsVar, hentData=0)

aar <- 0
grType <- 99
grVar <- 'ShNavn'
InnMaate <- 99
erMann <- '' 
aldGr  <- 0

load(paste0('C:/Registre/NIR/data/RegData01', valgtVar, '.Rdata'))

DataTilbake <- NIRAndelerGrVarOff(RegData=RegData, valgtVar=valgtVar, aar=aar, grType=grType, grVar='ShNavn', InnMaate=InnMaate, 
                               erMann=erMann, aldGr=aldGr, hentData=0, outfile='', lagFig=1) 

DataTilbake <- NIRAndelerGrVarOff(RegData=RegData, hentData=0, outfile='', lagFig=1) 

#-------------------------------------- Parametre ----------------------------------------------------
library(intensiv)
setwd("c:/ResultattjenesteGIT/Intensiv/")
reshID=112044
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
valgtMaal = 'Gjsn' #'Med' = median. 'Gjsn' = gjennomsnitt. Alt annet gir gjennomsnitt
datoFra <- '2015-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2016-12-31'	# standard: 3000
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
overfPas <- ''    #Overført under pågående intensivbehandling?	1 = Nei, 2 = Ja
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
grVar <- 'ShNavn'
enhetsUtvalg <- 1	#0-5
#Parameter for evt. kvalitetsmål? angis i Tilrettelegging

#--------------------------------------- Ny struktur basert på grVar? ----------------------------------
#Prioriter kvalitetsindikatorene: reinn, SMR, median innleggelse (se årsrapport)
#Median respiratortid < 2,5 døger -> Kan vi vise andel med respiratortid <2,5døgn og sette grense på 50%?
#Standardisert mortalitetsratio (SMR) < 0,7 (etter ikkje-justert alvorsskåre) 
#Andel reinnlegging til intensiv i løpet av 72 timar < 4% av opphalda (def. endret 2016)
#Alle disse vises per sykehus for et gitt tidsintervall (siste 12 mnd?)
#I tillegg kanskje vi skal vise utvikling over tid for valgt sykehus og sykehustype?

#Forslag til def av grVar hvis IKKE kjernen skal være figurtypen
grVar <- 
#      0 - (Søyle) fordelingsfigur for den aktuelle variabelen.
#      1 - (Søyle) fordelingsfigur for flere variable, dvs. andel av mange variable samlet.
#      (med 0/1 erstatter grVar "flerevar")
#      aar - AndelTid -> linjeplott
#      shus - (Søyle) AndelerGrVar, GjsnGrVar - hvordan skille disse?
      

#--------------------------------------- Andeler ----------------------------------
valgtVar <- 'Nas24'	#'alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS24', 'Nas24', 'InnMaate'
outfile <- '' #paste('Ford_',valgtVar, '.pdf', sep='')

Utdata <- NIRAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
                        datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
                        hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg, lagFig=1)

#NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
#	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
#	hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg)


variable <- c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS24', 'Nas24', 'InnMaate')
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_Ford.png')
	NIRAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	                     datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	                     hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg, lagFig=1)
}

#--------------------------------------- AndelGrVar ----------------------------------
grVar <- 'ShNavn'
valgtVar <- 'reinn'	#alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'innMaate', 
                        #'respStotte', 'reinn
outfile <- '' #paste0(valgtVar, 'GrVar.png')

NIRAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
                datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
                grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1)

NIRAndelerGrVar(RegData=OffDataKvalInd, valgtVar='reinn', aar=2015, erMann='', outfile='', 
               grVar='ShNavn', hentData=0, preprosess=0, lagFig=1)

#NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
#	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
#	grType=grType)
variable <- c('alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', #'innMaate', 
      'respStotte', 'reinn')
for (valgtVar in variable) {
		outfile <- paste0(valgtVar, 'GrVar.png')
		NIRAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
		                datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
		                grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1)
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
valgtVar <- 'alder'	#'alder', 'liggetid', 'respiratortid', 'SAPSII', 
outfile <- ''	#paste0(valgtVar, '.png')

NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
		              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)

NIRAndelerGrVar(grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1)
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

valgtMaal <- 'Med'
valgtVar <- 'respiratortid'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas'
outfile <- '' #paste0(valgtVar, 'MM.png')#,grType

NIRGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
                grType=grType, grVar=grVar, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
                erMann=erMann, outfile=outfile) 


for (valgtVar in c('alder', 'liggetid', 'respiratortid','NEMS' ,'SAPSII', 'SMR')){ # 
      outfile <- paste0(valgtVar, 'GjsnGrVar.pdf')
      NIRGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
                   grType=grType, grVar=grVar, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
                   erMann=erMann, outfile=outfile) 
      #NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, grType=grType, 
       #               InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, 
        #              outfile=outfile) 
}

#--------------------------------------- FORDELING - tatt vekk ----------------------------------

Fordeling(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald, grType=grType, 
          InnMaate=InnMaate, erMann=erMann, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, outfile=outfile) 
#valgtVar in c('alder', 'liggetid', 'respiratortid', 'SAPSII', 'NEMS')) {




