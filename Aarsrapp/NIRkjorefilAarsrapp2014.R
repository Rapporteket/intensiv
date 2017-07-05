#Alle figurer som er standard i årsrapporten.

rm(list=ls())

NIRdata <- read.table('C:/Registre/NIR/data/NIR2013.csv', sep=';', header=T)	#, fileEncoding = "UTF-8")
source("C:/Registre/NIR/trunk/AndelSoyler/NIRFigAndelSoyler.R", encoding="UTF-8")	#source("FigInnMaate.R", encoding="UTF-8")
source("C:/Registre/NIR/trunk/MeanMed/NIRFigMeanMed.R", encoding="UTF-8")

setwd('C:/Registre/NIR/Aarsrapp/2013')
datoFra <- '2013-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2013-12-31'	# standard: 3000
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog,

for (valgtVar in c('InnMaate', 'Reinn')) {
	for (ShType in c('region', 'sentral', 'lokal', 'alle')) {
		outfile <- paste(valgtVar, '_',ShType, '.pdf', sep='')
#		AndelSoyler(valgtVar=valgtVar, ShType=ShType, 	#minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, 
#			datoFra=datoFra, datoTil=datoTil, outfile=outfile) 
		AndelSoyler(RegData=NIRdata, valgtVar=valgtVar, libkat=libkat, ShType=ShType, datoFra=datoFra, datoTil=datoTil, 
			InnMaate=InnMaate, outfile=outfile) #minald=minald, maxald=maxald, dodInt=dodInt,erMann=erMann, 
	}
}

for (valgtVar in c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS','SMR')) {
	for (valgtMaal in c('Med','')) {
		for (ShType in c('region', 'sentral', 'lokal', 'alle')) {
			outfile <- paste(valgtVar, '_',ShType, valgtMaal, '.pdf', sep='')
#			MeanMed(valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, ShType=ShType, 
#				InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, 
#				outfile=outfile) 
			MeanMed(RegData=NIRdata, valgtVar=valgtVar, valgtMaal=valgtMaal, libkat=libkat, minald=minald, maxald=maxald, ShType=ShType, 
					InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, outfile=outfile) 
		}
	}
}

dodInt <- 1	# 0-i live, 1 -død, standard: alle (alle andre verdier)
valgtVar <- 'liggetid'	#'SMR', liggetid, respiratortid,  SAPSII, 'NEMS'
for (valgtMaal in c('Med','')) {
	for (ShType in c('region', 'sentral', 'lokal', 'alle')) {
		outfile <- paste(valgtVar, '_',ShType, valgtMaal, '_dod.pdf', sep='')
			MeanMed(RegData=NIRdata, valgtVar=valgtVar, valgtMaal=valgtMaal, libkat=libkat, minald=minald, maxald=maxald, ShType=ShType, 
					InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, outfile=outfile) 
	}
}


