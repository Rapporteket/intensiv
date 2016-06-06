


query <- "SET character_set_results = latin1;"	#utf8;"	#		
tmp <- dbGetQuery(con, query)
query <- 'select 
	AgeAdmitted AS alder,
	FreshOrganizationalUnitId AS AvdID, 
	date(DateAdmittedIntensive) AS datoinn,
	DaysAdmittedIntensiv AS liggetid,
	Respirator AS respiratortid,
	TransferredStatus AS Overf,
	Saps2Score,
	Saps2ScoreNumber AS SAPSII,
	NEMS,
	TypeOfAdmission AS InnMaate,
	ShType,
	ShTypeTxt,
	ShusNr,
	ShNavn,
	DischargedHospitalStatus,
	DischargedIntensiveStatus,
 Nas
Organisasjon, 
OrgReshID, 
PatientTransferredFromHospital, 
PatientTransferredToHospital,
ReAdmitted

FROM dump as d, resh as r 
WHERE
	(d.FreshOrganizationalUnitId = r.AvdReshID)'

NIRdata <- dbGetQuery(con, query)
dbDisconnect(con)

#----------------------------------------------------------------------------------------
ENDRINGER, kommentarer 7.feb. 2014 ->
#----------------------------------------------------------------------------------------

Variable i bruk per feb.2014:
	Endre (i Talend el tilsvarende):
	Fra dumptabell:
	FreshOrganizationalUnitId AS AvdID, 
	date(DateAdmittedIntensive) AS datoinn,
	DaysAdmittedIntensiv AS liggetid,
	Respirator AS respiratortid,
	TransferredStatus AS Overf,
	TypeOfAdmission AS InnMaate,	#NB: Brukt TypeOfAdmission i NIRLibUtvalg
	Saps2ScoreNumber AS SAPSII,
	(AgeAdmitted AS alder - trenger ikke endre)
	DischargedIntensiveStatus, Saps2Score, NEMS, 
	Fra reshtabell: r.ShType, r.ShTypeTxt, r.ShNavn,
		#Innhold i ShType er nå 1,2,3. Kan se ut til at det tidligere har vært 
		#c('region', 'sentral', 'lokal'). Programmerer ut fra 1,2,3. 
		#Vil gjerne endre så kommmer 1,2, fra inputktr.
	dump as d, resh as r
	WHERE d.FreshOrganizationalUnitId = r.AvdReshID
	#Saps2Score tilsvarer beregnet SMR
Beregnet vi tidligere alder selv? Vi trenger desimalalder. Det er nå ei salig blanding av 
heltall og desimaltall som i utgangspunktet er definert som tekst.


#--------------------------------------Kvalitetskontroll-----------------------------------
rm(list=ls())
library(knitr)
setwd('C:/registre/NIR/trunk/KvalKtrAvData') 

aggregate(NIRdata$PatientInRegistryKey, by=list(NIRdata$ShNavn, NIRdata$ShTypeTxt), FUN=length)
aggregate(NIRdata$DaysAdmittedIntensiv, by=list(NIRdata$ShNavn, NIRdata$ShTypeTxt), FUN=length)

#--------------------------------------SAMLERAPPORT-----------------------------------
rm(list=ls())
library(knitr)
library(tools)	#texi2pdf

source("C:/Registre/NIR/trunk/MeanMed/NIRFigMeanMed.R", encoding="UTF-8")
source("C:/Registre/NIR/trunk/Fordeling/NIRFigFordeling.R", encoding="UTF-8")
source("C:/Registre/NIR/trunk/Andeler/NIRFigAndeler.R", encoding="UTF-8")
source("C:/Registre/NIR/trunk/AndelerGrVar/NIRFigAndelerGrVar.R", encoding="UTF-8")
source("C:/Registre/Rlib/trunk/NIRLibUtvalg.R", encoding="UTF-8")
NIRdata <- read.table('C:/Registre/NIR/data/NIR2014-11-07ansi.csv', sep=';', header=T)	#NIRvarSQL.csv
libkat <-  'C:/Registre/Rlib/trunk/'
reshID <- 112044 #102090 Ahus, 112044 Haukeland
setwd('C:/Registre/NIR/trunk/NIRSamleRapp') 
NIRdata <- NIRdata[sample(1:dim(NIRdata)[1],4000), ]
#knit(input, output = NULL, tangle = FALSE, text = NULL, envir = parent.frame())
knit('NIRSamleRapp.Rnw')
texi2pdf(file='NIRSamleRapp.tex')

#NIRSamleRapp for hver enkelt enhet (alle)
NIRdata <- read.table('C:/Registre/NIR/data/NIR2014-11-07.csv', sep=';', header=T)	#NIRvarSQL.csv
AlleResh <- c(...)
#AlleResh <- c(700385, 700720, 106271)	#De som mangler sykehusnavn

for (reshID in AlleResh ) {
	knit('NIRSamleRapp.Rnw')
	texi2pdf(file='NIRSamleRapp.tex')
	file.rename('NIRSamleRapp.pdf', paste0('NIRSamleRapp', reshID, '.pdf'))	#list.files(pattern="water_*.img", paste0("water_", 1:700))
}

#--------------------------------------- SENTRALMÅL ----------------------------------
rm(list=ls())
setwd("c:/ResultattjenesteGIT/Intensiv/")
load("NIRdata10000.Rdata")
valgtVar <- 'alder'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas'
minald <- 30 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- 6 #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
valgtMaal = '' #'Med' = median. Alt annet gir gjennomsnitt
datoFra <- '2010-12-30'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2016-08-01'	# standard: 3000
dodInt <- 0	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
outfile <- paste0(valgtVar,grType, '.png')

NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, 
                grType=grType, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, 
                erMann=erMann, outfile=outfile) 


#Ta med med/uten overføringer som valg? JA

for (valgtVar in c('SMR', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS')){
outfile <- paste(valgtVar, 'GjsnGrVar.pdf', sep='')
NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, grType=grType, 
		InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, erMann=erMann, 
		outfile=outfile) 
}
		
#--------------------------------------- FORDELING - tas ut? ----------------------------------
rm(list=ls())
setwd("c:/ResultattjenesteGIT/Intensiv/")
NIRdata <- read.table('C:/Registre/NIR/data/NIR2013.csv', sep=';', header=T)	#NIRvarSQL.csv
RegData <- NIRdata
setwd('C:/Registre/NIR/trunk/Fordeling') 
valgtVar <- 'respiratortid'	#'respiratortid', liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas'
outfile <- ''	#paste('Ford_',valgtVar, '.pdf', sep='')
minald <- 80 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
grType <- ''	#3:region, 1 el 2: sentral/lokal, alle
datoFra <- '2013-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
datoTil <- '2013-12-31'	# standard: 3000
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)

Fordeling(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald, grType=grType, 
		InnMaate=InnMaate, erMann=erMann, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, outfile=outfile) 

#Liggetid og respiratortid for pasienter som ikke er overført mellom sykehus.

for (valgtVar in c('alder', 'liggetid', 'respiratortid', 'SAPSII', 'NEMS')) {
outfile <- paste(valgtVar, '.pdf', sep='')
Fordeling(RegData=NIRdata, valgtVar=valgtVar, minald=minald, maxald=maxald, grType=grType, 
		InnMaate=InnMaate, erMann=erMann, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, outfile=outfile) 
		}

#--------------------------------------- Andeler ----------------------------------
rm(list=ls())
#NIRdata <- read.table('C:/Registre/NIR/data/Main2016-02-02.csv', sep=';', header=T) #, 
NIRdata <- read.table('C:/Registre/NIR/data/MainPROD2016-05-10.csv', sep=';', header=T) #, 
save(NIRdata, file='NIRdata10000.Rdata')
RegData <- NIRdata[sample(dim(NIRdata)[1], 10000),]
valgtVar <- 'Nas'	#'alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS', 'Nas', 'InnMaate'
outfile <- ''	#paste('Ford_',valgtVar, '.pdf', sep='')
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
#grType <- ''	#region, sentral, lokal, alle (må velges) - Ikke nå lenger?
datoFra <- '2013-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
datoTil <- '2016-12-31'	# standard: 3000
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
reshID=112044
enhetsUtvalg=3

setwd("c:/ResultattjenesteGIT/Intensiv/")
FigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg)

variable <- c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS', 'Nas', 'InnMaate')

for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_and.png')
	FigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	reshID=reshID, enhetsUtvalg=enhetsUtvalg)
}

#--------------------------------------- AndelSoylerGrVar ----------------------------------
rm(list=ls())
setwd("c:/ResultattjenesteGIT/Intensiv/")
load("NIRdata10000.Rdata")#RegData
valgtVar <- 'innMaate'	#alder_u18', 'alder_over80', 'dodeSykehus', 'dodeIntensiv', 'innMaate', 
                        #'respStotte', 'reinn
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
grType <- 3	#1/2: sentral/lokal, 3:regional, 99:'alle'
datoFra <- '2011-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2017-05-01'	# standard: 3000-01-01
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
outfile <- '' #paste0(valgtVar, 'GrVar.png')

NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
	datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
	grType=grType)


variable <- c('alder_u18', 'alder_over80', 'dodeSykehus', 'dodeIntensiv', 'innMaate', 
      'respStotte', 'reinn')
for (valgtVar in variable) {
		outfile <- paste0(valgtVar, 'GrVar3.png')
		NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra, 
		                   datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile, 
		                   grType=grType)
                  }

		
		
		
		
#---------------------AndelTid----------------------------------------------
rm(list=ls())
NIRdata <- read.table('C:/Registre/NIR/data/NIR2014-11-07ansi.csv', sep=';', header=T) #, 
RegData <- NIRdata
setwd('C:/Registre/NIR/trunk/AndelTid') 	
reshID=106218
minald <- 0 #(standard: 0)
maxald <- 130	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
datoFra <- '2013-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2014-12-01'	# standard: 3000-01-01
dodInt <- ''	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- 0	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
enhetsUtvalg <- 1	#0-5
valgtVar <- 'respiratortid'	#'respiratortid', 'liggetid','over80','under18','Reinn',
				#'DischargedIntensiveStatus', 'DischargedHospitalStatus', 'SMR',MechanicalRespirator
outfile <- ''	#paste(valgtVar, '.png', sep='')

source("NIRFigAndelTid.R", encoding="UTF-8")	
FigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt, 
		reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg)	

#---------------------GjsnTid----------------------------------------------
rm(list=ls())
NIRdata <- read.table('C:/Registre/NIR/data/NIR2014-11-07ansi.csv', sep=';', header=T) #, 
RegData <- NIRdata
setwd('C:/Registre/NIR/trunk/GjsnTid') 	
reshID=106218
minald <- 0 #(standard: 0)
maxald <- 60	#(standard: 130, må være større enn minald!)
InnMaate <- 8 #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
datoFra <- '2012-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM. 
datoTil <- '2014-12-01'	# standard: 3000-01-01
dodInt <- 1	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
valgtMaal <- 'Med'
enhetsUtvalg <- 3	#0-5
valgtVar <- 'SAPSII'	#'respiratortid', 'liggetid','over80','under18','Reinn','SAPSII'
				#'DischargedIntensiveStatus', 'DischargedHospitalStatus', 
outfile <- ''	#paste(valgtVar, '.png', sep='')

source("NIRFigGjsnTid.R", encoding="UTF-8")	
GjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    erMann=erMann,minald=minald, maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
		              valgtMaal=valgtMaal,tittel=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
		
		
#---------------------------------------TEST----------------------------------

setwd('C:/registre/NIR/trunk/MeanMed') 
source("FigMeanMed.R", encoding="UTF-8")
for (valgtVar in c('alder', 'liggetid', 'respiratortid',  'SAPSII')) {
#valgtVar <- 'NEMS'
	for (minald in c(0, 50, 80) ) {
		for (InnMaate in c(0,6,8) ) {
			for (valgtMaal in c('', 'Med') ) {
				for (grType in c('region', 'sentral', 'lokal')) {
					for (dF in 1:3) { datoFra <- c('0','2011','2012')[dF]
						for (dT in 1:3) {datoTil <- c('2012-02-02','3000')[dT]
							for (dodInt in c('',0,1)) {
							outfile <- paste('C:/NIR/NIRrep/testFig/',
								valgtVar, '_minA', minald, '_innM', InnMaate, valgtMaal, '_', grType,
								'_dF', dF, '_dT', dT, dodInt, '.png', sep='')
							MeanMed(RegData=NIRdata, valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald, grType=grType, 
								InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt, outfile=outfile) 
							}
						}
					}
				}
			}
		}	
	}
}

4*3*3*2*3*3*2*3

