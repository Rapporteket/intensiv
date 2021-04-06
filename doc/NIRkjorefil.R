#Til analysebok:
#Liggetid og respiratortid for pasienter som ikke er overført mellom sykehus.

#Alle off.farger:

      #c6dbef #6baed6 #4292c6 #2171b5 #084594 #000059 #FF7260 #4D4D4D #737373 #A6A6A6 #DADADA
blaa <- c('#084594','#2171b5','#4292c6','#6baed6','#c6dbef')  #Mørk til lys																# Fem blaatoner
graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
kontrast <- '#FF7260'; moerkeblaa <- '#000059'                																# Spesialfarger


datoFra <- '2020-09-01'
datoTil=Sys.Date()
reshID <- 706078
tellInfluensa(datoFra='2018-09-01', datoTil=Sys.Date(), reshID=reshID)

#--------------------------------------Kvalitetskontroll - ikke operativ-----------------------------------
rm(list=ls())
library(knitr)
setwd('C:/registre/NIR/trunk/KvalKtrAvData')

aggregate(NIRdata$PatientInRegistryKey, by=list(NIRdata$ShNavn, NIRdata$ShTypeTxt), FUN=length)
aggregate(NIRdata$DaysAdmittedIntensiv, by=list(NIRdata$ShNavn, NIRdata$ShTypeTxt), FUN=length)

#--------------------------------------SAMLERAPPORT/MndRapp/Influensa/Beredskap-----------------------------------
rm(list=ls())
library(knitr)
library(intensiv)
library(tools)	#texi2pdf
setwd('C:/ResultattjenesteGIT/intensiv/inst/')
setwd('/home/rstudio/intensiv/inst')
reshID=706078 #Tromsø med int: 601302, Ullevål Kir int: 109773, 102090 Ahus, 112044 Haukeland, 102673 Ålesund Med, Kristiansund: 706078

RegData <- NIRPreprosess(NIRRegDataSQL(datoFra = '2021-01-01', datoTil = '2021-12-31'))



# Kan du gje oss tal på registrerte pasientar nasjonalt i Norsk intensivregister med denne avgrensinga:
#   1.       Fylte 80 år eller eldre ved innlegging intensiv i perioden 01.07.16-30.06.18 (truleg kring 5000)
# Kor mange av desse er framleis i live slik det er oppdatert i registeret i dag?

RegData <- NIRPreprosess(NIRRegDataSQL(datoFra = '2016-07-16', datoTil = '2018-06-18'))
RegData <- NIRUtvalgEnh(RegData = RegData, minald = 80)$RegData
antall <- length(unique(RegData$PasientID))
ind <- which(is.na(RegData$Morsdato))
table(RegData$DischargedIntensiveStatus)
table(RegData$Dod30)
table(RegData$Dod90)
table(RegData$Dod365)
table(!is.na(RegData$Morsdato))

levendeNaa <- length(unique(RegData$PasientID[is.na(RegData$Morsdato)]))



# test <- unique(RegData[,c("ShNavn", "ReshId")])
# test[order(test[,1]),]
RegData <- FinnReinnleggelser(RegData)
ind <- which(RegData$PatientTransferredFromHospital>0 | RegData$PatientTransferredToHospital>0)
RegData$Reinn[ind] <- 2

table(RegData$Reinn)

RegData <- NIRRegDataSQL(datoFra = '2019-01-01', datoTil = '2019-12-31')
DataKalnesRaa <- RegData[RegData$UnitId == 4209889, ] #c('PasientID', 'SkjemaGUID', 'InnDato')]
DataKalnes <- NIRPreprosess(RegData=DataKalnesRaa)
DataKalnes <- FinnReinnleggelser(RegData = DataKalnes)

table(DataKalnes$Reinn)
pas <- DataKalnes$PasientID[DataKalnes$Reinn==1]
DataPas <- DataKalnes[DataKalnes$PasientID %in% pas, c('PasientID', 'SkjemaGUID','InnDato', 'Reinn')] #'DateDischargedIntensive',
TabKalnes <- DataPas[order(DataPas$PasientID, DataPas$InnDato), ]
write.table(TabKalnes, file='KalnesReinn.csv', fileEncoding = 'UTF-8', sep = ';', row.names = F)

dataUtGjsnTid <- NIRFigGjsnTid(RegData=RegData, preprosess = 0, tidsenhet = 'Aar',
                               enhetsUtvalg = 2,
                               reshID=700419, datoFra='2020-01-01')
t(dataUtGjsnTid$AggVerdier)

#ind <- intersect(which(RegData$CerebralCirculationAbolishedReasonForNo>-1))
gr <- 0:8
RegData <- RegData[which(RegData$CerebralCirculationAbolishedReasonForNo %in% gr),]
Utdata <- NIRFigAndeler(RegData=RegData, valgtVar='CerebralCirculationAbolishedReasonForNo', #CerebralCirculationAbolishedReasonForNo
                        reshID = 107717, enhetsUtvalg = 7)
Utdata$Nfig
Utdata$N
#RegData <- RegData[ind,]
aggVar  <- list(RegData$ShType) #RegData$CerebralCirculationAbolishedReasonForNo,
aggregate(x=RegData$ReshId, by=aggVar, FUN=length)


#load(paste0("A:/Intensiv/NIRdata10000.Rdata")) #RegDataTEST, 21.mai 2018
load(paste0("A:/Intensiv/MainFormDataContract2019-01-30.Rdata")) #RegData 2018-06-18
#knit('NIRmndRapp.Rnw', encoding = 'UTF-8')
#tools::texi2pdf(file='NIRmndRapp.tex')
knit2pdf('NIRmndRapp.Rnw') #, encoding = 'UTF-8')
#Får ikke denne til å funke: rmarkdown::render('NIRmndRapp.Rnw', output_format = pdf_document(),
                         #params = list(tableFormat="latex"))
load(paste0("A:/Intensiv/intensivdata.Rdata")) #RegData 2018-06-18
reshID=706078 #Tromsø med int: 601302, Ullevål Kir int: 109773, 102090 Ahus, 112044 Haukeland, 102673 Ålesund Med, Kristiansund: 706078
knit('NIRmndRapp.Rnw', encoding = 'UTF-8')
tools::texi2pdf(file='NIRmndRapp.tex')
rmarkdown::render('NIRmndRapp.Rnw', output_format = 'beamer_presentation')
#, params = list(tableFormat="latex"))

#knit(input, output = NULL, tangle = FALSE, text = NULL, envir = parent.frame())
# NIRdata <- RegData
knit('NIRSamleRapp.Rnw')
texi2pdf(file='NIRSamleRapp.tex')

knit('OffDataIntensiv.Rnw')
texi2pdf(file='OffDataIntensiv.tex')

dato <- '2019-11-05' #2019-01-30
InfluDataAlle <- read.table(paste0('A:/Intensiv/InfluensaFormDataContract', dato, '.csv'), sep=';',
                            stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
variableTilTab <- c('ShNavn', 'RHF', 'PatientInRegistryGuid', 'FormDate','FormStatus', 'ICD10_1') #'DateAdmittedIntensive',
InfluData <- InfluDataAlle[ ,variableTilTab]
# knit('NIRinfluensaUtenICD10.Rnw', encoding = 'UTF-8')
# tools::texi2pdf(file='NIRinfluensaUtenICD10.tex')
 knit('NIRinfluensa.Rnw', encoding = 'UTF-8')
 tools::texi2pdf(file='NIRinfluensa.tex')

 knit('BeredskapCorona.Rnw', encoding = 'UTF-8')
 tools::texi2pdf(file='NIRinfluensa.tex')
 knitr::knit2pdf('BeredskapCorona.Rnw') #, encoding = 'UTF-8')

#-------------------------------------LASTE DATA-----------------------------------------------
rm(list=ls())

dato <- '2019-09-24' #'2018-12-14' #MainFormDataContract2018-06-19
dataKat <- 'A:/Intensiv/'
fil <- paste0(dataKat,'MainFormDataContract',dato)
NIRdata <- read.table(file=paste0(fil,'.csv'), header=T, stringsAsFactors=FALSE, sep=';',encoding = 'UTF-8')
RegData <- NIRdata
load(paste0(fil,".Rdata")) #RegData 2019-01-07
save(RegData, file=paste0('intensivdata.Rdata'))
 # RegData <- RegData[which(
 #       as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d")>= '2015-01-01'), ]
#RegData <- RegData[sample(1:dim(RegData)[1],10000),]
#save(RegData, file=paste0(dataKat,'NIRdata10000.Rdata'))
library(intensiv)
load(paste0("A:/Intensiv/NIRdata10000.Rdata")) #RegDataTEST, 2018-06-05

Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

"LC_CTYPE=en_US.UTF-8;
LC_NUMERIC=C;
LC_TIME=nb_NO;
LC_COLLATE=en_US.UTF-8;
LC_MONETARY=en_US.UTF-8;
LC_MESSAGES=en_US.UTF-8;
LC_PAPER=en_US.UTF-8;LC_NAME=C;
LC_ADDRESS=C;LC_TELEPHONE=C;LC_MEASUREMENT=en_US.UTF-8;LC_IDENTIFICATION=C"
#---------LagSyntetiskeData-------------------------
library(intensiv)
#Hovedtabell
varBort <- c('PostalCode', 'HF Sykehus', 'Helseenhet', 'HelseenhetKortnavn', 'LastUpdate', 'ShNavn', 'MunicipalNumber', 'Municipal',
             'ICD10_1', 'ICD10_2', 'ICD10_3', 'ICD10_4', 'ICD10_5', 'Sykehus', 'HelseenhetID')
HovedData <- read.table(file=paste0(fil,'.csv'), header=T, stringsAsFactors=FALSE, sep=';',encoding = 'UTF-8')
RegData <- lageTulleData(RegData=HovedData, varBort=varBort, antSh=26, antObs=20000)
#Pårørendedata
filPaaror <- paste0(dataKat,'QuestionaryFormDataContract',dato,'.csv')
PaarorData <- read.table(file=filPaaror, header=T, stringsAsFactors=FALSE, sep=';',encoding = 'UTF-8')

KobleMedHoved <- function(HovedSkjema, Skjema2, alleHovedskjema=F, alleSkjema2=F) {
  #HovedSkjema <- plyr::rename(HovedSkjema, c('FormDate' = 'FormDateHoved'))
      varBegge <- intersect(names(HovedSkjema),names(Skjema2)) ##Variabelnavn som finnes i begge datasett
      Skjema2 <- Skjema2[ , c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",
      data <- merge(HovedSkjema, Skjema2, suffixes = c('','_S2'),
                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = alleHovedskjema, all.y=alleSkjema2)
      return(data)
}
HovedSkjema <- RegData
Skjema2 <- PaarorData

PaarorDataH2018 <- KobleMedHoved(HovedSkjema = RegData2018, Skjema2 = PaarorData)
PaarorDataH <- lageTulleData(RegData=PaarorDataH, varBort=varBort, antSh=26, antObs=600)
save(PaarorDataH, file=paste0(dataKat, 'PaarorRegData.RData'))
write.table(PaarorDataH, file='A:/Intensiv/PaarorDataHtull.csv', fileEncoding = 'UTF-8', sep = ';', row.names = F)
save(list=c('RegData', 'PaarorDataH'), file=paste0(dataKat, '/NIRRegDataSyn.RData'))
#save(RegData, PaarorData, file=paste0(dataKat, '/NIRRegDataSyn.RData'))
load('A:/Intensiv/NIRRegDataSyn.RData')

# Div sjekk
table(RegData$ShNavn, RegData$Aar)
ind <- which(RegData$ShNavn == 'Kristiansand')
table(RegData$ShNavn[ind], RegData$TransferredStatus[ind])
reshID <- 114240
RegData$PatientTransferredFromHospital[ind]
table(RegData$PatientTransferredFromHospital)
table(RegData$PatientTransferredFromHospitalName[ind])
table(RegData$PatientTransferredToHospitalName[ind])
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
erMann <- 99
tidsenhet <- 'Kvartal'
outfile <- ''
valgtVar <- 'reinn'  #reinn, respiratortidInv
outfile <- '' #paste0('OffRand', valgtVar, '.pdf')
#Laste offdata
filnavn <- paste0('NIRdata01', valgtVar)
load(paste0(dataKat, filnavn, '.Rdata'))
RegData <- NIRdata01reinn #NIRdata01respiratortid #NIRdata01reinn


DataTilbake <- NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, aar=aar, grType=grType,
                               grVar='ShNavn', InnMaate=InnMaate, hentData=0, aldGr=aldGr,
                               outfile=outfile, lagFig=1, offData=1) #

DataTilbake <- NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                           tidsenhet = tidsenhet,minald=minald, maxald=maxald, InnMaate=InnMaate,
                           dodInt=dodInt, reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg,
                           lagFig = 1, offData=1)
#aar=0, grType=grType )

ind <- which(RegData$InvasivVentilation>0)
Resp <- tapply(RegData$respiratortid[ind],RegData$Aar[ind], FUN=sum)
Inv <- tapply(RegData$InvasivVentilation[ind],RegData$Aar[ind], FUN=sum)
NonInv <- tapply(RegData$NonInvasivVentilation[ind],RegData$Aar[ind], FUN=sum)

#-------------------------------------- Parametre ----------------------------------------------------
library(intensiv)
setwd("C:/ResultattjenesteGIT/intensiv/")
RegData <- NIRPreprosess(NIRRegDataSQL(datoFra = '2019-01-01'))
reshID=700419 #109773 #Tromsø med int: 601302, Ullevål Kir int: 109773, Haukeland ROE: 107717
minald <- 0 #(standard: 0)
maxald <- 40	#(standard: 130, må være større enn minald!)
InnMaate <- '' #0-El, 6-Ak.m, 8-Ak.k, (alle - alt unntatt 0,6,8)
valgtMaal = 'Gjsn' #'Med' = median. 'Gjsn' = gjennomsnitt. Alt annet gir gjennomsnitt
datoFra <- '2020-01-01'	# standard: 0	format: YYYY-MM-DD. Kan spesifisere bare første del, eks. YYYY el. YYYY-MM.
datoTil <- '2020-12-31'	# standard: 3000
aar <- 0
dodInt <- 9	# 0-i live, 1 -død, standard: alle (alle andre verdier)
erMann <- ''	#Kjønn: 0-kvinner, 1-menn, standard: alle (alle andre verdier)
overfPas <- ''    #Overført under pågående intensivbehandling?	1 = Nei, 2 = Ja
grType <- 99	#1/2: sentral/lokal, 3:regional, 99:'alle'
enhetsUtvalg <- 2	#0-8
grVar <- 'ShNavn'
tidsenhet <- 'Mnd'
medKI <- 0
offData <- 0
outfile <- ''
valgtVar <- 'alder'
#Parameter for evt. kvalitetsmål? angis i Tilrettelegging


NIRFigInnMaate (RegData=RegData, valgtVar='InnMaate', minald=0, maxald=130, datoTil = datoTil,
                           grVar='ShNavn', InnMaate=99, dodInt='', outfile='')

#--------------------------------------- Ny struktur basert på grVar? ----------------------------------
#Prioriter kvalitetsindikatorene: reinn, SMR, median innleggelse (se årsrapport)
#Median respiratortid < 2,5 døger -> Kan vi vise andel med respiratortid <2,5døgn og sette grense på 50%?
#Standardisert mortalitetsratio (SMR) < 0,7 (etter ikkje-justert alvorsskåre)
#Andel reinnlegging til intensiv i løpet av 72 timar < 4% av opphalda (def. endret 2016)
#Alle disse vises per sykehus for et gitt tidsintervall (siste 12 mnd?)
#I tillegg kanskje vi skal vise utvikling over tid for valgt sykehus og sykehustype?


#--------------------------------------- Fordelinger ----------------------------------
valgtVar <- 'inklKrit'	#'alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS24', 'Nas24', 'InnMaate'
                              #Nye: PrimaryReasonAdmitted, inklKrit, respiratortidNonInv, respiratortidInv
                              #nyreBeh, nyreBehTid, ExtendedHemodynamicMonitoring, isolering, isoleringDogn,
                              #spesTiltak
                              #Nye, aug-18: CerebralCirculationAbolishedReasonForNo, OrganDonationCompletedReasonForNoStatus
                              #Nye: 'utenforVakttidInn'
RegData <- NIRPreprosess(NIRRegDataSQL())
Utdata <- NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar='inklKrit', #datoFra=datoFra, datoTil=datoTil,
              #minald=minald, maxald=maxald,   InnMaate=InnMaate, dodInt=dodInt,erMann=erMann,
              outfile='', reshID=109773, enhetsUtvalg=0, lagFig=1)

outfile <- '' #paste0(valgtVar,'_Ford', '.png')
NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra,
                         datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile,
                         hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=0, lagFig=1)


variable <- c('alder', 'liggetid', 'respiratortid',  'SAPSII', 'NEMS24', 'Nas24', 'InnMaate')
variable <- c('PrimaryReasonAdmitted', 'inklKrit', 'respiratortidNonInv', 'respiratortidInv', 'nyreBeh',
              'nyreBehTid', 'ExtendedHemodynamicMonitoring', 'isolering', 'isoleringDogn')
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_Ford.png')
	NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra,
	                     datoTil=datoTil, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile,
	                     hentData=0, preprosess=1, reshID=reshID, enhetsUtvalg=enhetsUtvalg, lagFig=1)
}

#--------------------------------------- AndelGrVar ----------------------------------
grVar <- 'ShNavn'
valgtVar <- 'liggetidDod'	#alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'innMaate',
                        #liggetidDod, respiratortid, 'respStotte', 'reinn
                        #trakeostomi, trakAapen, respiratortidInv, nyreBeh, ExtendedHemodynamicMonitoring,
                        #ExtendedHemodynamicMonitoringPA, isolering
                        #Nye, aug-18: OrganDonationCompletedStatus, OrganDonationCompletedCirc
#Ny, okt-18: utenforVakttidInn, utenforVakttidUt
outfile <- ''# paste0(valgtVar, '_sh.pdf')
RegData <- NIRRegDataSQL()
RegData <- NIRPreprosess(RegData = RegData)
NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra,
                datoTil=datoTil, aar=0, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile,
                grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1, medKI=0,offData = offData)

#NIRAndelerGrVar(RegData=RegData, valgtVar=valgtVar, minald=minald, maxald=maxald,  datoFra=datoFra,
#                datoTil=datoTil, aar=0, InnMaate=InnMaate, dodInt=dodInt,erMann=erMann, outfile=outfile,
#                grType=grType, grVar=grVar, hentData=0, preprosess=1, lagFig=1, offData = offData)

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
tidsenhet <- 'Mnd'
enhetsUtvalg <- 2
valgtVar <- 'reinn'	#'alder_u18', 'alder_over80', 'dod30d', 'dodeIntensiv', 'liggetidDod',
                        #'respiratortidDod', 'respStotte', 'reinn',
                        #'UT: respiratortid,
                        #Ny: respiratortidInvMoverf, respiratortidInvUoverf
outfile <- '' #paste0(valgtVar, '.png')

AndelerTid <- NIRFigAndelTid(RegData=RegData, preprosess = 0, reshID = 706078, tidsenhet = 'Mnd', enhetsUtvalg = 1)

tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid)

NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, tidsenhet = tidsenhet,
		minald=minald, maxald=maxald, erMann=erMann,InnMaate=InnMaate, dodInt=dodInt,
		reshID=reshID, outfile=outfile, enhetsUtvalg=enhetsUtvalg, lagFig = 1, offData=offData)
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
tidsenhet <- 'Aar'
datoFra <- '2012-01-01'
valgtVar <- 'alder'	#'alder', 'liggetid', 'respiratortid', 'SAPSII',
                        #Nye: respiratortidInvMoverf, respiratortidInvUoverf, respiratortidNonInv
outfile <- '' #paste0(valgtVar, 'GjsnTid.pdf')

utdata <- NIRFigGjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
              tidsenhet=tidsenhet,
                    erMann=erMann,minald=minald,  maxald=maxald, InnMaate=InnMaate, dodInt=dodInt,
		              valgtMaal='Med',tittel=1, enhetsUtvalg=0, reshID=reshID)

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
grType <- 1
outfile <- '' #paste0(valgtVar, '_test.pdf')#,grType
data <- NIRFigGjsnGrVar(RegData=RegData, medKI = 1 ,valgtVar=valgtVar, valgtMaal=valgtMaal, minald=minald, maxald=maxald,
             grType=grType, grVar=grVar, InnMaate=InnMaate, datoFra=datoFra, datoTil=datoTil, dodInt=dodInt,
             erMann=erMann, outfile=outfile)
NIRFigGjsnGrVar(RegData=NIRPreprosess(NIRRegDataSQL()), preprosess = 0, valgtVar='SMR')
              valgtMaal=valgtMaal, minald=minald, maxald=maxald,
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


#--------------------------------------OFFENTLIGGJØRING, figurer-------------------------------------

setwd('aarsrappOff/')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', valgtMaal = 'Med',
                datoTil='2016-12-31', grType=1, outfile='Respiratortid_loksent_Fig2aNy.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', valgtMaal = 'Med',
                                datoTil='2016-12-31', grType=3, outfile='Respiratortid_region_Fig2bNy.pdf')#

NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01',
                datoTil='2016-12-31', grType=1, outfile='Respiratortid_loksent_Fig2a.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', datoFra='2016-01-01', valgtMaal = 'Gjsn',
                                datoTil='2016-12-31', grType=3, outfile='test.png') #Respiratortid_region_Fig2bNy.pdf')

NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra='2016-01-01', medKI = 1,
                   datoTil='2016-12-31', grType=1, outfile='Reinnlegging_loksent_Fig3aKonfInt.pdf')
NIRFigAndelerGrVar(RegData=RegData, valgtVar='reinn', datoFra='2016-01-01', medKI = 1,
                   datoTil='2016-12-31', grType=3, outfile='Reinnlegging_region_Fig3bKonfInt.pdf') #Reinnlegging_region_Fig3bNy.pdf')


#------------------Tabeller-----------------------------------

RegData <- NIRPreprosess(RegData)
tabBelegg(RegData=RegData, datoTil = datoTil, tidsenhet='Mnd') #personIDvar='PasientID',

finnDblReg(RegData, reshID=114240)

tabAntOpphSh5Aar(RegData, datoTil)

tabAntPasSh5Aar(RegData, personIDvar='PasientID' , datoTil)



#----------------Kobling av transport-data-----------------------
TransportData <- read.table(file='A:/Intensiv/Intensivtransport/Intensivtransport.csv', header=T, stringsAsFactors=FALSE, sep=';',encoding = 'UTF-8')
TransportData$DatoTid <- paste(TransportData$Dato, TransportData$Klokkeslett)
TransportData <- TransportData[order(TransportData$Personnummer),]
RegisterData <- read.table(file='A:/Intensiv/Intensivtransport/IntensivVariabel.csv', header=T, stringsAsFactors=FALSE, sep=';',encoding = 'UTF-8')
RegisterData <- RegisterData[order(RegisterData$Fnr), ]

library(intensiv)
RegisterData$Innleggelsestidspunkt <- as.POSIXlt(RegisterData$DateAndTimeAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d %H:%M" )
finnDblReg(RegData = RegisterData, pasientID = 'Fnr') #datoFra = '2013-01-01',

reshID=112044
RegData <- NIRPreprosess(RegData)
DblReg <- finnDblReg(RegData = RegData) #reshID = reshID, datoFra = '2018-01-01'
write.table(DblReg,file = 'Dobbeltreg.csv',row.names = F, col.names = T, sep = ';')

#Skal koble sammen på personnummer og tid. Tillater inntil 24t avvik mellom innleggelsesdato og transportdato
#Bruke difftime?
# 1. Sjekke hvilke personnummer fra intensivtransporten som finnes i intensivfila
# 2. Sjekke match på tid for de aktuelle personnumrene

#PersnrBruk <- sort(TransportData$Personnummer)[sort(TransportData$Personnummer) %in% sort(RegisterData$Fnr)]
indPersMatchTransp <- TransportData$Personnummer %in% RegisterData$Fnr
indPersMatchRegister <- which(RegisterData$Fnr %in% TransportData$Personnummer)
TransportData <- TransportData[indPersMatchTransp,]
RegisterData <- RegisterData[indPersMatchRegister,]
#1142 (av 16958)reg. basert på personnummer fra Registeret finnes i TransportData
#699 (av 822) reg. basert på personnummer fra Registeret finnes i TransportData
write.table(TransportData,file = 'TransportDataMatch.csv',row.names = F, col.names = T, sep = ';')
write.table(RegisterData,file = 'RegDataTranspMatch.csv',row.names = F, col.names = T, sep = ';')

#Beregne 30-dagers dødelighet
RegisterData$Dod30d <- 0
RegisterData$Dod90d <- 0
RegisterData$Dod365d <- 0
RegisterData$Dod30d[which(difftime(as.Date(RegisterData$Morsdato, format="%d.%m.%Y"),
                             as.Date(RegisterData$DateAndTimeAdmittedIntensive), units='days')< 30)] <- 1

RegisterData$Dod90d[which(difftime(as.Date(RegisterData$Morsdato, format="%d.%m.%Y"),
                             as.Date(RegisterData$DateAndTimeAdmittedIntensive), units='days')< 90)] <- 1
RegisterData$Dod365d[which(difftime(as.Date(RegisterData$Morsdato, format="%d.%m.%Y"),
                             as.Date(RegisterData$DateAndTimeAdmittedIntensive), units='days')< 365)] <- 1
table(RegisterData$Dod30d)
table(RegisterData$Dod90d)
table(RegisterData$Dod365d)

avvik <- 24
indMatchRegData <- NULL
indMatchTranspData <- NULL
TranspRegAlle <- cbind(TransportData[0,], RegisterData[0,])
#NB: Tar ikke høyde for dobbeltregistreringer
for (k in 1:dim(TransportData)[1]) { #dim(TransportData)[1]
      ind <- which(RegisterData$Fnr  %in% TransportData$Personnummer[k]) #Hvilke reg. som er aktuelle ut fra pers.nr.
       diff <- difftime(as.POSIXlt(TransportData$DatoTid[k], tz='UTC'),
                             as.POSIXlt(RegisterData$DateAndTimeAdmittedIntensive[ind], tz='UTC'), units = 'hours')
       sjekk <- sum(min(abs(diff)) < avvik) #antall av minste differanse < avvik
       if (sjekk > 0){
             indMatchTranspData <- c(indMatchTranspData, k) #Radnummer i transportdata
             indMatchRegData <- ind[which(abs(diff) == min(abs(diff)))] #c(indMatchRegData , ind[which(abs(diff) == min(abs(diff)))]) #matcher minste avvik. Kan være flere
             for (j in indMatchRegData){
             TranspRegAlle <- rbind(TranspRegAlle,
                                    cbind(TransportData[k,],
                                    RegisterData[j,])
                          )
             }
       }
#k <- k+1
       }
write.table(TranspRegAlle,file = 'A:/Intensiv/Intensivtransport/TranspRegDataAlleMatch.csv',row.names = F, col.names = T, sep = ';')

indMatchTranspData <- indMatchTranspData[indMatchTranspData>0]
indMatchRegData <- unique(indMatchRegData)

TransportData[indMatchTranspData,]
RegisterData[indMatchRegData,]


merge(TransportData[k,],
      RegisterData[ind,c('Fnr', "DateAndTimeAdmittedIntensive")])


PersnrMatch <- TransportData$Personnummer[indPersMatch]
table(table(PersnrMatch))

diffTid <- difftime(as.POSIXlt(TransportData$DatoTid, tz='UTC'), #, format = '%Y-%m-%d %t:%m'),
                    as.POSIXlt(RegisterData$DateAndTimeAdmittedIntensive, tz='UTC'), units = 'hours')


avvik <- 48 #timer
test <- difftime(as.POSIXlt(TransportData$DatoTid[1:10], tz='UTC'), #, format = '%Y-%m-%d %t:%m'),
                 as.POSIXlt(RegisterData$DateAndTimeAdmittedIntensive[1:10], tz='UTC'), units = 'hours')


DataKoblet <- merge(TransportData, RegisterData, suffixes = c('','_Int'),
              by.x = 'Fnr', by.y = 'Personnummer', all.x = F, all.y=F)



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


#----------------- Sepsispasienter 2017 og 2018
library(intensiv)
rm(list=ls())
RegData <- NIRRegDataSQL(datoFra = '2017-01-01', datoTil = '2018-12-31')
RegData <- NIRPreprosess(RegData = RegData)
RegData <- RegData[which(RegData$PrimaryReasonAdmitted == 5), ] #Sepsis

AntSepsis <- dim(RegData)[1]
Ant30 <- table(RegData$Dod30)
Andel30 <- paste0(sprintf('%.1f',Ant30/AntSepsis*100), '%')
Ant90 <- table(RegData$Dod90)
Andel90 <- paste0(sprintf('%.1f',Ant90/AntSepsis*100), '%')

tab <- rbind(
  Ant30,
  Andel30,
  Ant90,
  Andel90
)
colnames(tab) <- c('Levende', 'Død')
t(tab)

#----------------------- INFLUENSA ------------------------------------------

NIRInfluDataSQL <- function(datoFra = '2019-09-25', datoTil = Sys.Date()) {

  query <- paste0('SELECT
                  *
                  # ShNavn,
                  # RHF,
                  # PatientInRegistryGuid,
                  # FormDate,
                  # ICD10_1,
                  # FormStatus
            FROM InfluensaFormDataContract
            WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  #WHERE cast(DateAdmittedIntensive as date) >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')

  RegData <- rapbase::LoadRegData(registryName = "nir", query, dbType = "mysql")
  return(RegData)
}

InfluData <- NIRInfluDataSQL(datoFra = '2018-09-20')

InfluData$Influensa <- factor(NA, levels = c('Mistenkt', 'Bekreftet'))
#--Identifiser J10 og J11 i ICD10-variablene.
InfluData$Influensa[which(InfluData$ICD10_1 %in% c(-1,13:16))] <- 'Mistenkt'
InfluData$Influensa[which(InfluData$ICD10_1 %in% c(9:12))] <- 'Bekreftet'

#Legge på tidsenheter
InfluData$InnDato <- as.Date(InfluData$FormDate) #, tz='UTC', format = '%Y-%m-%d"')
InfluData$Aar <- format(InfluData$InnDato, '%Y')
InfluData$UkeNr <- as.factor(format(InfluData$InnDato, '%V'))
#InfluData$UkeNr <- factor(InfluData$UkeNr, levels=c(min(InfluData$UkeNr):max(InfluData$UkeNr)))
InfluData$UkeAar <- format(InfluData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
InfluData$UkeAar <- as.factor(InfluData$UkeAar)
InfluData$Sesong <- NA
InfluData$Sesong[which(InfluData$InnDato> '2018-09-20' & InfluData$InnDato < '2019-05-20')] <- '2018/19'
InfluData$Sesong[which(InfluData$InnDato >= '2019-09-30' & InfluData$InnDato < '2020-05-18')] <- '2019/20'

  gr <- c(0, 15, 25, 60, 80,150)
  InfluData$AlderGr <- cut(InfluData$AgeAdmitted, breaks=gr, include.lowest=TRUE, right=FALSE)
  #Aldersfordeling for BEKREFTEDE tilfeller.
  InfluDataBekr <- InfluData[which(InfluData$Influensa=='Bekreftet'), ]
TabAlderSes <- table(InfluDataBekr$AlderGr, InfluDataBekr$Sesong)
TabAlderSes <- addmargins(TabAlderSes)
write.table(TabAlderSes, file='TabAlderSes.csv', fileEncoding = 'UTF-8', sep = ';', row.names = T)

# indFerdig <- which(InfluData$FormStatus==2)
# antFerdig <- length(indFerdig)
# antSkjema <- dim(InfluData)[1]

TabUkeInflu <- table(InfluData[ ,c('UkeAar', 'Influensa')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
TabUkeTot <- addmargins(TabUkeInflu) #cbind(TabUkeInflu, 'Tot. ant. skjema' = table(InfluData$UkeAar))
write.table(TabUkeTot, file='InfluPrUke.csv', fileEncoding = 'UTF-8', sep = ';', row.names = T)



