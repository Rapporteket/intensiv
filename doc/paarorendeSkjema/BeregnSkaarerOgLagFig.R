#Skåring
# Sp.mål 1-9 og 11-14 del 1 og 2-6 del 2 :
#       Fremragende   100
# Meget godt 	75
# Godt 		50
# Noenlunde 	25
# Dårlig 		 0
# 
# Spørsmål 10:
#       Svært ofte       100
# Ofte 		75
# Av og til	 	50
# Sjelden 		25
# Aldri 		 0
# 
# Svært ofte       100
# Ofte 		75
# Av og til	 	50
# Sjelden 		25
# Aldri 		 0
# 
# Sp.mål 7-9 del 2:
#       0 (fyrste/øvste/dårlegaste alternativ)
# 25
# 50
# 75
# 100 siste/nedste/beste alternativ)
# 
# Spm10
# Jeg kunne ha trengt mer tid: 0
# Jeg hadde tilstrekkelig med tid: 0
# 
# Sp.mål 11-13 del 2:
#       0 (fyrste/øvste/dårlegaste alternativ)
# 25
# 50
# 75
# 100 (siste/nedste/beste alternativ)








rm(list=ls())

PaarorData <- read.table(file='A:/Intensiv/QuestionaryFormDataContract2017-09-18.csv', header=T, sep=';',encoding = 'UTF-8')
load('A:/Intensiv/MainFormDataContract2017-09-18.Rdata') #read.table(file='C:/Registre/NIR/data/Main2017-03-20.csv', header=T, sep=';',encoding = 'UTF-8')
#PaarorData$PreskjemaGUID <- toupper(PaarorData$HovedskjemaGUID)
hovedVar <- c('SkjemaGUID','DateAdmittedIntensive', 'DaysAdmittedIntensiv','Respirator','TransferredStatus', 
              'Saps2Score','Saps2ScoreNumber', 'TypeOfAdmission', 'Nems', 'Morsdato', 
              'PatientTransferredFromHospital', 'PatientTransferredToHospital', 'ShNavn')
varUT <- which(names(PaarorData) %in% c('Forslag', 'Kommentar', 'Personalet'))
RegData <- merge(PaarorData[ ,-varUT], RegData[ ,hovedVar], 
                 by.x='HovedskjemaGUID', by.y='SkjemaGUID', all.x = TRUE, all.y = FALSE)
#write.table(RegData, file = 'PaarorData.csv', row.names=FALSE, sep = ';', fileEncoding = "UTF-8")

#Definerer variabel hvor 1 - før intervensjon, 2-etter intervensjon (innleggelse f.o.m. 1.okt. 2016)
#Fyrste måleperiode var pasientar innlagde 01.10.15-31.12.15 og andre 01.10.16-31.12.16.  
#Basaldata gjeld altså data for alle opphald før 01.10.16, og kontroll etter intervensjon er 
#data for alle opphald etter 01.10.16.
#Dette må senere gjøres om til et brukervalg, dvs. at brukeren kan velge hvilke perioder 
#som skal sammenlignes

RegData$PrePost <- 0
RegData$PrePost[as.POSIXlt(RegData$DateAdmittedIntensive) < as.POSIXlt('2016-10-01')] <- 1
RegData$PrePost[as.POSIXlt(RegData$DateAdmittedIntensive) >=as.POSIXlt('2016-10-01')] <- 2
#feil: 
RegData$PrePost[is.na(RegData$DateAdmittedIntensive) & 
                      (as.POSIXlt(RegData$FormDate, format='%Y-%m-%d %H:%M') < as.POSIXlt('2016-10-01'))] <- 1
RegData$PrePost[is.na(RegData$DateAdmittedIntensive) & 
                      (as.POSIXlt(RegData$FormDate, format='%Y-%m-%d %H:%M') >= as.POSIXlt('2016-10-01'))] <- 2

Del1 <- c('BehandlingHoeflighetRespektMedfoelelse',
          'SymptomSmerte',
          'SymptomPustebesvaer',
          'SymptomUro',
          'BehandlingBesvarerBehov',
          'BehandlingBesvarerStoette',
          'BehandlingSamarbeid',
          'BehandlingBesvarerHoeflighetRespektMedfoelelse',
          'SykepleierOmsorg',
          'SykepleierKommunikasjon',
          'LegeBehandling',
          'AtmosfaerenIntensivAvd',
          'AtmosfaerenPaaroerenderom',
          'OmfangetAvBehandlingen')
Del2 <- c('LegeInformasjonFrekvens',
          'SvarPaaSpoersmaal',
          'ForklaringForstaaelse',
          'InformasjonsAerlighet',
          'InformasjonOmForloep',
          'InformasjonsOverensstemmelse',
          'BeslutningsInvolvering',
          'BeslutningsStoette',
          'BeslutningsKontroll',
          'BeslutningsTid',
          'LivsLengde',
          'LivssluttKomfor',
          'LivssluttStoette')



#----- OM SPØRSMÅLENE----------
#-1: Ikke besvart 
#Del1: Alle spm 1-5, 6:ikke aktuelt
#Del2: Spm 1-6:  1-5, 6:ikke aktuelt
#Spm 7-13[-10]: 1-5, 
#Spm 10 1-2
#Dvs. alle spm har spenn 1-5, unntatt spm.10 del 2
#Spørsmål som skal snus: Del2, spm.7-13 1:5 = 0:100



library(plyr)
Del1Skaar <- paste0(Del1,'Skaar')
Del2Skaar <- paste0(Del2,'Skaar')
RegData[,c(Del1Skaar,Del2Skaar)] <- NA

#Standard: 1:5 -> 100:0
verdi5 <- c(100, 75, 50, 25, 0)

Spm <- c(Del1,Del2[1:6])
Skaar <- paste0(Spm,'Skaar')
for (nr in 1:length(Spm)) { RegData[,Skaar[nr]] <- mapvalues(RegData[ ,Spm[nr]], 
                                                             from = c(-1,1:6), to = c(NA,verdi5,NA))}

RegData[ ,Del2Skaar[10]] <- mapvalues(RegData[ ,Del2[10]], 
                                      from = c(-1,1:2), to = c(NA,0,100))

Spm <- Del2[c(7:9,11:13)]
Skaar <- paste0(Spm,'Skaar')
for (nr in 1:length(Spm)) { RegData[ ,Skaar[nr]] <-  mapvalues(RegData[ ,Spm[nr]], 
                                                               from = c(-1,1:5), to = c(NA,rev(verdi5)))}


#Each score is calculated by averaging available items, 
#provided the respondent answers at least 70% of the items in the respective scale
#NB: Legg inn sjekk på om nok observasjoner
#rowSums(is.na(RegData[ ,Del1Skaar])

RegData$OmsorgTot <- rowMeans(RegData[ ,Del1Skaar])
RegData$BeslutningTot <- rowMeans(RegData[ ,Del2Skaar[1:10]])
RegData$FSICUtot <- rowMeans(RegData[ ,c(Del1Skaar, Del2Skaar[1:10])])

write.table(RegData, file = 'A:/Intensiv/PaarorDataSkaar.csv', row.names=FALSE, sep = ';', fileEncoding = "UTF-8")


#FamiliefornoydTotScore: Overall family satisfaction score
#OmsorgTot: Satisfaction with care domain score
#MedvirknTot: Satisfaction with decision-making domain score
#InfoTot: Satisfaction with information domain score
#BeslutningTot: Satisfaction with the decision-making process domain score

#--------------------------- Figurtilrettelegging og figur--------------------------------------

#--------------start her____________
rm(list=ls())
#Last inn PaarorDataSkaar, Del1 og Del2
PaarorData <- read.table(file='A:/Intensiv/PaarorDataSkaar.csv', header=T, sep=';',encoding = 'UTF-8')
library(intensiv)
RegData <- NIRPreprosess(RegData=PaarorData)

#--------------Fordeling før og etter, samt tot. gjennomsnitt før og etter----------------------
#variable <- c('BehandlingHoeflighetRespektMedfoelelseNum', 'BehandlingBesvarerHoeflighetRespektMedfoelelseNum','InformasjonsOverensstemmelseNum', 'LegeInformasjonFrekvensNum')
variable <- c(Del1Skaar, Del2Skaar)

variable <- c(Del1, Del2)
setwd('C:/ResultattjenesteGIT/intensiv/doc/paarorendeSkjema/Figurer/')

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '.png')
      NIRFigPrePostPaaror(RegData=RegData, valgtVar=valgtVar, outfile=outfile)
}
valgtVar <- 'ForklaringForstaaelse'
NIRFigPrePostPaaror(RegData=RegData, valgtVar=valgtVar, outfile='')


#-------------- Sentralmål per sykehus----------------------
#Vise pre som prikk?
#	Gjennomsnittlig skår m/konf.int per sykehus. Velge pre post. Standardutvalg
#	Gjennomsnittlig endring i skår m/konf.int per enhet. Standardutvalg

setwd('C:/ResultattjenesteGIT/intensiv/doc/paarorendeSkjema/FigurerGjsn/')
Spm <- c(Del1,Del2[1:6])
Skaar <- c(paste0(Spm,'Skaar'), 'OmsorgTot', 'BeslutningTot','FSICUtot')
for (valgtVar in Skaar) {
      outfile <- paste0(valgtVar, '.png')
      NIRFigGjsnPaaror(RegData=RegData, valgtVar=valgtVar, outfile=outfile)
}
valgtVar <- 'OmsorgTot'
NIRFigGjsnPaaror(RegData=RegData, valgtVar=valgtVar, outfile='')

#RegData$PrePost
valgtVar <- 'BehandlingHoeflighetRespektMedfoelelseSkaar'

NIRFigGjsnPaaror(RegData=RegData, valgtVar=valgtVar, prePost=2, valgtMaal='Gjsn', outfile='EksempelGjsn.pdf')

            


