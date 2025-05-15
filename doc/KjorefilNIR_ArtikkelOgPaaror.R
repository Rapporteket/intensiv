#_________________________________________________________________________________________________________
# _________________________________________________________________________________________________________

#                               P Å R Ø R E N D E 
#-----------------------------------------------------------------------------------------------


library(intensiv)
rm(list=ls())
load("A:/Intensiv/NIRdataPaaror.RData") #RegDataTEST, 2018-06-05
# HovedSkjema <- read.table(file='A:/Intensiv/mainformdatacontract2018-10-02.csv', header=T, sep=';',encoding = 'UTF-8')
#  HovedSkjema <- RegData
#  PaarorData <- read.table(file='A:/Intensiv/questionaryformdatacontract2018-10-02.csv', header=T, sep=';',encoding = 'UTF-8')
# PaarorData$PreskjemaGUID <- toupper(PaarorData$HovedskjemaGUID)

# hovedVar <- c('SkjemaGUID','DateAdmittedIntensive', 'DaysAdmittedIntensiv','Respirator','TransferredStatus', 
#               'Saps2Score','Saps2ScoreNumber', 'TypeOfAdmission', 'Nems', 'Morsdato', 
#               'PatientTransferredFromHospital', 'PatientTransferredToHospital', 'ShNavn', 'ShType',
#               'DateDischargedIntensive')
# varUT <- which(names(PaarorData) %in% c('Forslag', 'Kommentar', 'Personalet'))
# RegData <- merge(PaarorData[ ,-varUT], HovedSkjema[ ,hovedVar], 
#                  by.x='HovedskjemaGUID', by.y='SkjemaGUID', all.x = TRUE, all.y = FALSE)
#RegData <- NIRPreprosess(RegData)

#Definerer variabel hvor 1 - før intervensjon, 2-etter intervensjon (innleggelse f.o.m. 1.okt. 2016)
#Fyrste måleperiode var pasientar innlagde 01.10.15-31.12.15 og andre 01.10.16-31.12.16.  
#Basaldata gjeld altså data for alle opphald før 01.10.16, og kontroll etter intervensjon er 
#data for alle opphald etter 01.10.16.
#Dette må senere gjøres om til et brukervalg, dvs. at brukeren kan velge hvilke perioder 
#som skal sammenlignes


#FamiliefornoydTotScore: Overall family satisfaction score
#OmsorgTot: Satisfaction with care domain score
#MedvirknTot: Satisfaction with decision-making domain score
#InfoTot: Satisfaction with information domain score
#BeslutningTot: Satisfaction with the decision-making process domain score


#------------------FS-ICU, artikkel pårørendetilfredshet--------------------------------
library(intensiv)
datoPre1 <- '2015-10-01'
datoPre2 <- '2015-12-31'
datoPost1 <- '2016-10-01'
datoPost2 <- '2016-12-31'

   Hoved <- NIRRegDataSQL(datoFra= datoPre1, datoTil = datoPost2) #, session = session) #datoFra = datoFra, datoTil = datoTil)
   PaarorData <- NIRpaarorDataSQL() #datoFra= datoPre1, datoTil = datoPost2) #, medH=1) Tar grusomt lang tid
   PaarorDataH <- KobleMedHoved(Hoved, PaarorData, alleHovedskjema=F, alleSkjema2=F)
   PaarorDataH <- NIRPreprosess(RegData = PaarorDataH) #Må først koble på hoveddata for å få ShType++

#Tar bort skjema registrert i "opplæringsperioden"
indMellom <- which(PaarorDataH$InnDato > as.Date(datoPre2) & PaarorDataH$InnDato < as.Date(datoPost1) )
PaarorDataH <- PaarorDataH[-indMellom, ]

indPre <- which(PaarorDataH$InnDato >= as.Date(datoPre1) & PaarorDataH$InnDato <= as.Date(datoPre2))
indPost <- which(PaarorDataH$InnDato >= as.Date(datoPost1) & PaarorDataH$InnDato <= as.Date(datoPost2))
PaarorDataH$Post <- NA #'mellom'
PaarorDataH$Post[indPre] <- 0 #'pre'
PaarorDataH$Post[indPost] <- 1 #'post'
table(PaarorDataH$Post, useNA = 'a')

write.table()

#Pasientkarakteristikker

InnMaateTab <- table(PaarorDataH$InnMaate, PaarorDataH$Post)
rownames(InnMaateTab) <- c('Planlagt operasjon','Akutt non-operativ', 'Akutt operasjon')

#PrimaryReasonAdmitted ble innført 01.01.2016
# PrimData <- NIRVarTilrettelegg(RegData = PaarorDataH, valgtVar = 'PrimaryReasonAdmitted')
# table(PaarorDataH$PrimaryReasonAdmitted, PaarorDataH$Post)
# PrimAarsakTab <- table(PrimData$RegData$VariabelGr, PrimData$RegData$Post)
# rownames(PrimAarsakTab) <- PrimData$grtxt

Tab <- round(rbind('Antall pasienter' = c(length(indPre), length(indPost)),
   'Alder, gj.sn' = c(mean(PaarorDataH$Alder[indPre], na.rm = T), 
                      mean(PaarorDataH$Alder[indPost], na.rm = T)),
   'SAPSII' = c(mean(PaarorDataH$SMR[indPre], na.rm = T), 
                mean(PaarorDataH$SMR[indPost], na.rm = T)),
   InnMaateTab,
'Liggetid (t), median' = c(median(PaarorDataH$liggetid[indPre], na.rm = T), 
                           median(PaarorDataH$liggetid[indPost], na.rm = T)),
'Død på intensivavd.' = c(sum(PaarorDataH$DischargedIntensiveStatus[indPre], na.rm = T), 
                      sum(PaarorDataH$DischargedIntensiveStatus[indPost], na.rm = T))
), 1)

colnames(Tab) <- c('pre', 'post')
xtable::xtable(Tab, digits=1, align=c('l','r','r'))
#Hovedårsak, innleggelse - vis figur?


table(PaarorDataH$SumScoreAllQuestions != -1, PaarorDataH$Post)
table(PaarorDataH$SumScoreSatisfactionCare != -1, PaarorDataH$Post)
table(PaarorDataH$SumScoreSatisfactionDecision != -1, PaarorDataH$Post)
#--------------------------- Figurtilrettelegging og figur--------------------------------------

rm(list=ls())
#Last inn PaarorDataSkaar, Del1 og Del2
#PaarorData <- read.table(file='A:/Intensiv/PaarorDataSkaar.csv', header=T, sep=';',encoding = 'UTF-8')
library(intensiv)
load("A:/Intensiv/NIRdataPaaror.RData") #RegDataTEST, 2018-10-02

#--------------Fordeling før og etter, samt tot. gjennomsnitt før og etter----------------------
#variable <- c('BehandlingHoeflighetRespektMedfoelelseNum', 'BehandlingBesvarerHoeflighetRespektMedfoelelseNum','InformasjonsOverensstemmelseNum', 'LegeInformasjonFrekvensNum')
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
totSkaarer <-  c('SumScoreSatisfactionCare', 'SumScoreSatisfactionDecision', 'SumScoreAllQuestions')

# Del1Skaar <- paste0(Del1,'Skaar')
# Del2Skaar <- paste0(Del2,'Skaar')
# variable <- c(Del1Skaar, Del2Skaar)
variable <- c(Del1, Del2, totSkaarer)
setwd('C:/ResultattjenesteGIT/intensiv/')
test <- NIRPreprosess(RegData)

valgtVar <- 'BeslutningsTid'
NIRFigPrePostPaaror(RegData=RegData, valgtVar=valgtVar, datoTil='2017-08-01' , outfile='')

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '.png')
      NIRFigPrePostPaaror(RegData=RegData, valgtVar=valgtVar, outfile=outfile)
}

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

            


