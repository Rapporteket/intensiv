#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

rm(list=ls())
library(intensiv)
datoFra <- '2011-01-01'
datoTil <- '2020-12-31'	#
datoFra1aar <- '2020-01-01'
#------Klargjøre årsrapportfil-------------
RegData <- read.table('A:/Intensiv/NIRAarsrapp2019_2020-09-09.csv', sep=';', stringsAsFactors=FALSE,
                      header=T, encoding = 'UTF-8')
 indDato <- intersect(which(as.Date(RegData$DateAdmittedIntensive) >= as.Date(datoFra, tz= 'UTC')),
                         which(as.Date(RegData$DateAdmittedIntensive) <= as.Date(datoTil, tz= 'UTC')))
# indDato <- intersect(which(RegData$DateAdmittedIntensive >= datoFra),
#                      which(RegData$DateAdmittedIntensive <= datoTil))
RegData <- RegData[indDato,]
save(RegData, file = 'A:/Intensiv/NIRaarsrapp2019.RData')
RegDataAarCSV <- NIRPreprosess(RegData)
write.table(RegDataAarCSV, file = 'A:/Intensiv/NIRaarsrapp2019.csv', row.names = F, col.names=T,
            fileEncoding = 'UTF-8', sep = ';')

RegData <- NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil)
#load("A:/Intensiv/NIRaarsrapp2019.Rdata")
#PaarorDataH <- PaarorDataH2018
#load('A:/Intensiv/PaarorRegData2018.RData')
setwd('/home/rstudio/intensiv/aarsrappOff')


#Innkomstmåte (egen fig.) reg/sentLok
NIRFigInnMaate(RegData=RegData, datoFra=datoFra1aar, datoTil = datoTil,
               grType=1, outfile='InnMaateLokSen.pdf')
NIRFigInnMaate(RegData=RegData, valgtVar='InnMaate', datoFra=datoFra1aar, datoTil = datoTil,
               grType=3, outfile='InnMaateReg.pdf')

#--------------------------------------- Fordelinger ----------------------------------

#NIRFigAndeler(RegData=NIRRegDataSQL(), valgtVar='komplikasjoner', enhetsUtvalg = 0)
variable <- c('OrganDonationCompletedReasonForNoStatus', 'CerebralCirculationAbolishedReasonForNo',
              'frailtyIndex', 'inklKrit','liggetid','InnMaate','komplikasjoner',
              'NEMS24', 'Nas24', 'regForsinkelse', 'respiratortidNonInv',
                   'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak') #, 'respiratortidInvMoverf')
variable <-'regForsinkelse'
for (valgtVar in variable) {
   outfile <- paste0(valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                 outfile=outfile)
}


NIRFigAndeler(RegData=RegData, valgtVar='liggetid', dodInt=1, datoFra=datoFra1aar, datoTil=datoTil,
              outfile='liggetidDod_ford.pdf')
NIRFigAndeler(RegData=RegData, valgtVar='spesTiltak', datoFra=datoFra1aar, datoTil=datoTil, grType = 3,
              outfile='spesTiltak_ford.pdf')
#Pårørende - ikke gjort for 2019
#--------------------------------------- AndelGrVar ----------------------------------
# Reinnleggelser reg/sentlok
# Død innen 30 dager reg/sentLok
# Trakeostomi reg/lokSent


variable <- c('OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
              'dod30d', 'dodeIntensiv', 'frailtyIndex', 'trakeostomi', 'regForsinkelse', 'reinn', 
              'komplReg', 'invasivVent', 'trakeostomi')
variable <-'dod30d'
for (grType in 2:3) {
      for (valgtVar in variable) {
            outfile <- paste0(valgtVar, grType, 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, Ngrense=10,
                            datoTil=datoTil, grType=grType, outfile=outfile)
      }
}
# #Organdonorer av døde: OrganDonationCompletedStatus
# #Organdonorer, av alle med opphevet intrakran. sirk.': 'OrganDonationCompletedCirc',
# NIRFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, Ngrense=10,
#                    datoTil=datoTil, grType=grType, outfile=outfile)


#---------------------AndelTid----------------------------------------------

variable <- c('dod30d', 'liggetidDod')

for (valgtVar in variable){
      outfile <- paste0(valgtVar, '_AndelTid.pdf')
      NIRFigAndelTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                     tidsenhet = 'Aar', outfile=outfile)
}

#---------------------GjsnTid----------------------------------------------
# Alder, hele landet
# Liggetid, hele landet
# Liggetid, død
# SAPSII

valgtMaal <- 'Med'
variable <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')


for (valgtVar in variable) {
      outfile <- paste0(valgtVar, 'MedTid.pdf')
      NIRFigGjsnTid(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                    valgtMaal=valgtMaal, tidsenhet= 'Aar', outfile=outfile)
}
NIRFigGjsnTid(RegData=RegData, valgtVar='liggetid', datoFra=datoFra, datoTil=datoTil,
              valgtMaal=valgtMaal, dodInt=1, outfile='liggetidDod_MedTid.pdf')

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
#variable <- 'respiratortidNonInv'	#'SMR', alder, liggetid, respiratortid,  SAPSII, 'NEMS', 'Nas24'
#Nye: respiratortidInvMoverf, respiratortidInvUoverf, respiratortidNonInv
variable <- c('alder', 'liggetid', 'respiratortid','NEMS', 'NEMS24', 'Nas24',
              'respiratortidInvMoverf', 'respiratortidNonInv', 'SAPSII',
              'respiratortidInvUoverf')
for (grType in 2:3) {
      for (valgtVar in variable){ #
            outfile <- paste0(valgtVar,grType, '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData, valgtVar=valgtVar, valgtMaal=valgtMaal,
                         grType=grType, datoFra=datoFra1aar, datoTil=datoTil, outfile=outfile)
      }
      NIRFigGjsnGrVar(RegData=RegData, valgtVar='SMR', grType=grType,
                      datoFra=datoFra1aar, datoTil=datoTil, outfile=paste0(valgtVar,grType, '_PrSh.pdf'))
      NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1,
                      grType=grType, datoFra=datoFra1aar, datoTil=datoTil,
                      outfile=paste0('liggetidDod',grType,'_MedPerSh.pdf'))
}
NIRFigGjsnGrVar(RegData=RegData, valgtVar='SMR',
                datoFra=datoFra1aar, datoTil=datoTil, outfile='SMR_PrSh.pdf')

NIRFigGjsnGrVar(RegData=RegData, valgtVar='liggetid', valgtMaal=valgtMaal, dodInt=1,
                grType=3, datoFra=datoFra1aar, datoTil=datoTil, outfile='liggetidDod3_MedPerSh.pdf')

# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar. Figurer per sykehus
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Med',
                datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidInvMoverf_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Gjsn',
                datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidInvMoverf_GjsnPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidNonInv', valgtMaal='Med',
                datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidNonInv_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidNonInv_GjsnPrSh.pdf')



#-- Pårørende----------------------------------------------------------

RegData <- NIRRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil) #, session = session) #datoFra = datoFra, datoTil = datoTil)
PaarorData <- NIRpaarorDataSQL(datoFra = datoFra1aar)
PaarorDataH <- KobleMedHoved(RegData, PaarorData, alleHovedskjema=F, alleSkjema2=F)
PaarorDataH <- NIRPreprosess(PaarorDataH)

Totalskaarer <- c('SumScoreSatisfactionCare', 'SumScoreSatisfactionDecision', 'SumScoreAllQuestions')
Del1 <- c('BehandlingHoeflighetRespektMedfoelelse', 'SymptomSmerte', 'SymptomPustebesvaer',
          'SymptomUro', 'BehandlingBesvarerBehov', 'BehandlingBesvarerStoette',
          'BehandlingSamarbeid', 'BehandlingBesvarerHoeflighetRespektMedfoelelse',
          'SykepleierOmsorg', 'SykepleierKommunikasjon', 'LegeBehandling',
          'AtmosfaerenIntensivAvd', 'AtmosfaerenPaaroerenderom', 'OmfangetAvBehandlingen')
Del2 <- c('LegeInformasjonFrekvens', 'SvarPaaSpoersmaal', 'ForklaringForstaaelse',
          'InformasjonsAerlighet', 'InformasjonOmForloep', 'InformasjonsOverensstemmelse',
          'BeslutningsInvolvering', 'BeslutningsStoette', 'BeslutningsKontroll',
          'BeslutningsTid', 'LivsLengde', 'LivssluttKomfor', 'LivssluttStoette')
variable <- c(Del1, Del2, Totalskaarer)

for (valgtVar in variable) {
   outfile <- paste0('Paaror', valgtVar, '_Ford.pdf')
   # NIRFigPrePostPaaror(RegData=PaarorDataH, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
   #               outfile=outfile, preprosess = 0)
   NIRFigAndeler(RegData=PaarorDataH, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                       outfile=outfile, preprosess = 0)
}

#-------------------------------Tabeller--------------------------------
#Belegg
library(xtable)
RegData1aar <- NIRPreprosess(RegData)
Utvalg <- NIRUtvalgEnh(RegData1aar, datoFra = datoFra1aar, datoTil = datoTil)
RegData1aar <- Utvalg$RegData

tabBeleggN <- rbind(
            'Ferdigstilte intensivopphald' = tapply(RegData1aar$PasientID, RegData1aar$ShType, FUN=length),
            'Registrerte pasientar' = tapply(RegData1aar$PasientID, RegData1aar$ShType,
                                             FUN=function(x) length(unique(x))),
            'Tal intensivdøger' = round(as.numeric(tapply(RegData1aar$liggetid, RegData1aar$ShType, sum, na.rm=T)),0)
      )
tabBeleggNtot <- cbind(tabBeleggN, rowSums(tabBeleggN))
colnames(tabBeleggNtot) <- c('lokal-/sentral', 'region', 'alle')

xtable(tabBeleggNtot, digits=0, align=c('l', rep('r', ncol(tabBeleggNtot))),
       caption='Antal opphald og liggedøger i 2019.', label='tab:RegEget')

#NIRFigGjsnTid(RegData = RegData1aar, valgtVar = 'NEMS', tidsenhet = 'Mnd', valgtMaal = 'Med', outfile = 'NEMStest.pdf')
library(lubridate)
#Antall opphold
tabDum <- tabAntOpphShMnd(RegData=RegData1aar, datoTil=datoTil, antMnd=12)
table(RegData1aar$ShNavn) #[ ,c('ShNavn' ,'Aar')])
RegData1aar$ShNavn
xtable(table(RegData1aar$ShNavn), align=c('l','r'), #row.names=F,
       caption = 'Intensivopphald per år')





#Fordeling av kjønn per sykehustype og år
RegDataPre <- NIRPreprosess(RegData)
tabShTypeAar <- table(RegDataPre$Aar, RegDataPre$ShType)
tabKj <- table(RegDataPre[RegDataPre$erMann==1 , c('Aar', 'ShType')])
kjLandet <- prop.table(table(RegDataPre[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
#AndelMennShType <- prop.table(table(RegDataPre[ , c("erMann",'ShType')]),2)[2,]
AndelMenn <- rbind(AndelMenn,
                 'Alle år' = 100*c(prop.table(table(RegDataPre[ , c("erMann",'ShType')]),2)[2,],
                            prop.table(table(RegDataPre[ , "erMann"]))[2])
)

colnames(AndelMenn) <- c('Lok./Sentral', 'Region', 'Hele landet')
xtable(AndelMenn, digits=1, align=c('l', rep('r', ncol(AndelMenn))),
       caption='Andel (prosent) av oppholdene som er menn.', label='tab:KjonnAar')


#Aktivitet/Nøkkeltall

#Legge til nivå
Nivaa <- c(
   '4205696' = '1', # Akershus univ.sykehus, Postoperativ
   '108897' = '1', # Diakonhjemmet sykehus, Postop/intensivavd 
   '100180' = '1', # Haraldsplass diakonale sykehus, Med intensiv postoperativ
   '106271' = '1', # Haukeland, KSK Postoperativ
   '107717' = '1', # Haukeland, Lunge 1,Respiratorisk Overvakingseining (ROE)
   '4208892' = '1', # Lovisenberg Diakonale, MIO
   '705757' = '1', # OUS, Radiumhospitalet - Postop og intensivavd 
   '109779' = '1', # OUS, Ullevål - Nevrointensiv
   '109778' = '1', # OUS, Ullevål - Postoperativ
   '4205574' = '1', # St. Olavs Hospital - Medisin- og lungeovervåkning 
   '102090' = '2', # Akershus univ.sykehus, Intensiv (generell)
   '105282' = '2', # Førde sentralsjukehus
   '101858' = '2', # Hammerfest Sykehus, Akuttmed avd, Intensiv
   '100273' = '2', # Haugesund sjukehus, Intensiv
   '109363' = '2', # Haukeland, Brannskadeavdelinga 
   '106285' = '2', # Haukeland, KSK Thoraxkirurgisk intensiv (TIO)
   '105048' = '2', # Haukeland, Medisinsk intensiv og overvaking (MIO)
   '103015' = '2', # Helgelands. Mo i Rana
   '103141' = '2', # Helgelands. Mosjøen
   '103149' = '2', # Helgelands.Sandnessjøen
   '102250' = '2', # Helse NT Levanger
   '105893' = '2', # Helse NT Namsos
   '101830' = '2', # Kirkenes sykehus, Akuttmed avd, Intesniv
   '706078' = '2', # Kristiansund sykehus, Intensiv (felles)
   '706079' = '2', # Molde sykehus, Intensiv (felles)
   '4210053' = '2', # Nordlandssykehuset, Bodø - Intensivavdelingen (generell)
   '110867' = '2', # Nordlandssykehuset, Vesterålen - Intensiv Stokmarknes
   '111487' = '2', # OUS, Aker - Postoperativ og intensiv 
   '111449' = '2', # OUS, Ullevål - Barneintensiv
   '109877' = '2', # OUS, Ullevål - Hjertekirurgisk postoperativ
   '4205969' = '2', # OUS, Ullevål - Hjertemedisinsk intensiv
   '109870' = '2', # OUS, Ullevål - Medisinsk intensiv
   '106572' = '2', # St. Olavs Hospital - Hjertemedisinsk intensiv 
   '114282' = '2', # Stavanger univ.sjkehus - Intensiv 2M (generell) 
   '4208715' = '2', # Sykehuset i Kongsberg, Intensiv
   '103948' = '2', # Sykehuset i Vestfold, Tønsberg, Intensivmed. seksjon  
   '108609' = '2', # Sykehuset Innlandet, Akuttmed. Elverum
   '108618' = '2', # Sykehuset Innlandet, Akuttmed. Gjøvik
   '108610' = '2', # Sykehuset Innlandet, Akuttmed. Hamar,
   '108626' = '2', # Sykehuset Innlandet, Akuttmed. Lillehammer 
   '102026' = '2', # Sykehuset Telem. Skien, Akuttmed avd, Intensv 
   '4209889' = '2', # Sykehuset Østfold Kalnes, Intensiv
   '104450' = '2', # Sørlandet s. Arendal, Intensivenheten 
   '114240' = '2', # Sørlandet s. Kristiansand, Intensivenheten
   '700617' = '2', # UNN, Harstad - Intensiv 
   '700618' = '2', # UNN, Narvik
   '601302' = '2', # UNN, Tromsø Medisinsk intensiv- og hjerteoppvåkning
   '103090' = '2', # Vestre Viken HF, Bærum sykehus, Int.
   '103620' = '2', # Vestre Viken HF, Drammen sykeh. Avd. for anest. og int.med
   '103539' = '2', # Vestre Viken, HF, Ringerike sykeh. Intensivavdelingen 
   '4209764' = '2', # Volda sjukehus, Intensiv (felles)
   '108308' = '2', # Ålesund sjukehus Kir.int.
   '102673' = '2', # Ålesund sjukehus Med.int.
   '112044' = '3', # Haukeland, KSK Intensiv (Generell)
   '705576' = '3', # OUS, Rikshospitalet - Barneintensiv
   '706929' = '3', # OUS, Rikshospitalet - Generell Intensiv. 2
   '705577' = '3', # OUS, Rikshospitalet - Generell Intensiv 1
   '109773' = '3', # OUS, Ullevål - Generell intensiv 
   '4201313' = '3', # St Olavs Hospital - Hovedintensiv 
   '700720' = '3' # UNN, Tromsø - Intensivavdelingen (generell intensiv)
)

RegData1aar$Nivaa <- as.character(Nivaa[as.character(RegData1aar$ReshId)])
# head(RegData1aar$Nivaa)
# table(RegData1aar$Nivaa, useNA = 'a')
# unique(RegData1aar[is.na(RegData1aar$Nivaa), c("ReshId", "ShNavn")])
# table(RegData1aar[is.na(RegData1aar$Nivaa), c("ReshId", "ShNavn")])
# table(RegData1aar$ReshId[is.na(RegData1aar$Nivaa)])
# RegData <- NIRPreprosess(RegData = RegData)
# ftable(RegData[,c('ShNavn', 'Aar')])
# sjekk <- unique(RegData[, c("ReshId", "ShNavn")])
# sjekk[order(as.character(sjekk$ShNavn)),]

tabNokkeltall <- tabNokkeltall(RegData=RegData1aar, datoTil=datoTil) #, tidsenhet='Mnd'
xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
       caption = 'Samla tal på intensivopphald og aktivitet i NIR')

for (nivaa in 1:3) {
   nivaa
   RegDataNivaa <- RegData1aar[RegData1aar$Nivaa == nivaa, ]
   tabNokkeltall <- tabNokkeltall(RegData=RegDataNivaa, datoTil=datoTil) #, tidsenhet='Mnd'
   print(xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
          caption = paste0('Samla tal på intensivopphald og aktivitet i NIR, einingar nivå ', nivaa)))
}
#Legg til  \resizebox{\columnwidth}{!}{ \begin{tabular}... }



#--------------------------------------Data til offentlig visning (SKDE, Resultatportalen)-------------------------------------

library(intensiv)
library(magrittr)
NIRData <- NIRPreprosess(RegData = NIRRegDataSQL(datoFra = '2016-01-01', datoTil = '2020-12-31'))

#valgteAar <- 2016:2020

DataTilSKDE <- dataTilOffVisning(RegData = NIRData, valgtVar='reinn', #aar=valgteAar,
                                 ResPort=0, indID = 'intensiv_innlegg_72t', filUt = 'innlegg_72t')

DataTilSKDE <- dataTilOffVisning(RegData = NIRData, valgtVar='respiratortidInvUoverf', #aar=valgteAar, #'respiratortidInvMoverf'
                                 ResPort=0, indID = 'intensiv_inv_vent', filUt = 'inv_vent')

tapply(DataTilSKDE$var, INDEX = DataTilSKDE$year, FUN = mean)






