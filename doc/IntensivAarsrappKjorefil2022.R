#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

rm(list=ls())
library(intensiv)
aarsrappAar <- 2022
datoFra <- '2011-01-01'
datoTil <- paste0(aarsrappAar, '-12-31')
datoFra1aar <- paste0(aarsrappAar, '-01-01')
setwd('~/Aarsrappresultater/NiPar22/intensiv')
RegData <- NIRPreprosess(NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil))
RegData1aar <- NIRPreprosess(NIRRegDataSQL(datoFra=datoFra1aar, datoTil=datoTil))
#Fjernes for årsrapp 2022: Telemark 100132 - ingen registreringer

## DATA HENTET 24.april 2023
#SkjemaGUID 6A5D7672-A706-426C-9900-D760AC9EA61B manglerresh/enhetstilhørighet

# RegData <- NIRPreprosess(RegData)
# table(RegData$ShNavn, RegData$Aar)
# test <- unique(RegData[ ,c('ShNavn', 'ReshId')])
# test[order(test$ShNavn),]
# table(test$ShNavn)


#--------------------------------------- Fordelinger ----------------------------------

#Fjernet apr. -22, 'respiratortidInvMoverf',  #'CerebralCirculationAbolishedReasonForNo', 'frailtyIndex', 'komplikasjoner',


variabler <- c('OrganDonationCompletedReasonForNoStatus',
               'frailtyIndex', 'inklKrit','liggetid','InnMaate',
              'NEMS24', 'Nas24', 'regForsinkelse', 'respiratortidNonInv',
              'SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak')

for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar,
                 outfile=outfile)
}

NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar='CerebralCirculationAbolishedReasonForNo',
              outfile='CerebralCirculationAbolishedReasonForNo_Ford.pdf')

#Lagt til mai -23
variabler <- 'komplikasjoner' #c('frailtyIndex', )
for (grType in 2:3) {
  for (valgtVar in variabler) {
    outfile <- paste0(valgtVar, '_', grType, 'Ford.pdf')
    NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, 
                       Ngrense=10, grType=grType, outfile=outfile)
  }
}

  # NIRFigAndeler(RegData=RegData1aar, preprosess = 0, valgtVar='spesTiltak', grType = 3,
  #             outfile='spesTiltak_ford.pdf')

#------ AndelTid-ingen ---------------------------------------------

#---------------------GjsnTid----------------------------------------------
# Alder, hele landet
# Liggetid, hele landet
# Liggetid, død
# SAPSII

variabler <- c('NEMS', 'respiratortid', 'alder', 'liggetid', 'SAPSII')

for (valgtVar in variabler) {
  outfile <- paste0(valgtVar, 'MedTid.pdf')
  NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
                valgtMaal='Med', tidsenhet= 'Aar', outfile=outfile)
}


#--------------------------------------- AndelGrVar ----------------------------------
#Ny mai 2023: variabler <- 'komplReg' 'frailtyIndex', 'komplReg', 'potDonor'
variabler <- c('dod30d', 'frailtyIndex', 'komplikasjoner', 
               'OrganDonationCompletedCirc', 'OrganDonationCompletedStatus',
               'potDonor', 'regForsinkelse', 'reinn', 'trakeostomi')

for (grType in 2:3) {
      for (valgtVar in variabler) {
            outfile <- paste0(valgtVar, grType, 'PrSh.pdf')
            NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, 
                               Ngrense=10, grType=grType, outfile=outfile)
      }
}

NIRFigAndelerGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='OrganDonationCompletedCirc', 
                     Ngrense=10, outfile='OrganDonationCompletedCircPrSh.pdf')


# #Organdonorer av døde: OrganDonationCompletedStatus
# #Organdonorer, av alle med opphevet intrakran. sirk.': 'OrganDonationCompletedCirc',

#--------------------------------------- SENTRALMÅL per enhet----------------------------------

variabler <- c('alder',  'NEMS24', 'Nas24',
              'respiratortidInvMoverf',  'respiratortidNonInv', 'SAPSII')
  #Lagt til mai -23: variabler <- 'frailtyIndex'
for (grType in 2:3) {
      for (valgtVar in variabler){ #
            outfile <- paste0(valgtVar,grType, '_MedPrSh.pdf')
            NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar=valgtVar, valgtMaal='Med',
                         grType=grType, outfile=outfile)
      }
}

NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='Nas24', valgtMaal='Med')

#KvalInd:
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='SMR'
                ,outfile='SMR_PrSh.pdf')

#KvalInd:
#årsrapp 22: Etter litt fram og tilbake endte vi på 'respiratortidInvUoverf'. 
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidInvUoverf', valgtMaal='Med',
                outfile='respiratortidInvUoverf_MedPrSh.pdf')




# Figurar for gjennomsnittleg og median respiratortid for non-invasiv og invasiv respiratorstøtte med overførte pasientar. 
#Figurer per sykehus
#Tar ut apr02: NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Med',
#                 datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidInvMoverf_MedPrSh.pdf')
#Tar ut apr02: NIRFigGjsnGrVar(RegData=RegData, valgtVar='respiratortidInvMoverf', valgtMaal='Gjsn',
#                 datoFra=datoFra1aar, datoTil=datoTil, outfile='respiratortidInvMoverf_GjsnPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Med',
                outfile='respiratortidNonInv_MedPrSh.pdf')
NIRFigGjsnGrVar(RegData=RegData1aar, preprosess = 0, valgtVar='respiratortidNonInv', valgtMaal='Gjsn',
                outfile='respiratortidNonInv_GjsnPrSh.pdf')


# #-- Pårørende----------------------------------------------------------
#
# RegData <- NIRRegDataSQL(datoFra = datoFra1aar, datoTil = datoTil) #, session = session) #datoFra = datoFra, datoTil = datoTil)
# PaarorData <- NIRpaarorDataSQL(datoFra = datoFra1aar)
# PaarorDataH <- KobleMedHoved(RegData, PaarorData, alleHovedskjema=F, alleSkjema2=F)
# PaarorDataH <- NIRPreprosess(PaarorDataH)
#
# Totalskaarer <- c('SumScoreSatisfactionCare', 'SumScoreSatisfactionDecision', 'SumScoreAllQuestions')
# Del1 <- c('BehandlingHoeflighetRespektMedfoelelse', 'SymptomSmerte', 'SymptomPustebesvaer',
#           'SymptomUro', 'BehandlingBesvarerBehov', 'BehandlingBesvarerStoette',
#           'BehandlingSamarbeid', 'BehandlingBesvarerHoeflighetRespektMedfoelelse',
#           'SykepleierOmsorg', 'SykepleierKommunikasjon', 'LegeBehandling',
#           'AtmosfaerenIntensivAvd', 'AtmosfaerenPaaroerenderom', 'OmfangetAvBehandlingen')
# Del2 <- c('LegeInformasjonFrekvens', 'SvarPaaSpoersmaal', 'ForklaringForstaaelse',
#           'InformasjonsAerlighet', 'InformasjonOmForloep', 'InformasjonsOverensstemmelse',
#           'BeslutningsInvolvering', 'BeslutningsStoette', 'BeslutningsKontroll',
#           'BeslutningsTid', 'LivsLengde', 'LivssluttKomfor', 'LivssluttStoette')
# variable <- c(Del1, Del2, Totalskaarer)
#
# for (valgtVar in variable) {
#    outfile <- paste0('Paaror', valgtVar, '_Ford.pdf')
#    # NIRFigPrePostPaaror(RegData=PaarorDataH, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
#    #               outfile=outfile, preprosess = 0)
#    NIRFigAndeler(RegData=PaarorDataH, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
#                        outfile=outfile, preprosess = 0)
# }

#-------------------------------Tall 2022--------------------------------
# For alle intensivpasientar i 2022
# Gjennomsnitt og median alder med KI
# GjSn: 62,5 KI: 62,2-62,8
# Median: 68,4 KI:68,1-68,7
# 
# Del over 80 år: 16,9%
# under 18 år:5,5%
# 
# Gjennomsnitt invasiv respiratortid med KI:
#   UTEN overførte: 0,9 KI 0,8-1,0
#   
# Del nyreerstattande behandling (totalt): 
#   Andel av opphold: 5,5%
# Del døde ved utskriving frå intensiv: 10,5%
# Del døde etter 30 dagar: 21,3%
# 
# 
# For pandemipasientar på intensiv i 2022 (beredskap)
# (Hentet fra Rapporteket-Intensiv og filtrert på Covid-pasienter)
# Del kvinner og menn: 735/1160
# Median alder med KI: 66,5 KI: 65,5-67,5
# Del døde ved utskriving frå intensiv: 18,9%
# Del døde etter 30 dagar: 31,6%


  


#-------------------------------Tabeller--------------------------------
#Belegg
library(intensiv)
library(xtable)
#RegData1aar <- NIRPreprosess(RegData1aar)

tabBeleggN <- rbind(
            'Ferdigstilte intensivopphald' = tapply(RegData1aar$PasientID, RegData1aar$ShType, FUN=length),
            'Registrerte pasientar' = tapply(RegData1aar$PasientID, RegData1aar$ShType,
                                             FUN=function(x) length(unique(x))),
            'Tal intensivdøger' = round(as.numeric(tapply(RegData1aar$liggetid, RegData1aar$ShType, sum, na.rm=T)),0),
            'Gjennomsnittleg liggjetid' = round(tapply(RegData1aar$liggetid, RegData1aar$ShType, mean, na.rm=T),1)
      )
tabBeleggNtot <- cbind(tabBeleggN, c(rowSums(tabBeleggN)[1:3], round(mean(RegData1aar$liggetid, na.rm=T),1)))
colnames(tabBeleggNtot) <- c('lokal-/sentral', 'region', 'alle')

xtable(tabBeleggNtot, digits=0, align=c('l', rep('r', ncol(tabBeleggNtot))),
       caption= paste0('Antal opphald og liggedøger i ',aarsrappAar, '.'), label='tab:RegEget')



#Antall opphold
library(lubridate)
xtable(table(RegData1aar$ShNavn), align=c('l','r'), #row.names=F,
       caption = paste0('Intensivopphald per eining i ', aarsrappAar, '.'))





#Fordeling av kjønn per sykehustype og år
tabShTypeAar <- table(RegData$Aar, RegData$ShType)
tabKj <- table(RegData[RegData$erMann==1 , c('Aar', 'ShType')])
kjLandet <- prop.table(table(RegData[ , c('Aar', "erMann")]),1)
AndelMenn <- 100*cbind(tabKj/tabShTypeAar,
                       kjLandet[,'1'])
#AndelMennShType <- prop.table(table(RegData[ , c("erMann",'ShType')]),2)[2,]
AndelMenn <- rbind(AndelMenn,
                 'Alle år' = 100*c(prop.table(table(RegData[ , c("erMann",'ShType')]),2)[2,],
                            prop.table(table(RegData[ , "erMann"]))[2])
)

colnames(AndelMenn) <- c('Lok./Sentral', 'Region', 'Hele landet')
xtable(AndelMenn, digits=1, align=c('l', rep('r', ncol(AndelMenn))),
       caption='Del (prosent) av intensivopphald som er menn.', label='tab:KjonnAar')


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

tabNokkeltall <- tabNokkeltall(RegData=RegData1aar, datoTil=datoTil) #, tidsenhet='Mnd'
xtable::xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
               label = 'tab:nokkel',
               caption = paste0('Samla tal på intensivopphald og aktivitet i NIR, ', aarsrappAar))


for (nivaa in 1:3) {
   nivaa
   RegDataNivaa <- RegData1aar[RegData1aar$Nivaa == nivaa, ]
   tabNokkeltall <- tabNokkeltall(RegData=RegDataNivaa, datoTil=datoTil) #, tidsenhet='Mnd'
   print(xtable(tabNokkeltall, digits= 1, align=c('l', rep('r', ncol(tabNokkeltall))), #row.names=F,
          caption = paste0('Samla tal på intensivopphald og aktivitet i NIR, einingar nivå ', nivaa)))
}
#Legg til  \resizebox{\columnwidth}{!}{ \begin{tabular}... }



#--------------------------------------Data til offentlig visning (SKDE, Resultatportalen)-------------------------------------
setwd('~/Aarsrappresultater/NETTsider/')
library(intensiv)
library(magrittr)
NIRData <- NIRPreprosess(RegData = NIRRegDataSQL(datoFra = '2016-01-01'))
NIRData <- NIRData[-which(NIRData$ShNavn ==''), ]

#nyResh <- setdiff(unique(NIRData$ReshId), names(nyID))
#unique(NIRData[which(NIRData$ReshId %in% nyResh),c("ShNavn", "ReshId")])

ind1 <- dataTilOffVisning(RegData = NIRData, valgtVar='reinn', 
                                 indID = 'intensiv_innlegg_72t', filUt = 'innlegg_72t')

ind2 <- dataTilOffVisning(RegData = NIRData, valgtVar='respiratortidInvUoverf', # respiratortidInvMoverf
                                 indID = 'intensiv_inv_vent', filUt = 'inv_vent')

NIRindFraReg <- rbind(ind1, ind2)

write.table(NIRindFraReg, file = 'NIRindFraReg.csv', sep = ';', row.names = F)
#tapply(DataTilSKDE$var, INDEX = DataTilSKDE$year, FUN = mean)



#----Kvalitetsindikatorer på enhetsnivå
KvalIndFil <- read.table(file = 'KvalindNIR_2022_manuell.csv',fileEncoding = 'UTF-8', sep = ';', header = TRUE) #, row.names = FALSE)
#nye <- setdiff(unique(as.character(KvalIndFil$resh_id)), names(nyID))
#KvalIndFil[which(KvalIndFil$resh_id %in% nye), c("resh_id", "namn")]

#Dataomorganisering
RegData <- KvalIndFil[, c("resh_id", "tverrfagleg_gjennomgang", "rutinenotat", "primarvakt", "data_nir")]
RegData$primarvakt <- dplyr::recode(RegData$primarvakt, '2' = 1L, '3'= 0L) #1-ja, 2-nei Innh: -1,1,2,3
variabler <- c( "tverrfagleg_gjennomgang", "rutinenotat",  "data_nir")
RegData[ , variabler][RegData[,variabler] == 2] <- 0
RegData$orgnr <- as.character(nyID[as.character(RegData$resh_id)])
#table(RegData$orgnr, useNA = 'a')

RegDataUt <- tidyr::pivot_longer(
  data = RegData[,-1],
  cols = c("tverrfagleg_gjennomgang", "rutinenotat", "primarvakt", "data_nir"),
  names_to = 'ind_id'
  ,values_to = 'var'
)
#table(RegDataUt$var, useNA = 'a')
RegDataUt <- RegDataUt[-which(RegDataUt$var == -1), ]

RegDataUt$ind_id <- paste0('intensiv_', RegDataUt$ind_id)
RegDataUt$denominator <- 1
RegDataUt$year <- 2022
RegDataUt$context <- 'caregiver'
write.table(RegDataUt, file = 'KvalIndEnhNivaa.csv', sep = ';', row.names = F)

