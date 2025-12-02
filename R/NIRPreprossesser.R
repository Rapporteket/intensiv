#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et "offentlig" datasett som kan benyttes til beregning
#' av kvalitetsindikatorer og som kan legges ved pakken
#'
#' @inheritParams NIRFigAndeler
#' @param skjema hvilket skjema data som skal preprosesseres tilhører
#' 1: hoved, 2: paaror, 3: influ, 4: beredsk (beredsk har egen preprosess-fil)
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NIRPreprosess <- function(RegData=RegData, skjema=1)	#, reshID=reshID)
{
  # Enhetsnivånavn
  RegData$RHF <- factor(RegData$RHF,
                        levels= c('Helse Nord', 'Helse Midt-Norge', 'Helse Vest', 'Helse Sør-Øst', 'Privat'),
                        labels = c('Nord', 'Midt', 'Vest', 'Sør-Øst', 'Privat'))


      #Kun ferdigstilte registreringer:
  if (skjema %in% 1:2){
      RegData <- RegData[RegData$FormStatus==2, ]}

      #Kjønn
      RegData$erMann <- RegData$PatientGender #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 0] <- NA
      RegData$erMann[RegData$PatientGender == 2] <- 0


      # Endre variabelnavn:
      #For enkelhetsskyld kalles Saps2Score som er Estimert mortalitet for SMR
      #RegData$logit <- -7.7631 + 0.0737*RegData$Saps2ScoreNumber + 0.9971*log(RegData$Saps2ScoreNumber+1)
      #RegData$Mort <- exp(RegData$logit)/(1+exp(RegData$logit))*100 # = Saps2Score = SMR

     if (skjema==1){
        RegData$SapsSum <- with(RegData, Glasgow+Age+SystolicBloodPressure+HeartRate+Temperature+MvOrCpap+UrineOutput+
              SerumUreaOrBun+Leukocytes+Potassium+Sodium+Hco3+Bilirubin+TypeOfAdmission)
        RegData[which(RegData$AgeAdmitted<16), c('SapsSum', 'Saps2Score', 'Saps2ScoreNumber')] <- 0
      }

      message("NIRPreprosess: Navnsetting av variabler")
      names(RegData)[which(names(RegData) == 'AgeAdmitted')] <- 'Alder' #Én desimal
      names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
      names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
      names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'Liggetid'
      names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
      names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
      names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
      names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'


      #Henter tilgangstre og mapper om resh og ShNavn
      message('Henter tilgangstre fra MRS og mapper om resh og ShNavn')
      TilgJsn <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL")
      if (TilgJsn == "") {
        stop("NIRPreprosess: Miljøvariabel MRS_ACCESS_HIERARCHY_URL er ikke satt")
      }
      Tilgangstre <- jsonlite::fromJSON(TilgJsn)$AccessUnits
      varTilg <- c("UnitId", "ParentUnitId", "HasDatabase", "ExternalId", "Title", "TitleWithPath","ExtraData")
      IntData <- merge(RegData, Tilgangstre[ ,varTilg],
                       by.x = 'ReshId', by.y = 'UnitId', suffixes = c('Int','Tilg'))
      RegData <- dplyr::rename(IntData,
                               Nivaa = ExtraData,
                               ReshIdReg = ReshId,
                               ReshId = ExternalId,
                               ShNavnReg = ShNavn,
                               ShNavn = Title) #newname = oldname
      RegData$NivaaNum <- as.numeric(plyr::mapvalues(RegData$Nivaa, from=c('1a', '1b', '2b', '3', '3c'),
                                          to = 1:5))

      #Fjerner mellomrom (før) og etter navn
      RegData$ShNavn <- trimws(as.character(RegData$ShNavn))

      #Sjekker om alle resh har egne enhetsnavn
      message("NIRPreprosess: Sjekker om alle resh har egne enhetsnavn")
      dta <- unique(RegData[ ,c('ReshId', 'ShNavn')])
      duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId)>1)])
      duplSh <- names(table(dta$ShNavn)[which(table(dta$ShNavn)>1)])

      #Tomme sykehusnavn får resh som navn:
      indTom <- which(is.na(RegData$ShNavn)) # | RegData$ShNavn == '')
      RegData$ShNavn[indTom] <- RegData$ReshId[indTom]

       message("NIRPreprosess: Setter dupliserte sykehusnavn til ReshId")
      if (length(c(duplSh, duplResh)) > 0) {
        ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$ShNavn %in% duplSh))
        RegData$ShNavn[ind] <- paste0(RegData$ShNavn[ind],' (', RegData$ReshId[ind], ')')
      }

      #Riktig format på datovariable:
      #	RegData <- RegData[which(RegData$DateAdmittedIntensive!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
      RegData$InnDato <- as.Date(RegData$DateAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d")
      RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$DateAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d %H:%M" ) #:%S
      RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC', format="%Y-%m-%d %H:%M" )

      # Nye variabler:
      RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
      RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
      RegData$UkeAar <- format(RegData$Innleggelsestidspunkt, 'uke%V.%g')
      RegData$Kvartal <- ceiling(RegData$MndNum/3)
      RegData$Halvaar <- ceiling(RegData$MndNum/6)
      RegData$Aar <- 1900 + RegData$Innleggelsestidspunkt$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year

      ##Kode om  pasienter som er overført til/fra egen avdeling til "ikke-overført"
      #1= ikke overført, 2= overført
      ind <- union(which(RegData$ReshId == RegData$PatientTransferredFromHospital),
                   which(RegData$ReshId == RegData$PatientTransferredToHospital))
      RegData$Overf[ind] <- 1


      #En "overlever": Person som er i live 30 dager etter innleggelse.
      if (skjema %in% c(1,3)){
      RegData$Dod30 <- 0
      RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d"), # %H:%M:%S
                                   as.Date(RegData$InnDato), units='days')< 30)] <- 1
      RegData$Dod90 <- 0
      RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d"), #  %H:%M:%S
                                   as.Date(RegData$InnDato), units='days')< 90)] <- 1

      RegData$Dod365 <- 0
      RegData$Dod365[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d"), # %H:%M:%S
                                   as.Date(RegData$InnDato), units='days')< 365)] <- 1

      }

# Angi om Covid-pasient før luftveisvariabel ble innført (okt -2025)
      qCovid <- paste0('SELECT UPPER(HovedskjemaGUID) AS HovedskjemaGUID, Diagnosis
                FROM beredskap')
      CovidData <- rapbase::loadRegData(registryName= "data", query=qCovid, dbType="mysql")
      # CovidData$Bekreftet <- 0
      # CovidData$Bekreftet[which(CovidData$Diagnosis %in% 100:103)] <- 1
      # RegData <- merge(RegData, CovidData[ ,-which(names(CovidData) == 'Diagnosis')], suffixes = c('','Cov'),
      #                by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)
      # match(c(9,4,7), c(10,2, 0, 3, 2, 5, 9, 7))

      indCov <- match(CovidData$HovedskjemaGUID, RegData$SkjemaGUID, nomatch = NA)
      RegData$SARS_CoV2[indCov] <- 1

return(invisible(RegData))
}

