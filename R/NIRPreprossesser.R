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
      #Miljøparametre
      #print(Sys.getlocale())
      #Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
      #print(paste('Etter at satt "nb_NO.UTF-8": ', Sys.getlocale()))

  # RegData1 <- rapbase::loadRegData(registryName="nir", query='SELECT * FROM mainformdatacontract', dbType="mysql") #intensiv::NIRRegDataSQL()
  # RegData2 <- rapbase::loadRegData(registryName="nir", query='SELECT * FROM questionaryformdatacontract', dbType="mysql")
  # RegData3 <- rapbase::loadRegData(registryName="nir", query='SELECT * FROM influensaformdatacontract', dbType="mysql")
  # RegData4 <- rapbase::loadRegData(registryName="nir", query='SELECT * FROM readinessformdatacontract', dbType="mysql")


  #Boolske variabler ser ut til å være er endret til 0-1 i ny ekstraktor mars-25
  # LogVarSjekk <- names(RegData)[unique(which(RegData[1,] %in% c('True','False')),
  #                                      which(RegData[dim(RegData)[1]-15,] %in% c('True','False')))]
  # LogVar <- unique(c(LogVarSjekk,
  #                    "Eeg", "EcmoEcla", "Hyperbar", "Iabp", "Icp", "Impella", "Intermitterende",
  #                    "KompHypoglykemi", "KompPneumotoraks", "KompLuftveisproblem",
  #                    "KompDekubitus", "KomIngen", "KompIkkeUtfylt", "Kontinuerlig", "Leverdialyse",
  #                    "No", "Oscillator", "Sofa", "TerapetiskHypotermi"))
  # 
  # RegData[, intersect(names(RegData), LogVar)] <-
  #   apply(RegData[, intersect(names(RegData), LogVar)], 2, as.logical)

      #Kun ferdigstilte registreringer:
      # Fra des. 2018 får Intensiv også kladd over fra  fra MRS/NHN. 1.april 2021 - alle er fortsatt ferdigstilte...
  if (skjema %in% 1:2){
      RegData <- RegData[RegData$FormStatus==2, ]}

      #Kjønn
      RegData$erMann <- RegData$PatientGender #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 0] <- NA
      RegData$erMann[RegData$PatientGender == 2] <- 0

      #Riktig navn på regions-variabel:
      #	RegData$Region <- RegData$RHF

      # Endre variabelnavn:
      #For enkelhetsskyld kalles Saps2Score som er Estimert mortalitet for SMR
      #RegData$logit <- -7.7631 + 0.0737*RegData$Saps2ScoreNumber + 0.9971*log(RegData$Saps2ScoreNumber+1)
      #RegData$Mort <- exp(RegData$logit)/(1+exp(RegData$logit))*100 # = Saps2Score = SMR
      if (skjema==1){
        #Boolske variabler var tidligere tekst ('True','False'). Endret til teksten 0-1 (mars -25)
        #LogVarSjekk <- names(RegData)[which(RegData[1,] %in% c('True','False'))]
        # LogVar <- c("Eeg", "EcmoEcla", "Hyperbar", "Iabp", "Icp", "Impella", "Intermitterende",
        #                    "Kontinuerlig", "Leverdialyse", "No", "Oscillator", "Sofa", "TerapetiskHypotermi")
        #Fra kodeboka:
        
        
        
        

RegData$SapsSum <- with(RegData, Glasgow+Age+SystolicBloodPressure+HeartRate+Temperature+MvOrCpap+UrineOutput+
              SerumUreaOrBun+Leukocytes+Potassium+Sodium+Hco3+Bilirubin+TypeOfAdmission)
        RegData[which(RegData$AgeAdmitted<16), c('SapsSum', 'Saps2Score', 'Saps2ScoreNumber')] <- 0
      }

      names(RegData)[which(names(RegData) == 'AgeAdmitted')] <- 'Alder' #Én desimal
      names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
      names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
      names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
      names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
      names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
      names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'
      # names(RegData)[
      #   names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'

     # names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId' 


      # Riktig format
      if (skjema %in% 1:3){
        RegData$ShType[RegData$ShType ==2 ] <- 1	#Har nå kun type lokal/sentral og regional
      }
      
      #Fjerner mellomrom (før) og etter navn
      RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) 
      #Sjekker om alle resh har egne enhetsnavn
      dta <- unique(RegData[ ,c('ReshId', 'ShNavn')])
      duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId)>1)])
      duplSh <- names(table(dta$ShNavn)[which(table(dta$ShNavn)>1)])
      
      #Tomme sykehusnavn får resh som navn:
      indTom <- which(is.na(RegData$ShNavn)) # | RegData$ShNavn == '')
      RegData$ShNavn[indTom] <- RegData$ReshId[indTom]
      
      if (length(c(duplSh, duplResh)) > 0) {
        ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$ShNavn %in% duplSh))
        RegData$ShNavn[ind] <- paste0(RegData$ShNavn[ind],' (', RegData$ReshId[ind], ')')
      }
      
      #Riktig format på datovariable:
      #	RegData <- RegData[which(RegData$DateAdmittedIntensive!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
      RegData$InnDato <- as.Date(RegData$DateAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d")
      RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$DateAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d %H:%M:%S" )
      #RegData$InnDato <- strptime(RegData$DateAdmittedIntensive, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
      RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC', format="%Y-%m-%d %H:%M:%S" )

      # Nye variable:
      RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
      RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
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
      RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
                                   as.Date(RegData$InnDato), units='days')< 30)] <- 1
      RegData$Dod90 <- 0
      RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
                                   as.Date(RegData$InnDato), units='days')< 90)] <- 1

      RegData$Dod365 <- 0
      RegData$Dod365[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
                                   as.Date(RegData$InnDato), units='days')< 365)] <- 1

      }

  return(invisible(RegData))
}

