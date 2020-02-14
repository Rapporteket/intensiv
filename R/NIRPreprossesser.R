#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et "offentlig" datasett som kan benyttes til beregning
#' av kvalitetsindikatorer og som kan legges ved pakken
#'
#' @inheritParams NIRFigAndeler
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NIRPreprosess <- function(RegData=RegData, lagreKvalIndData=0)	#, reshID=reshID)
{
      #Miljøparametre
      #print(Sys.getlocale())
      #Sys.setlocale("LC_TIME", "nb_NO.UTF-8")   
      #print(paste('Etter at satt "nb_NO.UTF-8": ', Sys.getlocale()))
      
      #Kun ferdigstilte registreringer:
      # Fra des. 2018 får Intensiv også kladd over fra  fra MRS/NHN.
      RegData <- RegData[RegData$FormStatus==2, ]
      
      #devtools::load_all(quiet = TRUE)
      #source('R/NIRhjelpefunksjoner.R', encoding = 'UTF-8')
      #load_all(pkg = ".", reset = TRUE, recompile = FALSE, export_all = TRUE,
      #          quiet = FALSE, create = NA)
      #Kjønn
      RegData$erMann <- RegData$PatientGender #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 0] <- NA
      RegData$erMann[RegData$PatientGender == 2] <- 0
      
      #Riktig navn på regions-variabel:
      #...
      #	RegData$Region <- RegData$RHF
      
      # Endre variabelnavn:
      #For enkelhetsskyld kalles Saps2Score som er Estimert mortalitet for SMR
      #RegData$logit <- -7.7631 + 0.0737*RegData$Saps2ScoreNumber + 0.9971*log(RegData$Saps2ScoreNumber+1)
      #RegData$Mort <- exp(RegData$logit)/(1+exp(RegData$logit))*100 # = Saps2Score = SMR
      RegData$SapsSum <- with(RegData, Glasgow+Age+SystolicBloodPressure+HeartRate+Temperature+MvOrCpap+UrineOutput+
              SerumUreaOrBun+Leukocytes+Potassium+Sodium+Hco3+Bilirubin+TypeOfAdmission)
      head(RegData$SapsSum)
      head(RegData$Saps2ScoreNumber)
      names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
      names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
      names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      #	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
      names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
      names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
      names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'
      names(RegData)[which(names(RegData) == 'ReshID')] <- 'ReshId'
      #names(RegData)[which(names(RegData) == 'PatientInRegistryGuid')] <- 'PasientID'
      #Avvik ml. test og prod-data:
      names(RegData)[
            names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'
      
      # Riktig format
      RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) #Fjerner mellomrom (før) og etter navn
      RegData$ShType[RegData$ShType ==2 ] <- 1	#Har nå kun type lokal/sentral og regional
      
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
      RegData$Dod30 <- 0
      RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"), 
                                   as.Date(RegData$InnDato), units='days')< 30)] <- 1
      RegData$Dod90 <- 0
      RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"), 
                                   as.Date(RegData$InnDato), units='days')< 90)] <- 1
 
      RegData$Dod365 <- 0
      RegData$Dod365[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"), 
                                   as.Date(RegData$InnDato), units='days')< 365)] <- 1
      
      #Konvertere boolske variable fra tekst til boolske variable...
      TilLogiskeVar <- function(Skjema){
            verdiGML <- c('True','False')
            verdiNY <- c(TRUE,FALSE)
            mapping <- data.frame(verdiGML,verdiNY)
            LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
            if (length(LogVar)>0) {
                  for (k in 1:length(LogVar)) {
                        Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
                  }}
            return(Skjema)
      }
      
      RegData <- TilLogiskeVar(RegData)
      
      
      return(invisible(RegData))
}

