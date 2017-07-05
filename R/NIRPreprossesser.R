#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et "offentlig" datasett som kan benyttes til beregning
#' av kvalitetsindikatorer og som kan legges ved pakken
#'
#' @inheritParams NIRAndeler
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NIRPreprosess <- function(RegData=RegData, lagreKvalIndData=0)	#, reshID=reshID)
{
  #Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer fra MRS/NHN.

  #Kjønn
  RegData$erMann <- NULL
  RegData$erMann[RegData$PatientGender == 'Female'] <- 0
  RegData$erMann[RegData$PatientGender == 'Male'] <- 1
  
  #Riktig navn på regions-variabel:
  #Mangler regionsvariabel!!!
#	RegData$Region <- RegData$RHF

# Endre variabelnavn:
	#For enkelhetsskyld kalles Saps2Score som er Estimert mortalitet for SMR
	names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
	names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
	names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
#	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
	names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
	names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
	names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
	names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
	names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'
	names(RegData)[which(names(RegData) == 'ReshID')] <- 'ReshId'
	
# Riktig format
	RegData$ShNavn <- as.character(RegData$ShNavn)

	#Riktig format på datovariable:
#	RegData <- RegData[which(RegData$DateAdmittedIntensive!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
	RegData$InnDato <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d") 
	RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d %H:%M:%S" )
	#RegData$InnDato <- strptime(RegData$DateAdmittedIntensive, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
	
	# Nye variable:
	RegData$Aar <- 1900 + RegData$InnDato$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
	RegData$Mnd <- paste(RegData$InnDato$year-100,RegData$InnDato$mon+1, sep='.')
	verdiGML <- 0:11
	verdiNY <- c(1,1,1,2,2,2,3,3,3,4,4,4)
	mapping <- data.frame(verdiGML,verdiNY)
	RegData$Kvartal <- paste(RegData$InnDato$year-100, 
	                         mapping$verdiNY[match(RegData$InnDato$mon, mapping$verdiGML)], sep='.')
	
	
	#En "overlever": Person som er i live 30 dager etter innleggelse.
	RegData$Dod30 <- 0
	RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"), 
	                              as.Date(RegData$InnDato), units='days')< 30)] <- 1
	
	
	 
return(invisible(RegData))
}

