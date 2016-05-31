#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#'
#' @inheritParams FigAndeler
#'
#' @return Data En liste med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export
#'
NIRPreprosess <- function(RegData=RegData, reshID=reshID)
{
  #Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer fra MRS/NHN.

  #Kjønn
  names(RegData)[which(names(RegData) == 'isMale')] <- 'erMann'
 #RegData$erMann[RegData$Kjonn == 'K'] <- 0
  #RegData$erMann[RegData$Kjonn == 'M'] <- 1
  
  #Riktig navn på regions-variabel:
	RegData$Region <- RegData$RHF

# Endre variabelnavn:
	#For enkelhetsskyld kalles Saps2Score som er Estimert mortalitet for SMR
	names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
	names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
	names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
	names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
	names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
	names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'
	names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
	
# Riktig format
	dodInt <- as.numeric(dodInt)
	RegData$ShNavn <- as.character(RegData$ShNavn)
	RegData$alder <- as.numeric(RegData$decimalAge)	#

	#Riktig format på datovariable:
	RegData <- RegData[which(RegData$DateAdmittedIntensive!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
	RegData$InnDato <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d") 
	RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d %H:%M:%S" )

  return(invisible(RegData))
}

