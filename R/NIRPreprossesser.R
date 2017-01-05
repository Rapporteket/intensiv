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
	names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
	names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
	names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
	names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
	names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'
	names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
#	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
	names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
	
# Riktig format
	RegData$ShNavn <- as.character(RegData$ShNavn)

	#Riktig format på datovariable:
#	RegData <- RegData[which(RegData$DateAdmittedIntensive!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
	RegData$InnDato <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d") 
	RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d %H:%M:%S" )
	#RegData$InnDato <- strptime(RegData$DateAdmittedIntensive, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
	
	# Nye variable:
	RegData$Aar <- 1900 + strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
	#En "overlever": Person som er i live 30 dager etter innleggelse.
	RegData$Dod30 <- 0
	RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"), 
	                              as.Date(RegData$InnDato), units='days')< 30)] <- 1
	
	
      ### Lager "anonymt" minimumsdatasett basert på rådata for å beregne kvalitetsindikatorer. 
	  # Alternativt kan man lage et 01-datasett, men vi vil da trenge egen beregning for dette datasettet.
	  # Enhet, RHF, sykehustype, kjønn, aldersgruppe, år, samt variable som inngår i kvalitetsindikatorene
	KvalInd <- c('ReAdmitted', 'Overf', 'SAPSII', 'SMR', 'respiratortid')
	OffDataKvalInd <- RegData[ ,c('Aar', 'erMann', 'ShNavn', 'ShType', KvalInd)]
	gr <- c(0, 18, 40,60,80,150)		
	OffDataKvalInd$AldersGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
	levels(OffDataKvalInd$AldersGr) <- c('0-17','18-39','40-59','60-79','80+')
	OffDataKvalInd <- OffDataKvalInd[which(RegData$Aar >= 2012),]
	
	test <- ftable(OffDataKvalInd[,c('AldersGr','erMann', 'ShNavn', 'Aar')])
	#Andel som mistes hvis tar bort <5: 
	      sum(test[which(test<5 & test>0)])/dim(OffDataKvalInd)[1]*100
	#Lagre beregnede data
	if (lagreKvalIndData==1) {
	save(OffDataKvalInd, file='data/OffDataKvalInd.RData')
	}
	
return(invisible(RegData))
}

