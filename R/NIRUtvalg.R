#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NIRFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', InnMaate='', 
			grType=99, overfPas=99, dodInt='', fargepalett='BlaaOff')    
{

# Definer intersect-operator
 "%i%" <- intersect
      
 dodInt <- as.numeric(dodInt)
 

Ninn <- dim(RegData)[1]
indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
indInnMaate <- if (InnMaate %in% c(0,6,8)) {which(RegData$InnMaate == InnMaate)
				} else {indInnMaateUt <- 1:Ninn}
indDod <- if (dodInt %in% 0:1) {which(as.numeric(RegData$DischargedIntensiveStatus)==dodInt)
				} else {indDod <- 1:Ninn}
indGrType <- if (grType %in% 1:3) {switch(grType,
                    '1' = which(RegData$ShType %in% 1:2),
                    '2' = which(RegData$ShType %in% 1:2),
                    '3' = which(RegData$ShType == 3))
            } else {indGrType <- 1:Ninn}

#indGrType <- if (grType %in% c('lokal','sentral','region')) {switch(grType,
#                    lokal = which(RegData$grType == 1),
#                    sentral = which(RegData$grType == 2),
#                    region = which(RegData$grType == 3))
#            } else {indGrType <- 1:Ninn}
indMed <- indAld %i% indDato %i% indKj %i% indInnMaate %i% indDod %i% indGrType

RegData <- RegData[indMed,]


N <- dim(RegData)[1]	#N=0 gir feilmelding
grTypetext <- c('lokal/sentral', 'lokal/sentral', 'region')				

utvalgTxt <- c(paste(
	'Registreringsperiode: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
	if ((minald>0) | (maxald<130)) {
		paste('Pasienter fra ', if (N>0) {sprintf('%.1f',min(RegData$Alder, na.rm=T))} else {minald}, 
						' til ', if (N>0) {sprintf('%.1f',max(RegData$Alder, na.rm=T))} else {maxald}, ' år', sep='')},
	if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
	if (InnMaate %in% c(0,6,8)) {paste('Innmåte: ', 
			c('Elektivt',0,0,0,0,0, 'Akutt medisinsk',0, 'Akutt kirurgi')[InnMaate+1], sep='')},
	if (grType %in% 1:3) {paste('Sykehustype: ', grTypetext[grType], sep='')},
	if (dodInt %in% 0:1) {paste('Status ut fra intensiv: ', c('Levende','Død')[as.numeric(dodInt)+1], sep='')}
)


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
return(invisible(UtData)) 
}