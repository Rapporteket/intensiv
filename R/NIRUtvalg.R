#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', InnMaate='', 
			dodInt='', fargepalett='BlaaOff')   #shType='alle', 
{

# Definer intersect-operator
 "%i%" <- intersect
      
#RegData <- RegData[indShType, ]
	

Ninn <- dim(RegData)[1]
indAld <- which(RegData$alder >= minald & RegData$alder <= maxald)
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
indInnMaate <- if (InnMaate %in% c(0,6,8)) {which(RegData$InnMaate == InnMaate)
				} else {indInnMaateUt <- 1:Ninn}
indDod <- if (dodInt %in% 0:1) {which(as.numeric(RegData$DischargedIntensiveStatus)==dodInt)
				} else {indDod <- 1:Ninn}
#indShType <- if (shType %in% c('lokal','sentral','region')) {switch(ShType,
#                    lokal = which(RegData$ShType == 1),
#                    sentral = which(RegData$ShType == 2),
#                    region = which(RegData$ShType == 3))
#            } else {indShType <- 1:Ninn}
indMed <- indAld %i% indDato %i% indKj %i% indInnMaate %i% indDod       #%i% indShType

RegData <- RegData[indMed,]


N <- dim(RegData)[1]	#N=0 gir feilmelding

utvalgTxt <- c(paste(
	'Registreringsperiode: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
	if ((minald>0) | (maxald<130)) {
		paste('Pasienter fra ', if (N>0) {sprintf('%.1f',min(RegData$alder, na.rm=T))} else {minald}, 
						' til ', if (N>0) {sprintf('%.1f',max(RegData$alder, na.rm=T))} else {maxald}, ' år', sep='')},
	if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
	if (InnMaate %in% c(0,6,8)) {paste('Innmåte: ', 
			c('Elektivt',0,0,0,0,0, 'Akutt medisinsk',0, 'Akutt kirurgi')[InnMaate+1], sep='')},
#	if (ShType %in% c('lokal','sentral','region')) {paste('Sykehustype: ', ShType, sep='')},
	if (dodInt %in% 0:1) {paste('Status ut fra intensiv: ', c('Levende','Død')[as.numeric(dodInt)+1], sep='')}
)


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
return(invisible(UtData)) 
}