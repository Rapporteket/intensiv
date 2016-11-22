#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NIRFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalgEnh <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', InnMaate='', 
			grType=99, overfPas=99, dodInt='', fargepalett='BlaaOff')    
{

# Definer intersect-operator
 "%i%" <- intersect
      
 dodInt <- as.numeric(dodInt)

 
 #Enhetsutvalg:
 #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
 #trengs ikke data for hele landet:
 reshID <- as.numeric(reshID)
 indEgen1 <- match(reshID, RegData$ReshId)
 if (enhetsUtvalg %in% c(2,3,4,6,7)) {	
       RegData <- switch(as.character(enhetsUtvalg),
                         '2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
                         '3' = subset(RegData,ShType==ShType[indEgen1]),
                         '4' = RegData[which(RegData$ShType == RegData$ShType[indEgen1]),],	#kun egen shgruppe
                         '6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
                         '7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
 }
 
  

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




#Enhetsutvalg:
shTypetext <- c('lokale/sentrale', 'lokale/sentrale', 'regionale')				
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
      shtxt <- as.character(RegData$ShNavn[indEgen1]) } else {
            shtxt <- switch(as.character(enhetsUtvalg), 	
                            '0' = 'Hele landet',
                            '4' = shTypetext[RegData$ShType[indEgen1]],
                            '5' = shTypetext[RegData$ShType[indEgen1]],
                            '7' = as.character(RegData$Region[indEgen1]),
                            '8' = as.character(RegData$Region[indEgen1]))
      }

if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
      medSml <- 0
      ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
      ind$Rest <- NULL
} else {						#Skal gjøre sammenlikning
      medSml <- 1
      if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
            ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
                  ind$Hoved <- switch(as.character(enhetsUtvalg),
                                     '5' = which(RegData$ShType == RegData$ShType[indEgen1]),	#shgr
                                     '8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
      smltxt <- switch(as.character(enhetsUtvalg),
                       '1' = 'landet forøvrig',
                       '3' = paste0('andre ', shTypetext[RegData$ShType[indEgen1]]),	#RegData inneh. kun egen shgruppe
                       '5' = 'andre typer sykehus',
                       '6' = paste0(RegData$Region[indEgen1], ' forøvrig'),	#RegData inneh. kun egen region
                       '8' = 'andre regioner')
      ind$Rest <- switch(as.character(enhetsUtvalg),
                        '1' = which(as.numeric(RegData$ReshId) != reshID),
                        '3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
                        '5' = which(RegData$ShType != RegData$ShType[indEgen1]),
                        '6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
                        '8' = which(RegData$Region != RegData$Region[indEgen1]))
}								








#Hvis denne skal flyttes til utvalg. Må sende med følgende tilbake:
#medSml, smltxt, 

UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, ind=ind, medSml, smltxt)
return(invisible(UtData)) 
}