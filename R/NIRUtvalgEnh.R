#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NIRFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalgEnh <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', InnMaate='', 
                         grType=99, enhetsUtvalg=0, reshID=0, overfPas=99, dodInt='', fargepalett='BlaaOff')    
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
      grTypetextstreng <- c('lokal/sentral', 'lokal/sentral', 'region')				
      if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}
      
      
      utvalgTxt <- c(paste0(
            'Registreringsperiode: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra}, 
            ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
            if ((minald>0) | (maxald<130)) {
                  paste0('Pasienter fra ', if (N>0) {sprintf('%.1f',min(RegData$Alder, na.rm=T))} else {minald}, 
                        ' til ', if (N>0) {sprintf('%.1f',max(RegData$Alder, na.rm=T))} else {maxald}, ' år')},
            if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
            if (InnMaate %in% c(0,6,8)) {paste('Innmåte: ', 
                                               c('Elektivt',0,0,0,0,0, 'Akutt medisinsk',0, 'Akutt kirurgi')[InnMaate+1], sep='')},
            if (grType %in% 1:3) {paste0('Sykehustype: ', grTypetextstreng[grType])},
            if (dodInt %in% 0:1) {paste0('Status ut fra intensiv: ', c('Levende','Død')[as.numeric(dodInt)+1])}
      )
      
      
      #Enhetsutvalg:
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
            hovedgrTxt <- as.character(RegData$ShNavn[indEgen1]) } else {
                  hovedgrTxt <- switch(as.character(enhetsUtvalg), 	
                                  '0' = 'Hele landet',
                                  '4' = grTypetextstreng[RegData$ShType[indEgen1]],
                                  '5' = grTypetextstreng[RegData$ShType[indEgen1]],
                                  '7' = as.character(RegData$Region[indEgen1]),
                                  '8' = as.character(RegData$Region[indEgen1]))
            }
      
      
      ind <- list(Hoved=0, Rest=0)
      smltxt <- grTypeTxt      #Før: ''
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
                             '3' = paste0('andre ', grTypetextstreng[RegData$ShType[indEgen1]]),	#RegData inneh. kun egen shgruppe
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
      
      
      
      
      UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, ind=ind, 
                     medSml=medSml, hovedgrTxt=hovedgrTxt,smltxt=smltxt, grTypeTxt=grTypeTxt)
      return(invisible(UtData)) 
}