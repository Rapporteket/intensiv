#' Funksjon som gjør utvalg av anonymiserte data, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NIRFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalgOff <- function(RegData, aldGr=0, erMann='', InnMaate='', 
                         aar=0, grType=99, fargepalett='BlaaOff')    
{
      
      # Definer intersect-operator
      "%i%" <- intersect
      
      Ninn <- dim(RegData)[1]
      indAld <- if(aldGr[1] != 0) {which(RegData$aldGr %in% aldGr) } else {1:Ninn}
      indAar <- if (aar[1] != 0) {which(RegData$Aar %in% aar)} else {1:Ninn}
      #indKvart <- if (aar[1] != 0) {which(RegData$Kvartal %in% aar)} else {1:Ninn}
      indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {1:Ninn}
      #indInnMaate <- if (InnMaate %in% c(0,6,8)) {which(RegData$InnMaate == InnMaate)
      #      } else {1:Ninn}
      indGrType <- if (grType %in% 1:3) {switch(grType,
                                                '1' = which(RegData$ShType %in% 1:2),
                                                '2' = which(RegData$ShType %in% 1:2),
                                                '3' = which(RegData$ShType == 3))
      } else {indGrType <- 1:Ninn}
      
      indMed <- indAld %i% indKj %i% indGrType
      
      RegData <- RegData[indMed,]
      
      
      N <- dim(RegData)[1]	#N=0 gir feilmelding
      grTypetextstreng <- c('lokal/sentral', 'lokal/sentral', 'region')				
      if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}
   
      
      
      
      utvalgTxt <- c(
            if (aar[1] > 0){paste0('Innleggelsesår: ', paste0(aar, collapse=', '))},
            if (aldGr[1] > 0) {
                  paste0('Aldersgruppe: ', paste0(aldGr, collapse=', '))}, 
            if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
            if (InnMaate %in% c(0,6,8)) {paste('Innmåte: ', 
                                               c('Elektivt',0,0,0,0,0, 'Akutt medisinsk',0, 'Akutt kirurgi')[InnMaate+1], sep='')},
            if (grType %in% 1:3) {paste0('Sykehustype: ', grTypetextstreng[grType])}
      )
      
      

      UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, 
                     hovedgrTxt='Hele landet',smltxt='') #, grTypeTxt=grTypeTxt)
      return(invisible(UtData)) 
}