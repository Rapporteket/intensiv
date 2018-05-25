# Må det kanskje komme en overornet tittel her?
#---------------------------------------------

#' Hjelpefunksjoner. Group of functions page title
#' 
#' Fil med div hjelpefunksjoner.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' @section Finne reinnleggelser After function section:
#' Despite its location, this actually comes after the function section.
#' Fil som inneholder hjelpefunksjoner. 
#' FinnReinnleggelser beregner reinnleggelser fra DateAdmittedIntensive og DateDischargedIntensive
#' 
#' @param RegData data
#' @param PasientID Variabelen som angir pasientidentifikasjon
# @inheritParams NIRFigAndeler
#' @return Div hjelpefunksjoner
#' @name hjelpeFunksjoner
NULL
#' @rdname hjelpeFunksjoner
#' @export

FinnReinnleggelser <- function(RegData, PasientID='PasientID'){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive og PasientID
      #SJEKK Bare innleggelser fra 2016 som skal ha reinnleggelse??
      #RegData <- RegData[
      #      as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d %H:%M:%S") >= as.POSIXlt('2016-01-01'), ]
      
      #TabAntOpph <- table(RegData$PasientID) #Tar relativt lang tid.
      #TabFlereOpph <- TabAntOpph[TabAntOpph>1]
      #indPasFlereOpph <- which(RegData$PasientID %in% names(TabFlereOpph))  #Tar relativt lang tid.
      RegDataSort <- RegData[order(RegData[ ,PasientID], RegData$DateAdmittedIntensive,     #Denne tar mest tid
                                   RegData$DateDischargedIntensive), ]
      #RegDataSort$AntOpph <- ave(RegDataSort$PasientID, RegDataSort$PasientID, FUN=length)
      RegDataSort$OpphNr <- ave(RegDataSort[ ,PasientID], RegDataSort[ ,PasientID], FUN=seq_along)
      indPasFlereOpph <- which(RegDataSort$OpphNr>1) #intersect(which(RegDataSort$AntOpph>1), which(RegDataSort$OpphNr>1))
      RegDataSort$TidUtInn <- NA
      RegDataSort$TidUtInn[indPasFlereOpph] <- 
            difftime(as.POSIXlt(RegDataSort$DateAdmittedIntensive[indPasFlereOpph], format="%Y-%m-%d %H:%M:%S"),
                     as.POSIXlt(RegDataSort$DateDischargedIntensive[indPasFlereOpph-1], format="%Y-%m-%d %H:%M:%S"),
                     units = 'hour')
      RegDataSort$Reinn <- 2 #Ikke reinnleggelse
      RegDataSort$Reinn[RegDataSort$TidUtInn<72 & RegDataSort$TidUtInn >= 0] <- 1 #Reinnleggelse
      
      #Div testing:
      # indNeg <- which(RegDataSort$TidUtInn < 0)
      # TabDobbeltRegSjekk <- RegDataSort[sort(c(indNeg-1,indNeg)), ]
      # write.table(TabDobbeltRegSjekk, file='TabDobbeltRegSjekk.csv', row.names = F, sep = ';')
      # write.table(RegDataSort, file='RegDataSort.csv', row.names = F, sep = ';')
      # RegDataSort[1:20,]               
      # table(RegDataSort$ReAdmitted)
      # table(RegDataSort$Reinn)
      # RegDataSort$PasientID[1:20,]               
      # RegDataSort$TidUtInn[indNeg[1:5]]
      return(RegDataSort)
}

#' @section Tilrettelegge tidsenhetvariabel:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#' @rdname hjelpeFunksjoner
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar') {
      #Lager sorteringsvariabel for tidsenhet:
      RegData$TidsEnhetSort <- switch(tidsenhet,
                                      Aar = RegData$Aar-min(RegData$Aar)+1,
                                      Mnd = RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
                                      +(RegData$Aar-min(RegData$Aar))*12,
                                      Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*4,
                                      Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*2
      )
      
      tidtxt <- switch(tidsenhet,
                       Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                   sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))
      
      RegData$TidsEnhetSort <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort))
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE, labels=tidtxt)
      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}
