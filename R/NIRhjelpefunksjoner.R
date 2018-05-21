#' Fil som inneholder hjelpefunksjoner. 
#'
#' FinnReinnleggelser beregner reinnleggelser fra DateAdmittedIntensive og DateDischargedIntensive
#' @param RegData data
#' @param PasientID Variabelen som angir pasientidentifikasjon
#' 	
#' @return Variabelen Reinn lagt til datasettet
#' 
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


