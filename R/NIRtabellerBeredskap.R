#' Funksjoner for å lage tabeller 
#' 
#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#' 
#' @param RegData data
#' @param datoTil sluttdato
#' @param reshID enhetens resh
#' @param pasientID pasientidentifikasjon, personentydig
#'
#' @export
RisikofaktorerTab <- function(RegData, datoTil=Sys.Date(), reshID=0){
  
    TabRisiko <- cbind(
      Kreft = tapply(CoroData$Kreft, CoroData$Dag, FUN=sum, na.rm = T),
      'Nedsatt immunforsvar' = tapply(CoroData$IsImpairedImmuneSystemIncludingHivPatient, CoroData$Dag, FUN=sum, na.rm = T),
      Diabetes	= tapply(CoroData$Diabetes, CoroData$Dag, FUN=sum, na.rm = T),
      Hjertesykdom = tapply(CoroData$IsHeartDiseaseIncludingHypertensionPatient, CoroData$Dag, FUN=sum, na.rm = T),
      'Fedme (KMI>30)' =	tapply(CoroData$IsObesePatient, CoroData$Dag, FUN=sum, na.rm = T),
      Astma	= tapply(CoroData$Astma, CoroData$Dag, FUN=sum, na.rm = T),
      'Kronisk lungesykdom' = tapply(CoroData$IsChronicLungDiseasePatient, CoroData$Dag, FUN=sum, na.rm = T),
      Nyresykdom =	tapply(CoroData$IsKidneyDiseaseIncludingFailurePatient, CoroData$Dag, FUN=sum, na.rm = T),
      Leversykdom = tapply(CoroData$IsLiverDiseaseIncludingFailurePatient, CoroData$Dag, FUN=sum, na.rm = T),
      'Nevrologisk/nevromusk.' = tapply(CoroData$IsChronicNeurologicNeuromuscularPatient, CoroData$Dag, FUN=sum, na.rm = T),
      Graviditet	= tapply(CoroData$Graviditet, CoroData$Dag, FUN=sum, na.rm = T),
      'Røyker' =	tapply(CoroData$IsActivSmoker, CoroData$Dag, FUN=sum, na.rm = T),
      'Opphold med risikofaktorer' = tapply(CoroData$IsRiskFactor, CoroData$Dag, FUN=sum, na.rm = T)
    )
    
    xtable::xtable(addmargins(t(TabRisiko), margin = 2),
                   digits=0,
                   caption='Risikofaktorer')
    return(TabRisiko)
}
