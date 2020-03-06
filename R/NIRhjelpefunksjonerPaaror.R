#' Fil som inneholder hjelpefunksjoner. 
#' 
#' 
#' Beregner skårer fra pårørendeskjema (FS-ICU). NB: Ikke tatt høyde for at 
#' minst 70% av spm. må være fylt ut for at en skår skal beregnes
#' 
#' @param RegData data
#' @export

beregneSkaarer <- function(RegData){
  library(plyr)
  
  Del1 <- c('BehandlingHoeflighetRespektMedfoelelse',
            'SymptomSmerte',
            'SymptomPustebesvaer',
            'SymptomUro',
            'BehandlingBesvarerBehov',
            'BehandlingBesvarerStoette',
            'BehandlingSamarbeid',
            'BehandlingBesvarerHoeflighetRespektMedfoelelse',
            'SykepleierOmsorg',
            'SykepleierKommunikasjon',
            'LegeBehandling',
            'AtmosfaerenIntensivAvd',
            'AtmosfaerenPaaroerenderom',
            'OmfangetAvBehandlingen')
  Del2 <- c('LegeInformasjonFrekvens',
            'SvarPaaSpoersmaal',
            'ForklaringForstaaelse',
            'InformasjonsAerlighet',
            'InformasjonOmForloep',
            'InformasjonsOverensstemmelse',
            'BeslutningsInvolvering',
            'BeslutningsStoette',
            'BeslutningsKontroll',
            'BeslutningsTid',
            'LivsLengde',
            'LivssluttKomfor',
            'LivssluttStoette')
  
  Del1Skaar <- paste0(Del1,'Skaar')
  Del2Skaar <- paste0(Del2,'Skaar')
  RegData[,c(Del1Skaar,Del2Skaar)] <- NA
  
  #----- OM SPØRSMÅLENE----------
  #-1: Ikke besvart 
  #Del1: Alle spm 1-5, 6:ikke aktuelt
  #Del2: Spm 1-6:  1-5, 6:ikke aktuelt
  #Spm 7-13[-10]: 1-5, 
  #Spm 10 1-2
  #Dvs. alle spm har spenn 1-5, unntatt spm.10 del 2
  #Spørsmål som skal snus: Del2, spm.7-13 1:5 = 0:100
  
  
  #Standard: 1:5 -> 100:0
  verdi5 <- c(100, 75, 50, 25, 0)
  
  Spm <- c(Del1,Del2[1:6])
  Skaar <- paste0(Spm,'Skaar')
  for (nr in 1:length(Spm)) { RegData[,Skaar[nr]] <- mapvalues(RegData[ ,Spm[nr]], 
                                                               from = c(-1,1:6), to = c(NA,verdi5,NA))}
  
  RegData[ ,Del2Skaar[10]] <- mapvalues(RegData[ ,Del2[10]], 
                                        from = c(-1,1:2), to = c(NA,0,100))
  
  Spm <- Del2[c(7:9,11:13)]
  Skaar <- paste0(Spm,'Skaar')
  for (nr in 1:length(Spm)) { RegData[ ,Skaar[nr]] <-  mapvalues(RegData[ ,Spm[nr]], 
                                                                 from = c(-1,1:5), to = c(NA,rev(verdi5)))}
  
  
  #Each score is calculated by averaging available items, 
  #provided the respondent answers at least 70% of the items in the respective scale
  #NB: IKKE lagt inn sjekk på om nok observasjoner
  #rowSums(is.na(RegData[ ,Del1Skaar])
  
  RegData$OmsorgTot <- rowMeans(RegData[ ,Del1Skaar], na.rm = T)
  RegData$BeslutningTot <- rowMeans(RegData[ ,Del2Skaar[1:10]], na.rm = T)
  RegData$FSICUtot <- rowMeans(RegData[ ,c(Del1Skaar, Del2Skaar[1:10])], na.rm = T)
  
  #write.table(RegData, file = paste0('A:/Intensiv/PaarorDataSkaar', Sys.Date(),'.csv'), row.names=FALSE, sep = ';', fileEncoding = "UTF-8")
  return(RegData)
}


