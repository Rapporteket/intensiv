#' Funksjoner for å lage tabeller
#'
#' Belegg (antall opphold, pasienter og intensivdøgn)
#' Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
#' @inheritParams NIRFigAndeler
#' @export
tabBelegg <- function(RegData, tidsenhet='Aar', datoTil, enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet,
                        Mnd = floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                             enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
      #RegData <- Mtid$RegData
      tabBeleggAnt <- rbind('Ferdigstilte intensivopphald' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,
                            'Registrerte pasientar' = tapply(RegData$PasientID, RegData$TidsEnhet,
                                                             FUN=function(x) length(unique(x))),
                            'Antal intensivdøger' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0),
                            'Antal respiratordøger' = round(as.numeric(tapply(RegData$respiratortid, RegData$TidsEnhet, sum, na.rm=T)),0)
      )

      antTidsenh <- ifelse(tidsenhet=='Aar', 4, 11)

      tabBeleggAnt <- tabBeleggAnt[, max(1, dim(tabBeleggAnt)[2]-antTidsenh) : dim(tabBeleggAnt)[2]] #Tar med 12 siste
      #format(as.yearmon(as.Date('2017-09-02')),'%b%y')
      #kol <- dimnames(tabBeleggAnt)[[2]]
      #format(as.Date(kol, format= '%y.%m'),'%b%y')

      # overskr <- dimnames(tabAvdNEget)[[2]]
      # aar <- substr(overskr, 1,2)
      # mnd <- as.numeric(substr(overskr, 4,5))
      # mndTxt <- c('jan', 'feb', 'mar', 'apr', 'mai', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'des')
      # colnames(tabAvdNEget) <- paste0(mndTxt[mnd], aar)
      #tabBeleggAnt <- xtable::xtable(tabBeleggAnt, digits=0, align=c('l', rep('r', ncol(tabBeleggAnt))),
       #      caption=paste0('Antal opphald og liggedøger, ', shtxt,'.'), label='tab:RegEget')
      return(tabBeleggAnt)
}
#' tabAntOpphShMnd antall opphold siste X (antMnd) mnd
#'
#' @param RegData Dataramme
#' @inheritParams NIRUtvalgEnh
#' @param antMnd antall måneder som skal vises
#'
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), datoFra='Ikke angitt', antMnd=6){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive
  if (datoFra == 'Ikke angitt') {
      datoFra <- lubridate::floor_date(as.Date(datoTil)%m-% months(antMnd, abbreviate = T), 'month')} #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(ymd(colnames(tabAvdMnd1)), '%b %y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd1 <- addmargins((tabAvdMnd1))
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1)
	return(tabAvdMnd1)
}

#' Antall opphold siste 5 år
#'
#' @param RegData data
#' @param datoTil sluttdato
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}

#'Antall registreringer/pasienter siste 5 år:
#'
#' @param RegData data
#' @param gr gruppering opphold (opph, standard), pasienter (pas)
#' @param datoTil sluttdato
#'
#' @export
tabAntOpphPasSh5Aar <- function(RegData, gr='opph', datoTil){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      if (gr == 'pas'){
            Data <- RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar', 'PasientID')]
            tabAvdAarN <- tapply(Data$PasientID, Data[ c('ShNavn','Aar')], FUN=function(x) length(unique(x)))
            tabAvdAarN[is.na(tabAvdAarN)] <- 0
            tabAvdAarN <- addmargins(tabAvdAarN) #, FUN = function(x) sum(x, na.rm=T))

      } else {
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      }
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}
# tabAntPasSh5Aar <- function(RegData, personIDvar='PasientID' , datoTil){
#       AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
#
#       Data <- RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar', personIDvar)]
#       tabPasAvdAarN <- tapply(Data$PasientID, Data[ c('ShNavn','Aar')], FUN=function(x) length(unique(x)))
#       tabPasAvdAarN[is.na(tabPasAvdAarN)] <- 0
#
#       tabPasAvdAarN <- addmargins(tabPasAvdAarN) #, FUN = function(x) sum(x, na.rm=T))
#       rownames(tabPasAvdAarN)[dim(tabPasAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
#       colnames(tabPasAvdAarN)[dim(tabPasAvdAarN)[2] ]<- 'TOTALT'
#       tabPasAvdAarN <- xtable::xtable(tabPasAvdAarN)
#       return(tabPasAvdAarN)
# }

#' Finn eventuelle dobbeltregistreringer
#' @param RegData data
#' @param datoTil sluttdato
#' @param reshID enhetens resh
#' @param pasientID pasientidentifikasjon, personentydig
#'
#' @export
finnDblReg <- function(RegData, datoTil=Sys.Date(), reshID=0, pasientID = 'PasientID'){ #datoFra='2017-01-01',
      #Registreringer kor same pasient har fått registrert to innleggingar med mindre enn 2 timars mellomrom.
      #RegData må inneholde PasientID, Innleggelsestidspunkt og SkjemaGUID
      #Evt. legge til utvalg på tidsrom
      sortVar <- c('ReshId',pasientID,'Innleggelsestidspunkt', "SkjemaGUID")
      ind <- if (reshID == 0) {1:dim(RegData)[1]} else {which(RegData$ReshId==reshID)}
      RegData <- RegData[ind, sortVar]
      RegDataSort <- RegData[order(RegData$ReshId, RegData$PasientID, RegData$Innleggelsestidspunkt), ]
      RegDataSort$OpphNr <- ave(RegDataSort[ ,pasientID], RegDataSort[ ,pasientID], FUN=seq_along)
      indPasFlereOpph <- which(RegDataSort$OpphNr>1)
      RegDataSort$TidInn <- NA
      RegDataSort$TidInn[indPasFlereOpph] <-
            difftime(RegDataSort$Innleggelsestidspunkt[indPasFlereOpph],
                     RegDataSort$Innleggelsestidspunkt[indPasFlereOpph-1],
                     units = 'hour')

      indDbl <- which(abs(RegDataSort$TidInn) <2 )
      tabDblRaa <- RegDataSort[sort(c(indDbl, indDbl-1)), sortVar]
                            #c(pasientID,'Innleggelsestidspunkt', "SkjemaGUID")]
      if (dim(tabDblRaa)[1] == 0) {
            tabDbl <- 'Ingen dobbeltregistreringar'
      } else {
        test <- as.matrix(tabDblRaa,dim(tabDblRaa)[1], dim(tabDblRaa)[2] )
        tabDbl <- xtable::xtable(test, row.names=NA)
            #tabDbl <- tabDblRaa
            }
      return(tabDbl)
}


#'  Nøkkeltall (antall opph., pasienter,  intensivdøgn, samt div oversiktstall)
#'
#' @param RegData dataramme
#' @param tidsenhet velg: Aar, Halvaar, Kvartal, Mnd (standard)
#' @param datoTil sluttdato
#' @param enhetsUtvalg enhetsutvalg
#' @param reshID enhetens resh-id
#' @export
tabNokkeltall <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet,
                        Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                              enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
      #NB: sjekk riktige utvalg!!!
      indLigget <- which(RegData$liggetid>0)
      indRespt <- which(RegData$respiratortid>0)
      indSAPS <- which(RegData$SAPSII > 0)
      indNEMS <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))
      RegDataReinn <- NIRVarTilrettelegg(RegData=RegData, valgtVar = 'reinn', figurtype = 'andelGrVar')$RegData
      #RegData <- FinnReinnleggelser(RegData=RegData, PasientID = 'PasientID')
      #indReinn <- intersect(which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC')), which(RegData$Overf==1))
      ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
      RegData$Ut1708 <- 0
      RegData$Ut1708[ind1708]<-1

      tabNokkeltall <- rbind(
            'Antall opphold' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,
            'Antall pasienter' = tapply(RegData$PasientID, RegData$TidsEnhet,
                                             FUN=function(x) length(unique(x))),
            'Antall intensivdøgn' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0),
            'Liggedøgn (median)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),
            'Liggedøgn (totalt)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=sum, na.rm=T),
            'Respirator-\nstøtte (%)' = tapply(RegData$respiratortid>0, RegData$TidsEnhet,
                                            FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            'Respiratordøgn (median)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                              FUN=median, na.rm=T),
            'Respiratordøgn (totalt)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                              FUN=sum, na.rm=T),
            'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$TidsEnhet[indSAPS], FUN=median, na.rm=T),
            'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                               RegData$TidsEnhet[indNEMS], FUN=median, na.rm=T),
            'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                                           RegData$TidsEnhet[indNEMS], FUN=sum, na.rm=T),
            'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$TidsEnhet,
                                FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            'Reinnleggelser, \n<72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$TidsEnhet,
              #tapply(RegData$Reinn[indReinn]==1, RegData$TidsEnhet[indReinn],
                                             FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            'Utskrevet \n kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$TidsEnhet,
                                           FUN=function(x) sum(x, na.rm=T)/length(x)*100)
      )

      #tabNokkeltall[,4:11] <- round(tabNokkeltall[,4:11],1)
      #dplyr::mutate_at(as.table(tabNokkeltall), vars(), funs(round(., 1)))
      #antTidsenh <- ifelse(tidsenhet=='Aar', 4, 11)
      #tabBeleggAnt <- tabBeleggAnt[, max(1, dim(tabBeleggAnt)[2]-antTidsenh) : dim(tabBeleggAnt)[2]] #Tar med 12 siste

      return(tabNokkeltall)
}



#'  Nøkkeltall (antall opph., pasienter,  intensivdøgn, samt div oversiktstall)
#'
#' @param RegData dataramme
#' @param sykehus HelseenhetKortnavn eller HF-navn
#' @export
tabNokkeltallUtvid <- function(RegData, tidsenhet = 'Aar', sykehus='Alle',
                               datoFra='2016-01-01', datoTil=Sys.Date()) {

  RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
  RegData <-  NIRUtvalgEnh(RegData = RegData, datoFra = datoFra, datoTil = datoTil)$RegData

  #Komplikasjoner:
  RegData$KompTot <- (rowSums(RegData[ ,c('KompHypoglykemi',	'KompPneumotoraks',	'KompLuftveisproblem',
                                          'KompDekubitus')])>0)


  if (sykehus %in% unique(RegData$RHF)) {
    RegData <- RegData[RegData$RHF == sykehus, ]
  }
  if (sykehus %in% unique(RegData$HF)) {
    RegData <- RegData[RegData$HF == sykehus, ]
  }
  if (sykehus %in% unique(RegData$HelseenhetKortnavn)) {
    RegData <- RegData[RegData$HelseenhetKortnavn == sykehus, ]
  }

  indLigget <- which(RegData$liggetid>0)
  indRespt <- which(RegData$respiratortid>0)
  indSAPS <- which(RegData$SAPSII > 0)
  indNEMS <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))
  RegDataReinn <- NIRVarTilrettelegg(RegData=RegData, valgtVar = 'reinn', figurtype = 'andelGrVar')$RegData
  ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
  RegData$Ut1708 <- 0
  RegData$Ut1708[ind1708]<-1

  tabNokkeltall <- rbind(
    'Antall opphold' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,
    'Antall pasienter' = tapply(RegData$PasientID, RegData$TidsEnhet,
                                FUN=function(x) length(unique(x))),
    'Antall intensivdøgn' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0),
    'Liggedøgn (median)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),
    'Liggedøgn (totalt)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=sum, na.rm=T),
    'Respirator-\nstøtte (%)' = tapply(RegData$respiratortid>0, RegData$TidsEnhet,
                                       FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Respiratordøgn (median)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                       FUN=median, na.rm=T),
    'Respiratordøgn (totalt)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                       FUN=sum, na.rm=T),
    'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$TidsEnhet[indSAPS], FUN=median, na.rm=T),
    'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                   RegData$TidsEnhet[indNEMS], FUN=median, na.rm=T),
    'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                             RegData$TidsEnhet[indNEMS], FUN=sum, na.rm=T),
    'Alder (median)' = tapply(RegData$Alder,
                              RegData$TidsEnhet, FUN=median, na.rm=T),
    'Alder over 80 år(%)' = tapply(RegData$Alder>=80,
                              RegData$TidsEnhet,
                              FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Alder under 18 år (%)' = tapply(RegData$Alder<18,
                                  RegData$TidsEnhet,
                                  FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Menn (%)' = tapply((RegData$erMann==1), RegData$TidsEnhet,
                          FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$TidsEnhet,
                        FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Død innen 30 dager (%)' = tapply((RegData$Dod30==1), RegData$TidsEnhet,
                                  FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Komplikasjoner (%)' = tapply((RegData$KompTot==1), RegData$TidsEnhet,
                                  FUN=function(x) sum(x, na.rm=T)/length(x)*100),
  'Reinnleggelser, \n<72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$TidsEnhet,
                                          #tapply(RegData$Reinn[indReinn]==1, RegData$TidsEnhet[indReinn],
                                          FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Utskrevet \n kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$TidsEnhet,
                                         FUN=function(x) sum(x, na.rm=T)/length(x)*100)
  )


  return(tabNokkeltall)
}










#' Vise figurdata som tabell
#' @param UtDataFraFig data fra figurfunksjoner, dvs. beregnede verdier
#' @export
lagTabavFig <- function(UtDataFraFig){
      tab <-cbind(UtDataFraFig$Ngr$Hoved,
                  UtDataFraFig$AggVerdier$Hoved,
                  UtDataFraFig$Ngr$Rest,
                  UtDataFraFig$AggVerdier$Rest)
      grtxt <- UtDataFraFig$grtxt
      if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
            grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
      rownames(tab) <- grtxt
      kolnavn <- c('Antall' , 'Andel (%)')
      colnames(tab) <- c(kolnavn, if(!is.null(UtDataFraFig$Ngr$Rest)){kolnavn})
      # colnames(tab) <- c(paste0(UtDataFraFig$hovedgrTxt,', Antall'),
#                    paste0(UtDataFraFig$hovedgrTxt, ', Andel (%)'),
#                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt,', Antall')},
#                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt, ', Andel (%)')})

return(tab)
}

#' Finne overføringer til/fra en enhet
#' @param RegData data
#' @param datoFra startdato
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
#' @param overfFraSh - overføring fra (1) eller til (0) den aktuelle enheten
#' @inheritParams NIRUtvalgEnh
#' @export
tabOverforinger <- function(RegData, datoFra=Sys.Date()-365, datoTil=Sys.Date(),
                            reshID=0, velgAvd=0, enhetsUtvalg=2, overfFraSh=1){
  #Overf: (1= ikke overført, 2= overført) TransferredStatus
  #Hvis alle kolonner som sier noe om til/fra-sykehus er tomme, vet vi ikke om det er ei overføring
  #til eller fra det aktuelle sykehuset. Disse må derfor ekskluderes. (Gjelder ca 5% i 2015-19)
  #Filtrerer på eget sykehus. Ser hvilke avdelinger overført fra eget TIL andre
  #Vi må ta med
  # RegData <- NIRRegDataSQL(datoFra = '2019-01-01')
  # RegData <- NIRPreprosess(RegData)
  # datoFra=Sys.Date()-365
  # datoTil=Sys.Date()
  # overfFraSh <- 0
  # velgAvd <- 0
  # reshID <- 108610 #Overfører TIL Hamar

  if (velgAvd!=0){ reshID<- velgAvd}
  shNavn <- RegData$ShNavn[match(reshID,RegData$ReshId)]
  RegData <- NIRUtvalgEnh(RegData = RegData, datoFra = datoFra, datoTil = datoTil)$RegData

  overfVariabel <- ifelse(overfFraSh==1, 'PatientTransferredFromHospital', 'PatientTransferredToHospital')
  tittel <- paste('Pasienter overført', ifelse(overfFraSh==1,'TIL', 'FRA'), shNavn)
  variabler <- c("InnDato", 'ShNavn', 'ReshId', 'PatientTransferredToHospital', 'PatientTransferredToHospitalName',
                 'PatientTransferredToHospitalText', 'PatientTransferredFromHospital',
                 'PatientTransferredFromHospitalName', 'PatientTransferredFromHospitalText')#overfVariabel)
  indMed <- which(RegData$Overf==2 & (RegData$ReshId == reshID | RegData[ ,overfVariabel]==reshID))
    Data <- RegData[indMed, variabler]
    if (overfFraSh==1) {
      ind <- which(!(Data$ReshId == reshID & Data$PatientTransferredFromHospital>0))
      Data$TilfraNavn <- Data$PatientTransferredToHospitalName
      Data$OverfTxt <- Data$PatientTransferredToHospitalText
    } else {
      ind <- which(!(Data$ReshId == reshID & Data$PatientTransferredToHospital>0))
      Data$TilfraNavn <- Data$PatientTransferredFromHospitalName
      Data$OverfTxt <- Data$PatientTransferredFromHospitalText
    }
    Data <- Data[ind, ] #Tar bort de som for eget sykehus har overføring i "feil" retning
    Data$OverfNavn <- as.character(Data$ShNavn)
    indEget <- which(Data$OverfNavn==shNavn)
    Data$OverfNavn[indEget] <- Data$TilfraNavn[indEget]
    Data$OverfNavn[which(!(is.na(Data$OverfTxt)) & is.na(Data$OverfNavn))] <- 'Annet'
    OverfVektor <- sort(table(Data$OverfNavn), decreasing = T)
    Tab <- cbind('Enhet' = c(names(OverfVektor), 'Totalt'),
      'Antall pasienter' = c(as.numeric(OverfVektor), sum(OverfVektor)),
      'Fordeling' = c(paste0(sprintf('%.1f', OverfVektor/sum(OverfVektor)*100), ' %'),'')) #sort(table(Data$PatientTransferredToHospitalName), decreasing = T)
    tilfra <- c('til','fra')[overfFraSh]
    #overfFraSh - overføring fra (1) eller til (0) den aktuelle enheten
    colnames(Tab) <- c(paste(c('til','fra:')[overfFraSh+1], shNavn, c('fra', 'til:')[overfFraSh+1]),
                       'Antall pasienter', 'Fordeling')
    return(Tab)
}
