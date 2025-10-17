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
                        Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
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
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(lubridate::ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd1 <- addmargins((tabAvdMnd1))
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=lubridate::floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1)
	return(tabAvdMnd1)
}

#' Antall opphold siste år
#'
#' @param RegData data
#' @param datoTil sluttdato
#' @param antAar antall år som skal vises
#' @export
tabAntOpphShAar <- function(RegData, datoTil, antAar = 5){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-antAar-1):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Sum valgte år'
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
#' @param datoTil sluttdato, format: 'yyyy-mm-dd'
#' @param enhetsUtvalg enhetsutvalg
#' @param reshID enhetens resh-id
#' @param respirator respirator/invasiv/non-inv 0:ikke respirator, 1:respirator, 2:invasiv, 3:non-invasiv
#' @export
tabNokkeltallGML <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0, respirator=4) {

    datoFra <- switch(tidsenhet,
                        Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                              enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
      if (respirator %in% 0:3) {
      indResp <- switch(as.character(respirator),
                        # '0' = setdiff(1:dim(RegData)[1], which(RegData$respiratortid>0)),
                        # '1' = which(RegData$respiratortid>0), # 88466 respiratortid>0 #87124
                        '0' = which(RegData$MechanicalRespirator==2),
                        '1' = which(RegData$MechanicalRespirator==1), # 88466 respiratortid>0 #87124
                        '2' = which(RegData$InvasivVentilation>0),
                        '3' = which(RegData$NonInvasivVentilation>0))
      RegData <- RegData[indResp, ]
      }

      indLigget <- which(RegData$liggetid>0)
      indRespt <- which(RegData$respiratortid>0)
      indRespInv <- which(RegData$InvasivVentilation >0)
      indRespNIV <- which(RegData$NonInvasivVentilation>0)
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
            'Liggedøgn (totalt)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=sum, na.rm=T),
            'Liggedøgn (median)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),
            'Mekanisk \nventilasjonsstøtte (%)' = tapply(RegData$respiratortid>0, RegData$TidsEnhet,
                                            FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            # 'Respiratordøgn (invasiv)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                             # FUN=median, na.rm=T),
            'Respiratordøgn (median, non-invasiv)' = tapply(RegData$NonInvasivVentilation[indRespNIV], RegData$TidsEnhet[indRespNIV],
                                                            FUN=median, na.rm=T),
            'Respiratordøgn (median, invasiv)' = tapply(RegData$InvasivVentilation[indRespInv], RegData$TidsEnhet[indRespInv],
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
#' @param tidsenhet velg: Aar, Halvaar, Kvartal, Mnd (standard)
#' @param datoTil sluttdato, format: 'yyyy-mm-dd'
#' @param enhetsUtvalg enhetsutvalg
#' @param reshID enhetens resh-id
#' @param sykehus HelseenhetKortnavn eller HF-navn
#' @param respirator respirator/invasiv/non-inv 0:ikke respirator, 1:respirator, 2:invasiv, 3:non-invasiv
#' @export
tabNokkeltall <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0, respirator=4,
                                 sykehus='Alle', datoFra='2020-02-29', utvidTab=0) {
  if (datoFra %in% c('2020-02-29', '2024-02-29')) {
    datoFra <- switch(tidsenhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                    Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
                    )
  }
  RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
  RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData

  if (respirator %in% 0:3) {
    indResp <- switch(as.character(respirator),
                      '0' = which(RegData$MechanicalRespirator==2),
                      '1' = which(RegData$MechanicalRespirator==1), # 88466 respiratortid>0 #87124
                      '2' = which(RegData$InvasivVentilation>0),
                      '3' = which(RegData$NonInvasivVentilation>0))
    RegData <- RegData[indResp, ]
  }


  if (sykehus %in% unique(RegData$RHF)) {
    RegData <- RegData[RegData$RHF == sykehus, ]
  }
  if (sykehus %in% unique(RegData$HF)) {
    RegData <- RegData[RegData$HF == sykehus, ]
  }
  if (sykehus %in% unique(RegData$ShNavn)) {
    RegData <- RegData[RegData$ShNavn == sykehus, ]
  }



  indLigget <- which(RegData$liggetid>0)
  indRespt <- which(RegData$respiratortid>0)
  indRespInv <- which(RegData$InvasivVentilation >0)
  indRespNIV <- which(RegData$NonInvasivVentilation>0)
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
    'Liggedøgn (totalt)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=sum, na.rm=T),
    'Liggedøgn (median)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),
    'Mekanisk \nventilasjonsstøtte (%)' = tapply(RegData$respiratortid>0, RegData$TidsEnhet,
                                                 FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
    'Respiratordøgn, \nsamlet (totalt)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                       FUN=sum, na.rm=T),
    'Respiratordøgn, \nsamlet (median)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt],
                                       FUN=median, na.rm=T),
    'Respiratordøgn, \ninvasiv (median)' = tapply(RegData$InvasivVentilation[indRespInv], RegData$TidsEnhet[indRespInv],
                                                FUN=median, na.rm=T),
    'Respiratordøgn, \nnon-invasiv (median)' = tapply(RegData$NonInvasivVentilation[indRespNIV], RegData$TidsEnhet[indRespNIV],
                                                    FUN=median, na.rm=T),
    'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$TidsEnhet[indSAPS], FUN=median, na.rm=T),
    'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                             RegData$TidsEnhet[indNEMS], FUN=sum, na.rm=T),
    'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                   RegData$TidsEnhet[indNEMS], FUN=median, na.rm=T),
    'Reinnleggelser, \n<72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$TidsEnhet,
                                          #tapply(RegData$Reinn[indReinn]==1, RegData$TidsEnhet[indReinn],
                                          FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
    'Utskrevet \n kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$TidsEnhet,
                                         FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
    'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$TidsEnhet,
                        FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1))
  )

  if (utvidTab==-1) { #Tar bort noen variabler for å tilpasse til Nord-bestilling
    tabNokkeltall <- tabNokkeltall[c(1:9, 13:15), ]
  }

  if (utvidTab==1) {
    #Komplikasjoner:
    RegData$KompTot <- (rowSums(RegData[ ,c('KompHypoglykemi',	'KompPneumotoraks',	'KompLuftveisproblem',
                                            'KompDekubitus')])>0)

  tabNokkeltall <- rbind(
    tabNokkeltall,
    'Død innen 30 dager (%)' = tapply((RegData$Dod30==1), RegData$TidsEnhet,
                                      FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Alder (median)' = tapply(RegData$Alder,
                              RegData$TidsEnhet, FUN=median, na.rm=T),
    'Alder over 80 år (%)' = tapply(RegData$Alder>=80,
                                   RegData$TidsEnhet,
                                   FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Alder under 18 år (%)' = tapply(RegData$Alder<18,
                                     RegData$TidsEnhet,
                                     FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Menn (%)' = tapply((RegData$erMann==1), RegData$TidsEnhet,
                        FUN=function(x) sum(x, na.rm=T)/length(x)*100),
    'Komplikasjoner (%)' = tapply((RegData$KompTot==1), RegData$TidsEnhet,
                                  FUN=function(x) sum(x, na.rm=T)/length(x)*100)
  )
  }


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
  # datoFra=Sys.Date()-700
  # datoTil=Sys.Date()
  # overfFraSh <- 0
  # velgAvd <- 0
  # reshID <- 108610 #Overfører TIL Hamar

  if (velgAvd != 0){ reshID <- velgAvd}
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
    colnames(Tab) <- c(paste(c('til','fra:')[overfFraSh+1], shNavn, c('fra', 'til:')[overfFraSh+1]),
                       'Antall pasienter', 'Fordeling')
    return(Tab)
}
