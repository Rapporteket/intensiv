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
      if (dim(RegData)[1]<10) {tabBeleggAnt <- as.matrix(c('Ant.reg', '<3'),1,2) } else {
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData
      #RegData <- Mtid$RegData
      tabBeleggAnt <- rbind('Ferdigstilte intensivopphald' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,
                            'Registrerte pasientar' = tapply(RegData$PasientID, RegData$TidsEnhet,
                                                             FUN=function(x) length(unique(x))),
                            'Antal intensivdøger' = round(as.numeric(tapply(RegData$Liggetid, RegData$TidsEnhet, sum, na.rm=T)),0),
                            'Antal respiratordøger' = round(as.numeric(tapply(RegData$respiratortid, RegData$TidsEnhet, sum, na.rm=T)),0)
      )

      antTidsenh <- ifelse(tidsenhet=='Aar', 15, 11)

      tabBeleggAnt <- tabBeleggAnt[, max(1, dim(tabBeleggAnt)[2]-antTidsenh) : dim(tabBeleggAnt)[2]] #Tar med 12 siste
      }
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
  if (dim(RegDataDum)[1] > 0) {
    tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
    colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b %y') #month(lubridate::ymd(colnames(tabAvdMnd1)), label = T)
    tabAvdMnd1 <- addmargins((tabAvdMnd1))
    #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=lubridate::floor_date(InnDato, "month"), ShNavn) %>%
    #      summarize(Antall=length(ShNavn))
    tabAvdMnd1 <- xtable::xtable(tabAvdMnd1)
  } else {
    tabAvdMnd1 <- 'Ingen registreringer for gitt utvalg'}
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
      aggVar <-  c('ShNavn', 'InnDato', 'Aar')
      RegData <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC'), aggVar]

      if (dim(RegData)[1]>0){
        tabAvdAarN <-   addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-antAar-1):AarNaa), c('ShNavn','Aar')]))
        rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
        colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Sum valgte år'
        tabAvdAarN <- xtable::xtable(tabAvdAarN)
        } else {
          tabAvdAarN <- 'Ingen registreringer for gitt utvalg'
        }
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
#' 20.okt 2025: Endrer fra å bare gruppere på tidsenhet til å kunne gruppere på andre variabler.
#' @param RegData dataramme
#' @param tidsenhet velg: Aar, Halvaar, Kvartal, Mnd (standard)
#' @param datoTil sluttdato, format: 'yyyy-mm-dd'
#' @param enhetsUtvalg enhetsutvalg
#' @param reshID enhetens resh-id
#' @param sykehus HelseenhetKortnavn eller HF-navn
#' @param respirator respirator/invasiv/non-inv 0:ikke respirator, 1:respirator, 2:invasiv, 3:non-invasiv
#' @export
tabNokkeltall <- function(RegData, tidsenhet='Mnd', grVar = '',
                          datoFra='2014-01-01', datoTil=Sys.Date(),
                          enhetsUtvalg=0, respirator=4,
                          luftvei=0,
                          reshID=0, sykehus='Alle', utvidTab=0) {

  # if (grVar == '') {
  #   if (datoFra %in% c('2020-02-29', '2024-02-29', '2028-02-29')) { #- sjekk hvilken betydning denne har
  datoFra <- switch(tidsenhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'),
                    Aar = paste0(lubridate::year(as.Date(datoTil))-15, '-01-01')
  )
  # }}
  RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                          luftvei = luftvei,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
  if (dim(RegData)[1]>0) {
    if (grVar == '') {
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet, tab=1)$RegData}

    if (respirator %in% 0:3) {
      indResp <- switch(as.character(respirator),
                        '0' = which(RegData$MechanicalRespirator==2),
                        '1' = which(RegData$MechanicalRespirator==1), # 88466 respiratortid>0 #87124
                        '2' = which(RegData$InvasivVentilation>0),
                        '3' = which(RegData$NonInvasivVentilation>0))
      RegData <- RegData[indResp, ]
    }

    RegData$GrVar <- if (grVar == '') {RegData$TidsEnhet} else {RegData[ ,grVar]}

    if (sykehus %in% unique(RegData$RHF)) {
      RegData <- RegData[RegData$RHF == sykehus, ]
    }
    if (sykehus %in% unique(RegData$HF)) {
      RegData <- RegData[RegData$HF == sykehus, ]
    }
    if (sykehus %in% unique(RegData$ShNavn)) {
      RegData <- RegData[RegData$ShNavn == sykehus, ]
    }


    indLigget <- which(RegData$Liggetid>0)
    indRespt <- which(RegData$respiratortid>0)
    indRespInv <- which(RegData$InvasivVentilation >0)
    indRespNIV <- which(RegData$NonInvasivVentilation>0)
    indSAPS <- which(RegData$SAPSII > 0)
    indNEMS <- which( (RegData$Liggetid>=1) & (RegData$NEMS>1))
    RegDataReinn <- NIRVarTilrettelegg(RegData=RegData, valgtVar = 'reinn', figurtype = 'andelGrVar')$RegData
    ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
    RegData$Ut1708 <- 0
    RegData$Ut1708[ind1708]<-1

    tabNokkeltall <- rbind(
      'Antall opphold' = tapply(RegData$PasientID, RegData$GrVar, FUN=length), #table(RegDataEget$GrVar), #Neget,
      'Antall pasienter' = tapply(RegData$PasientID, RegData$GrVar,
                                  FUN=function(x) length(unique(x))),
      #  'Antall opph m/ECMO' = tapply(RegData$EcmoEcla, RegData$GrVar, FUN=sum, na.rm=T),
      'Liggedøgn (totalt)' = tapply(RegData$Liggetid[indLigget], RegData$GrVar[indLigget], FUN=sum, na.rm=T),
      'Liggedøgn (median)' = tapply(RegData$Liggetid[indLigget], RegData$GrVar[indLigget], FUN=median, na.rm=T),
      'Mekanisk \nventilasjonsstøtte (%)' = tapply(RegData$respiratortid>0, RegData$GrVar,
                                                   FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
      'Respiratordøgn, \nsamlet (totalt)' = tapply(RegData$respiratortid[indRespt], RegData$GrVar[indRespt],
                                                   FUN=sum, na.rm=T),
      'Respiratordøgn, \nsamlet (median)' = tapply(RegData$respiratortid[indRespt], RegData$GrVar[indRespt],
                                                   FUN=median, na.rm=T),
      'Respiratordøgn, \ninv. (median)' = tapply(RegData$InvasivVentilation[indRespInv], RegData$GrVar[indRespInv],
                                                 FUN=median, na.rm=T),
      'Respiratordøgn, \nnon-inv. (median)' = tapply(RegData$NonInvasivVentilation[indRespNIV], RegData$GrVar[indRespNIV],
                                                     FUN=median, na.rm=T),
      'ECMOdøgn (median)' = tapply(RegData$EcmoEclaDager[RegData$EcmoEcla], RegData$GrVar[RegData$EcmoEcla],
                                   FUN=median, na.rm=T),
      'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$GrVar[indSAPS], FUN=median, na.rm=T),
      'NEMS (totalt)' = tapply(RegData$NEMS[indNEMS],
                               RegData$GrVar[indNEMS], FUN=sum, na.rm=T),
      'NEMS/opph. (median)' = tapply(RegData$NEMS[indNEMS],
                                     RegData$GrVar[indNEMS], FUN=median, na.rm=T),
      'Reinnleggelser, \n<72t (%)' = tapply(RegDataReinn$Reinn==1, RegDataReinn$GrVar,
                                            #tapply(RegData$Reinn[indReinn]==1, RegData$GrVar[indReinn],
                                            FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
      'Utskrevet \n kl 17-08 (%)' = tapply(RegData$Ut1708, RegData$GrVar,
                                           FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1)),
      'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$GrVar,
                          FUN=function(x) round(sum(x, na.rm=T)/length(x)*100,1))
    )


    if (utvidTab == -2) {
      indBort <- c(grep('NEMS', row.names(tabNokkeltall)),
                   grep('Reinn', row.names(tabNokkeltall)),
                   grep('Utskrevet', row.names(tabNokkeltall)))
      tabNokkeltall <- tabNokkeltall[-indBort,]

    }

    if (utvidTab==-1) { #Tar bort noen variabler for å tilpasse til Nord-bestilling
      tabNokkeltall <- tabNokkeltall[c(1:9, 13:15), ]
    }

    if (utvidTab==1) {
      #Komplikasjoner:
      RegData$KompTot <- (rowSums(RegData[ ,c('KompHypoglykemi',	'KompPneumotoraks',	'KompLuftveisproblem',
                                              'KompDekubitus')])>0)

      tabNokkeltall <- rbind(
        tabNokkeltall,
        'Død innen 30 dager (%)' = tapply((RegData$Dod30==1), RegData$GrVar,
                                          FUN=function(x) sum(x, na.rm=T)/length(x)*100),
        'Alder (median)' = tapply(RegData$Alder,
                                  RegData$GrVar, FUN=median, na.rm=T),
        'Alder over 80 år (%)' = tapply(RegData$Alder>=80,
                                        RegData$GrVar,
                                        FUN=function(x) sum(x, na.rm=T)/length(x)*100),
        'Alder under 18 år (%)' = tapply(RegData$Alder<18,
                                         RegData$GrVar,
                                         FUN=function(x) sum(x, na.rm=T)/length(x)*100),
        'Menn (%)' = tapply((RegData$erMann==1), RegData$GrVar,
                            FUN=function(x) sum(x, na.rm=T)/length(x)*100),
        'Komplikasjoner (%)' = tapply((RegData$KompTot==1), RegData$GrVar,
                                      FUN=function(x) sum(x, na.rm=T)/length(x)*100)
      )
    }
  } else {
    tabNokkeltall <- 'Ingen registreringer'
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


#' Aldersfordeling, tabell
#'
#' @param RegData datatabell, opprinnelig laget for beredskappskjema
#' @param reshID avdelingsresh
#' @param enhetsNivaa enhetsnivå
#' @param sens maskere celler <3. 0-nei, 1-ja
#' @inheritParams NIRUtvalgEnh
#'
#' @return aldersfordeling
#' @export
#'
#' @examples
#' \dontrun{
#' }
TabAlder <- function(RegData, reshID=0, enhetsNivaa='Alle',
                     skjemastatus=9, resp=9,
                     dodInt=9,erMann=9, sens=0){
 #Kan vurdere flere filtreringer:
  # datoFra='2011-01-01', datoTil=Sys.Date(), aar=0,
  # erMann='', luftvei=0, nivaa = 0, overfPas = 0,


  #HF-nivå skal se eget HF og eget RHF. Filterer derfor på RHF for HF
  egetRHF <- ifelse (enhetsNivaa=='HF',
                     RegData$RHF[match(reshID, RegData$ReshId)],
                     'Alle')

  UtData <- NIRUtvalgEnh(RegData=RegData,
                        #     valgtRHF=egetRHF,
                         #    resp=resp,
                             dodInt = dodInt,
                             erMann = erMann,
                             #skjemastatus=skjemastatus
  )
  RegData <- UtData$RegData


  enhet <- switch(enhetsNivaa,   #
                  Alle = 'hele landet',
                  RHF = RegData$RHF[match(reshID, RegData$ReshId)],
                  HF = RegData$HF[match(reshID, RegData$ReshId)]
  )

  N <- dim(RegData)[1]
  gr <- seq(0, 90, ifelse((N<100 & sens==0), 25, 10) )
  grtxt <-  if(N<100 & sens==0){
    c('0-24', '25-49', "50-74", "75+")
  } else {
    c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')
  }
  RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
  levels(RegData$AldersGr) <- grtxt
  AlderAlle <- table(RegData$AldersGr)    #table(RegData$AldersGr, RegData$EnhetsNivaaVar)
  AlderAllePst <-prop.table(AlderAlle)*100
  TabAlder <- cbind(
    'Antall pas.' = AlderAlle,
    'Andel pas.' = paste0(sprintf('%.0f', AlderAllePst), ' %'))
  TabAlder <- rbind(TabAlder, 'Totalt' = c(N, ''))
  if (sens == 1) {
    under3 <- which(AlderAlle<3)
    TabAlder[under3,] <- c(rep('<3', length(under3)), rep('', length(under3)))
  }

  txt <- ifelse(enhetsNivaa == 'HF', egetRHF, 'hele landet')
  colnames(TabAlder) <- paste0(c('Antall', 'Andel'), paste0(', ', txt))

  if (enhetsNivaa %in% c('RHF', 'HF')) {
    RegData$EnhetsNivaaVar <- RegData[ , enhetsNivaa]
    AlderEget <- table(RegData$AldersGr[RegData$EnhetsNivaaVar == enhet])
    AlderEgetPst <- prop.table(AlderEget)*100
    TabAlderEget <- cbind(
      'Antall pas., eget' = AlderEget,
      'Andel pas., eget' = paste0(sprintf('%.0f', prop.table(AlderEget)*100), ' %'))
    TabAlderEget <- rbind(TabAlderEget, 'Totalt' = c(sum(AlderEget), ''))
    if (sens==1){
      under3 <- which(AlderEget<3)
      TabAlderEget[under3,] <- c(rep('<3', length(under3)), rep('', length(under3)))
    }

    colnames(TabAlderEget) <- paste0(c('Antall', 'Andel'), paste0(', eget ', enhetsNivaa))

    TabAlder <- cbind(
      TabAlderEget,
      TabAlder)
  }

  return(invisible(UtData <-
                     list(Tab=TabAlder,
                          utvalgTxt=c(UtData$utvalgTxt, enhet))))

}


#' Oppsummeringer, luftveisinfeksjonspasienter
#'
#' @param RegData
#'
#' @return oppsummering i tabellform
#' @export
#'
TabOppsumLuftvei <- function(RegData, pgaLuftvei=0) {

  if (pgaLuftvei==1) {
    RegData <- RegData[RegData$RespiratoryTractInfectionPrimaryCauseForICUAdmission == 1, ]
  }

N <- dim(RegData)[1]
AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
AntBruktECMO <- sum(RegData$EcmoEcla, na.rm=T)
Liggetid <- summary(RegData$Liggetid, na.rm = T)
RespTid <- summary(RegData$respiratortid, na.rm = T)
ECMOtid <- summary(RegData$EcmoEclaDager[RegData$EcmoEcla], na.rm = T)
Alder <- summary(RegData$Alder, na.rm = T)
AntDod <- sum(RegData$DischargedIntensiveStatus==1, na.rm=T)

med_IQR <- function(x){
  c(sprintf('%.1f',x[4]), sprintf('%.1f',x[3]), paste(sprintf('%.1f',x[2]), sprintf('%.1f',x[5]), sep=' - '))
}

TabFerdigeReg <- rbind(
  'Alder (år)' = c(med_IQR(Alder), N, ''),
  'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
  'Respiratortid (døgn)' = c(med_IQR(RespTid), AntBruktResp*(c(1, 100/N))),
  'ECMO (døgn)' = c(med_IQR(ECMOtid), AntBruktECMO*(c(1, 100/N))),
  'Døde' = c('','','',AntDod, paste0(sprintf('%.f',100*AntDod/N),'%'))
)
colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall pasienter', 'Andel pasienter')
TabFerdigeReg[c(3:4),'Andel pasienter'] <-
  paste0(sprintf('%.0f', as.numeric(TabFerdigeReg[c(3:4),'Andel pasienter'])),'%')

# xtable::xtable(TabFerdigeReg, #FerdigBekr$Tab,
#                digits=0,
#                align = c('l','r','r','c', 'r','r'),
#                caption = 'Verdier basert på opphold med luftveisinfeksjon siste 40 uker.
#                IQR (inter quartile range) betyr at 25 \\% av pasientene er under minste verdi,
#                50 \\% av pasientene er i intervallet, og 25 \\% av pasientene er over høyeste verdi.'
# )

return(invisible(TabFerdigeReg))

}
