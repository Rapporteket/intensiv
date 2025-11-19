#Fil med samling av funksjoner som lager tabeller for beredskapsskjema

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param enhetsNivaa 'RHF', 'HF', 'ShNavn'
#' @param datoFra Vis hendelser fra og med dato
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return Tabell med antall for hver tidsenhet og enhet
#' @export
TabTidEnhet <- function(RegData, tidsenhet='dag', erMann=9, resp=9, datoFra=0,
                        bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle', velgAvd=0){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=bekr, skjemastatus=skjemastatus, resp=resp,
                             dodInt=dodInt) #, valgtRHF=valgtRHF) #velgAvd=velgAvd

  RegDataAlle <- UtData$RegData
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$InnDato >= datoFra), ]}
  RegDataAlle$TidsVar <-
    switch (tidsenhet,
            dag = factor(format(RegDataAlle$InnDato, '%d.%m.%y'),
                         levels = format(rev(seq(Sys.Date(),
                                                 if (datoFra!=0) datoFra else min(RegDataAlle$InnDato),
                                                 by=paste0('-1 day'))), '%d.%m.%y')),
            uke = factor(paste0('Uke ', format(RegDataAlle$InnDato, '%V.%g')),
                         levels = paste0('Uke ',
                                         format(rev(seq(Sys.Date(),
                                                        if (datoFra!=0) datoFra else min(RegDataAlle$InnDato),
                                                        by=paste0('-1 week'))), '%V.%g'))),
            maaned = factor(format(RegDataAlle$InnDato, '%b %y'),
                            levels = format(rev(seq(Sys.Date(),
                                                    if (datoFra!=0) datoFra else min(RegDataAlle$InnDato),
                                                    by=paste0('-1 month'))), '%b %y')))

  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$TidsVar), ]
  RegData <- if(valgtRHF=='Alle') {RegDataAlle} else {RegDataAlle[RegDataAlle$RHF == valgtRHF, ]}
  Ntest <- dim(RegData)[1]

  if (valgtRHF == 'Ukjent'){
    TabTidEnh <- as.matrix(c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]), ncol=1)
    colnames(TabTidEnh) <- 'Hele landet'
  } else {

    enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHF', 'HF')

    RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]
    kolNavnSum <- switch(enhetsNivaa,
                         RHF = 'Hele landet',
                         HF = paste0(valgtRHF, ', totalt'))
    if (Ntest==0) {
      TabTidEnh <- matrix(0, ncol=1, nrow=length(levels(RegData$TidsVar)) + 1,
                          dimnames = list(c(levels(RegData$TidsVar), 'Totalt (fra 10.mars)'), valgtRHF)) #table(RegData$TidsVar)
    }else{
      TabTidEnh <- table(RegData[ , c('TidsVar', enhetsNivaa)]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Covid')], row.vars =TidsVar)
      TabTidEnh <- addmargins(TabTidEnh, FUN=list('Totalt'=sum, 'Hele landet' = sum), quiet=TRUE)
      colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
    }
    if (valgtRHF != 'Alle'){
      TabTidEnh <- cbind(TabTidEnh,
                         'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}
  }
  TabTidEnh_tidy <- tidyr::as_tibble(as.data.frame.matrix(TabTidEnh), rownames='Tid')
  TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                              caption='Antall Coronatilfeller.')

  return(UtData <- list(Tab=TabTidEnh, utvalgTxt=UtData$utvalgTxt, Ntest=dim(RegData)[1],
                        Tab_tidy=TabTidEnh_tidy))
}



#' Antall som er  i ECMO/respirator
#'
#' @param RegData beredskapsskjema
#'
#' @return statustabell
#' @export
#'
statusECMOrespTab <- function(RegData, valgtRHF='Alle', erMann=9, bekr=9, influ=0){

  if (influ==1){
    RegData$FormDateSiste <- RegData$FormDate
    RegData$MechanicalRespiratorStartSiste <- RegData$MechanicalRespiratorStart
    RegData$MechanicalrespiratorTypeSiste <- RegData$MechanicalRespiratorType
  }
  UtData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                             erMann=erMann, bekr=bekr)
  RegData <- UtData$RegData
  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
  inneliggere <- is.na(RegData$DateDischargedIntensive)
  AntPaaIntNaa <- sum(inneliggere) #N - sum(!(is.na(RegData$DateDischargedIntensive)))
  LiggetidNaa <- as.numeric(difftime(Sys.Date(), RegData$FormDateSiste[inneliggere], units='days'))
  LiggetidNaaGjsn <- mean(LiggetidNaa[LiggetidNaa < 90], na.rm = T)

  respLiggere <- inneliggere & is.na(RegData$MechanicalRespiratorEnd) & !(is.na(RegData$MechanicalRespiratorStart) ) #Har antatt at respiratortid MÅ registreres
  AntIrespNaa <- sum(respLiggere)
  ResptidNaa <- as.numeric(difftime(Sys.Date(), RegData$MechanicalRespiratorStartSiste[respLiggere],
                                    units='days'))
  ResptidNaaGjsn <- mean(ResptidNaa[ResptidNaa < 90], na.rm=T)
  #sjekkLiggetidResp <- as.numeric(mean(difftime(Sys.Date(), RegData$Innleggelsestidspunkt[respLiggere], units='days')))

  #MechanicalrespiratorType: -1:ikke utfylt, 1-invasiv, 2-non-invasiv
  #InvNonIBegge:
  nonInvLiggere <- respLiggere & RegData$MechanicalrespiratorTypeSiste==2
  AntNonInvNaa <- sum(nonInvLiggere)
  AntInvNaa <- sum(respLiggere & RegData$MechanicalrespiratorTypeSiste==1)
  AntUkjInv <- sum(respLiggere & RegData$MechanicalrespiratorTypeSiste==-1)

  ECMOLiggere <- inneliggere & is.na(RegData$EcmoEnd) & !(is.na(RegData$EcmoStart) )
  AntIECMONaa <- sum(ECMOLiggere)
  ECMOtidNaa <- as.numeric(difftime(Sys.Date(), RegData$EcmoStart[ECMOLiggere],
                                    units='days'))
  ECMOtidNaaGjsn <- ifelse(AntIECMONaa==0, 0,
                           mean(ECMOtidNaa[ECMOtidNaa < 90], na.rm=T))

  TabHjelp <- rbind(
    'På intensiv nå' = c(AntPaaIntNaa,'', LiggetidNaaGjsn),

    'På respirator nå' = c(AntIrespNaa*(c(1, 100/AntPaaIntNaa)), ResptidNaaGjsn),
    '...Pustehjelp på tett maske' = c(AntNonInvNaa*(c(1, 100/AntPaaIntNaa)), ''),
    '...Invasiv respiratorstøtte' = c(AntInvNaa*(c(1, 100/AntPaaIntNaa)), ''),
    '...Ikke oppgitt pustehjelp' = c(AntUkjInv*(c(1, 100/AntPaaIntNaa)), ''),
    'På ECMO nå' = c(AntIECMONaa*(c(1, 100/AntPaaIntNaa)), ECMOtidNaaGjsn)
  )
  colnames(TabHjelp) <- c('Antall', 'Andel', 'Tid (gj.sn.)')
  TabHjelp[2:6,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabHjelp[2:6,'Andel'])),'%')
  TabHjelp[c(1,2,6), 3] <- paste0(sprintf('%.1f', as.numeric(TabHjelp[c(1,2,6), 3])), ' døgn')
  xtable::xtable(TabHjelp,
                 digits=0,
                 align = c('l','r','r','r'),
                 caption='Bruk av Respirator/ECMO.')
  UtData <- list(Tab=TabHjelp, utvalgTxt=UtData$utvalgTxt, PaaIntensivNaa=inneliggere)
  return(UtData)
}


#' Ferdigstilte registreringer
#'
#' @param RegData beredskapsskjema
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return nøkkeltalltabell for ferdigstilte registreringer
#' @export
#'
oppsumFerdigeRegTab <- function(RegData, valgtRHF='Alle', datoFra='2020-01-01', datoTil=Sys.Date(),
                                bekr=9, erMann=9, resp=9, dodInt=9){

  if (valgtRHF == 'Ukjent') {valgtRHF <- 'Alle'}
  UtData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                             datoFra = datoFra,
                             datoTil = datoTil,
                             bekr = bekr,
                             dodInt = dodInt,
                             resp=resp,
                             erMann = erMann,
                             skjemastatus=2)
  RegData <- UtData$RegData
  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
  AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
  AntBruktECMO <- sum(RegData$ECMOTid>0, na.rm=T)
  #AntUtInt <- sum(RegData$DateDischargedIntensive>0, na.rm=T)
  Liggetid <- summary(RegData$Liggetid, na.rm = T)
  RespTid <- summary(RegData$RespTid, na.rm = T)
  ECMOtid <- summary(RegData$ECMOTid, na.rm = T)
  Alder <- summary(RegData$Alder, na.rm = T)
  AntDod <- sum(RegData$DischargedIntensiveStatus==1, na.rm=T)
  AntRisiko <- sum(RegData$IsRiskFactor, na.rm = T)

  med_IQR <- function(x){
    #x[is.na(x)]<-0
    c(sprintf('%.1f',x[4]), sprintf('%.1f',x[3]), paste(sprintf('%.1f',x[2]), sprintf('%.1f',x[5]), sep=' - '))
  }
  # x <- Liggetid
  #  test <- sprintf('%.2f',c(x[2],x[5]))
  # test <- med_IQR(ECMOtid)
  TabFerdigeReg <- rbind(
    'Alder (år)' = c(med_IQR(Alder), N, ''),
    'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
    'Respiratortid (døgn)' = c(med_IQR(RespTid), AntBruktResp*(c(1, 100/N))),
    'ECMO-tid (døgn)' = c(med_IQR(ECMOtid), AntBruktECMO*(c(1, 100/N))),
    'Har risikofaktor(er)' = c('','','',AntRisiko, paste0(sprintf('%.f',100*AntRisiko/N),'%')),
    'Døde' = c('','','',AntDod, paste0(sprintf('%.f',100*AntDod/N),'%'))
  )
  #TabFerdigeReg[TabFerdigeReg==NA]<-""
  colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall pasienter', 'Andel pasienter')
  TabFerdigeReg[c(3:4),'Andel pasienter'] <-
    paste0(sprintf('%.0f', as.numeric(TabFerdigeReg[c(3:4),'Andel pasienter'])),'%')
  xtable::xtable(TabFerdigeReg,
                 digits=0,
                 align = c('l','r','r','c', 'r','r'),
                 caption='Verdier på rapporteringstidspunktet.
                 For pasienter overflyttet mellom intensivavdelinger, er samlede verdier
                 talt med (total liggetid, total respiratortid).
                 IQR (inter quartile range) betyr at 25% av oppholdene er under minste verdi,
                 50% av oppholdene er i intervallet, og 25% av oppholdene er over høyeste verdi.')
  return(invisible(UtData <- list(Tab=TabFerdigeReg,
                                  utvalgTxt=UtData$utvalgTxt,
                                  Ntest=N)))
}



#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#'
#' @param RegData data
#' @param datoTil sluttdato
#' @param reshID enhetens resh
#' @param valgtRHF 'Alle' (standard), RHF-navn uten 'Helse '
#'
#' @export
#' @return tabell med andel som har ulike risikofaktorer
RisikofaktorerTab <- function(RegData, datoFra='2020-01-01', datoTil=Sys.Date(), reshID=0,
                              erMann=9, bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle',
                              resp=9, minald=0, maxald=110, velgAvd=0, sens=0){ #tidsenhet='Totalt',

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=datoFra, datoTil=datoTil, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=bekr, skjemastatus=skjemastatus,dodInt=dodInt,
                             minald=minald, maxald=maxald, resp=resp,
                             reshID=reshID, valgtRHF=valgtRHF) #velgAvd=velgAvd
  Ntest <- dim(UtData$RegData)[1]
  RegData <- UtData$RegData

  AntRisiko <- rbind(
    Kreft = sum(RegData$IsCancerPatient, na.rm = T),
    'Nedsatt immunforsvar' = sum(RegData$IsImpairedImmuneSystemIncludingHivPatient, na.rm = T),
    Diabetes	= sum(RegData$IsDiabeticPatient, na.rm = T),
    Hjertesykdom = sum(RegData$IsHeartDiseaseIncludingHypertensionPatient, na.rm = T),
    'Fedme (KMI>30)' =	sum(RegData$IsObesePatient, na.rm = T),
    Astma	= sum(RegData$IsAsthmaticPatient, na.rm = T),
    'Kronisk lungesykdom' = sum(RegData$IsChronicLungDiseasePatient, na.rm = T),
    Nyresykdom =	sum(RegData$IsKidneyDiseaseIncludingFailurePatient, na.rm = T),
    Leversykdom = sum(RegData$IsLiverDiseaseIncludingFailurePatient, na.rm = T),
    'Nevrologisk/nevromusk.' = sum(RegData$IsChronicNeurologicNeuromuscularPatient, na.rm = T),
    Graviditet	= sum(RegData$IsPregnant, na.rm = T),
    'Røyker' =	sum(RegData$IsActiveSmoker, na.rm = T),
    'Pasienter med risikofaktorer' = sum(RegData$IsRiskFactor, na.rm = T)
  )

  if (Ntest>3){
    under3 <- which(AntRisiko < 3)

    TabRisiko <- cbind(AntRisiko,
                       'Andel' = paste0(sprintf('%.0f', 100*AntRisiko/dim(RegData)[1]),'%')) #[,"Sum"]
    if (sens==1){
    TabRisiko[under3, ] <- c(rep('<3', length(under3)), rep('', length(under3)))
      }
    TabRisiko <- rbind(TabRisiko,
                       'Pasienter, totalt' = c(dim(RegData)[1], ''))
    colnames(TabRisiko) <- c('Antall', 'Andel') #c('Antall pasienter', 'Andel pasienter')

  }
  return(UtData <- list(Tab=TabRisiko, utvalgTxt=UtData$utvalgTxt, Ntest=Ntest))
}




#' Aldersfordeling, tabell
#'
#' @param RegData datatabell, beredskapsdata
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return aldersfordeling, gruppert
#' @export
#'
#' @examples
#' \dontrun{
#' TabAlder(RegData=CoroData, enhetsNivaa='HF')
#' }
TabAlderGml <- function(RegData, valgtRHF='Alle',
                     skjemastatus=9,resp=9, bekr=9,
                     dodInt=9,erMann=9){#enhetsNivaa='RHF'

  #if (valgtRHF != 'Alle'){RegData$RHF <- factor(RegData$RHF, levels=unique(c(levels(as.factor(RegData$RHF)), valgtRHF)))}
  RegData$RHF <- as.factor(RegData$RHF)
  UtData <- NIRUtvalgBeredsk(RegData=RegData,
                             #valgtRHF=valgtRHF,
                             resp=resp,
                             #bekr=bekr,
                             dodInt = dodInt,
                             erMann = erMann,
                             #skjemastatus=skjemastatus
  )
  RegData <- UtData$RegData

  # enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHF', 'RHF') #'HF')
  RegData$EnhetsNivaaVar <- RegData$RHF #RegData[ , enhetsNivaa]

  N <- dim(RegData)[1]
  gr <- seq(0, 90, ifelse(N<100, 25, 10) )
  RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
  grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
    c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
  #grtxt <- c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))#paste(gr,sep='-')
  levels(RegData$AldersGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
  TabAlder <- table(RegData$AldersGr, RegData$EnhetsNivaaVar)
  TabAlder <- addmargins(TabAlder, FUN = list(Totalt = sum), quiet = TRUE) #switch(enhetsNivaa, RHF = 'Totalt', HF = paste0(valgtRHF, ', totalt'))
  TabAlderPst <-prop.table(TabAlder[-nrow(TabAlder),],2)*100

     TabAlderAlle <- cbind(
       'Ant. pas. tot.' = TabAlder[,'Totalt'],
       'Andel pas. tot.' = paste0(sprintf('%.0f', c(TabAlderPst[,'Totalt'], 100)), ' %')
     )

     if (valgtRHF %in% levels(RegData$RHF)){
       TabAlderUt <- cbind(
         'Antall, eget RHF' = TabAlder[ ,valgtRHF],
         'Andel, eget RHF' = paste0(sprintf('%.0f', c(TabAlderPst[ ,valgtRHF], 100)), ' %'),
         TabAlderAlle)
     } else {
         #colnames(TabAlderAlle) <- c('Antall pasienter', 'Andel pasienter')
         TabAlderUt <- TabAlderAlle
         }

  return(invisible(UtData <-
                     list(Tab=TabAlderUt,
                          utvalgTxt=c(UtData$utvalgTxt, paste0('Valgt RHF: ', valgtRHF)))))
}

#' Aldersfordeling, tabell
#'
#' @param RegData datatabell, beredskapsdata
#' @param reshID avdelingsresh
#' @param enhetsNivaa enhetsnivå
#' @param sens maskere celler <3. 0-nei, 1-ja
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return aldersfordeling
#' @export
#'
#' @examples
#' \dontrun{
#' TabAlder(RegData=CoroData, enhetsNivaa='HF')
#' }
TabAlder <- function(RegData, reshID=0, enhetsNivaa='Alle',
                     skjemastatus=9, resp=9, bekr=9,
                     dodInt=9,erMann=9, sens=0){

  #HF-nivå skal se eget HF og eget RHF. Filterer derfor på RHF for HF
  egetRHF <- ifelse (enhetsNivaa=='HF',
                     RegData$RHF[match(reshID, RegData$ReshId)],
                     'Alle')

  UtData <- NIRUtvalgBeredsk(RegData=RegData,
                           valgtRHF=egetRHF,
                           resp=resp,
                           #bekr=bekr,
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
  TabAlder[under3,] <- c(rep('<3', length(under3)), rep('', length(under3))) #c('<3', '')
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
    TabAlderEget[under3,] <- c(rep('<3', length(under3)), rep('', length(under3))) #c('<3','') #
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

#' Avdelingar som enno har ikkje-ferdigstilte NIR-skjema for ferdigstilte beredskapsskjema
#' @param reshID Avdelingas resh-id
#' @return datakvalitet, mangler ferdigstilt inteneivskjema
#' @export
ManglerIntSkjema <- function(reshID=0, datoFra='2020-03-01', datoTil=Sys.Date()){
  if (rapbase::isRapContext()) {
    DataNIRraa <- intensiv::NIRRegDataSQL(datoFra = datoFra, datoTil = datoTil) #Kun ferdigstilte intensivopphold sendes til Rapporteket
    DataBeredskapRaa <- NIRberedskDataSQL(kobleInt = 0, datoFra = datoFra, datoTil = datoTil)
  } else {
    DataNIRraa <- NIRraa[as.Date(NIRraa$DateAdmittedIntensive) >= '2020-03-01', ]
    DataBeredskapRaa <- CoroDataRaa
  }

#Filtrerer
  if (reshID !=0) {
    DataNIRraa <- DataNIRraa[DataNIRraa$ReshID == reshID, ]
    DataBeredskapRaa <- DataBeredskapRaa[DataBeredskapRaa$UnitId == reshID, ]
  }
  DataBeredskapRaa <- DataBeredskapRaa[which(DataBeredskapRaa$FormStatus == 2), ]
  ManglerIntOpph <- DataBeredskapRaa[-which(DataBeredskapRaa$HovedskjemaGUID %in% DataNIRraa$SkjemaGUID),
                                     c("ShNavn", "FormDate", "SkjemaGUID", "HovedskjemaGUID", 'PasientID')]
  ManglerIntOpph$FormDate <- as.Date(ManglerIntOpph$FormDate)
  ManglerIntOpph[order(ManglerIntOpph$ShNavn, ManglerIntOpph$FormDate), ]
}


#' Tabell med andel av div. variabler for koblet datasett (intensiv+beredskap)
#' Kun ferdigstilte registreringer
#'
#' @inheritParams NIRUtvalgBeredsk
#' @param valgtRHF 'Alle' (standard), RHF-navn uten 'Helse '
#'
#' @export
#' @return andel for ulike nøkkelverdier
AndelerTab <- function(RegData, datoFra='2020-01-01', datoTil=Sys.Date(),
                       erMann=9, bekr=9, dodInt=9, valgtRHF='Alle',
                       resp=9, minald=0, maxald=110){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=datoFra, datoTil=datoTil, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=bekr, dodInt=dodInt,
                             minald=minald, maxald=maxald, resp=resp,
                             valgtRHF=valgtRHF) #velgAvd=velgAvd
  Ntest <- dim(UtData$RegData)[1]
  RegData <- UtData$RegData
  #?Tal på pasientar som har fått ARDS-diagnosen

  AntAndel <- function(var, N){
    Ant <- sum(var, na.rm = T)
    Andel <- paste0(sprintf('%.1f', 100*Ant/dim(RegData)[1]),'%')
    c(Ant, Andel)
  }

  # andelen som har registrert NAS, maks SOFA, andre diagnosar enn covid-19, sekundærårsak til innlegging på intensiv? SMR
  # Tal på pasientar som har fått ARDS-diagnosen (obs. dette er ikkje obligatorisk). Diagnose = J80 ARDS


  TabAndeler <- rbind(
    'Menn' = AntAndel(var = RegData$erMann, N=Ntest),
    'Trakeostomi' = AntAndel(var = (RegData$Trakeostomi %in% 2:3), N=Ntest),
    'Nyreestattende behandling' = AntAndel(var = (RegData$KidneyReplacingTreatment==1), N=Ntest),
    'Vasoaktiv medikasjon' =  AntAndel(var = (RegData$VasoactiveInfusion==1), N=Ntest),
    'ECMO-bruk' = AntAndel(var = (RegData$ECMOTid>0), N=Ntest),
    'Bukleie' =  AntAndel(var = (RegData$Bukleie>0), N=Ntest),
    'Temperatur mer enn 39gr.' =  AntAndel(var = (RegData$Temperature==3), N=Ntest),
    'Overflyttet' = AntAndel(var = (RegData$AntRegPrPas>1), N=Ntest),
    'Død på intensiv' = AntAndel(var = (RegData$DischargedIntensiveStatus==1), N=Ntest),
    'Død innen 30 dager' = AntAndel(var = RegData$Dod30, N=Ntest),
    'Respirator (int)' = AntAndel(var = (RegData$MechanicalRespirator==1), N=Ntest),
    'Nas registrert' = AntAndel(var = (RegData$Nas>0), N=Ntest),
    'ARDS' = AntAndel(var = RegData$ARDS) #[which(RegData$Alder>=70)]
  )

  colnames(TabAndeler) <- c('Antall', 'Andel')

  return(UtData <- list(Tab=TabAndeler, utvalgTxt=UtData$utvalgTxt, Ntest=Ntest))
}

#' Tabell med sentralmål, min,maks IQR
#'
#' @param RegData beredskapsskjema
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return tabell, sentralmål
#' @export
#'
SentralmaalTab <- function(RegData, valgtRHF='Alle', datoFra='2020-01-01', datoTil=Sys.Date(),
                                bekr=9, erMann=9, resp=9, dodInt=9){

  if (valgtRHF == 'Ukjent') {valgtRHF <- 'Alle'}
  UtData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                             datoFra = datoFra,
                             datoTil = datoTil,
                             bekr = bekr,
                             dodInt = dodInt,
                             resp=resp,
                             erMann = erMann)
  RegData <- UtData$RegData
  N <- dim(RegData)[1]

  fun <- function(x){
    x[is.na(x)] <- 0
    Tall <- round(summary(x[x>0]), 1)
    Ut <- c(Tall[1:6], sum(x>0))
    names(Ut)[7] <- 'N'
    Ut}

  SentralmaalTab <- rbind(
    'Alder (år)' = fun(RegData$Alder),
    'ECMO-tid (døgn)' = fun(RegData$ECMOTid),
    'Respiratortid (døgn)' = fun(RegData$RespiratortidInt), #RespTid
    'NonInvasivVentilation' = fun(RegData$NonInvasivVentilation),
    'InvasivVentilation' = fun(RegData$InvasivVentilation),
    'Liggetid (døgn)' = fun(RegData$Liggetid),
    'SAPSII-skåre' = fun(RegData$Saps2ScoreNumber)
  )
  return(invisible(UtData <- list(Tab=SentralmaalTab,
                                  utvalgTxt=UtData$utvalgTxt,
                                  Ntest=N)))
}

#' Vise figurdata som tabell
#' @param UtDataFraFig data fra figurfunksjoner, dvs. beregnede verdier
#' @export
#'
#'
lagTabavFigFord <- function(UtDataFraFig){
  tab <-cbind(UtDataFraFig$Ngr$Hoved,
              UtDataFraFig$N$Hoved,
              UtDataFraFig$AggVerdier$Hoved,
              UtDataFraFig$Ngr$Rest,
              UtDataFraFig$N$Rest,
              UtDataFraFig$AggVerdier$Rest)
  grtxt <- UtDataFraFig$grtxt
  if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
    grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
  rownames(tab) <- grtxt
  kolnavn <- c('Teller', 'Nevner' , 'Andel (%)')
  colnames(tab) <- c(kolnavn, if(!is.null(UtDataFraFig$Ngr$Rest)){kolnavn})
  # colnames(tab) <- c(paste0(UtDataFraFig$hovedgrTxt,', Antall'),
  #                    paste0(UtDataFraFig$hovedgrTxt, ', Andel (%)'),
  #                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt,', Antall')},
  #                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt, ', Andel (%)')})

  return(tab)
}


#' Tabell som viser fordeling av registreringsforsinkelse per enhet
#'
#' @param RegData registerdata, IKKE personaggregert
#' @param innUt 1-forsinkelse innreg, 2-forsinkelse, ferdigstille utskr.
#' @param datoFra dato, fra og med
#' @param datoTil dato, til og med
#' @param pst 0-antall, 1-prosent
#'
#' @return tabell, registreringsforsinkelse
#' @export
#'
tabRegForsinkelse <- function(RegData, innUt=1, datoFra='2020-03-01', datoTil=Sys.Date(), pst=1){ #,  nivaa='ShNavn'

  RegData <- NIRUtvalgBeredsk(RegData, datoFra=datoFra, datoTil=datoTil)$RegData
    RegData$RegForsink <- switch(innUt,
                                 '1' = as.numeric(difftime(RegData$CreationDate,
                                                                         RegData$Innleggelsestidspunkt, units = 'days')),
                                 '2' = as.numeric(difftime(RegData$FirstTimeClosed,
                                                                        RegData$DateDischargedIntensive, units = 'days'))
    )
    RegData <- RegData[which(RegData$RegForsink>0), ] #which(!is.na(RegData$RegForsink))
    gr <- c(0,1:7,500) #gr <- c(seq(0, 90, 10), 1000)
    RegData$VariabelGr <- cut(RegData$RegForsink, breaks = gr, include.lowest = TRUE, right = TRUE)
    levels(RegData$VariabelGr) <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '7+')

    tab <- table(RegData$ShNavn, RegData$VariabelGr)
    tab <- rbind(tab,
                 'Hele landet' = colSums(tab))

    if (pst==1) {tab <- prop.table(tab, margin = 1)*100}
    return(tab)

      }
