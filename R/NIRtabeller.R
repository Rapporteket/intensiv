# Må det kanskje komme en overornet tittel her?
#---------------------------------------------

#' Funksjoner for å lage tabeller Group of functions page title
#' 
#' Fil som beregner div tabeller.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' @section Finne reinnleggelser After function section:
#' Despite its location, this actually comes after the function section.
#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer 
#' Aktuelle tabeller:
#' -Belegg (samlerapport), antall opphold per år og enhet, ant. pasienter per år og enhet, ant opph. per måned og enhet.
#' -tabAntOpphSh12mnd: Antall opphold per måned og enhet siste 12 måneder fram til datoTil. 
#' RegData må inneholde InnDato.
#' -tabAntOpphSh5Aar:Antall opphold per år og enhet siste 5 år (inkl. inneværende år) fram til datoTil. 
#' RegData må inneholde Aar.
#' 
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes 
# @inheritParams NIRFigAndeler
#' @return Div tabeller
#' @name NIRtabeller
NULL
#' @rdname NIRtabeller
#' @export

#' @section Antall opphold per enhet og måned:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#' @rdname hjelpeFunksjoner
#' @export
tabAntOpphSh12mnd <- function(RegData, datoTil){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive og PasientID
      RegData$Mnd <- as.yearmon(RegData$InnDato)
      datoFraMnd <- as.Date(paste0(1900+datoTil$year,'-', ifelse(datoTil$mon==0, 11, datoTil$mon), '-', '01')) #dato - 
      datoFra12 <- as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      RegData12mnd <- RegData[RegData$InnDato < as.POSIXlt(datoTil, tz='UTC')
                              & RegData$InnDato > as.POSIXlt(datoFra12, tz='UTC'), ]
      tabAvd12mnd <- addmargins(table(RegData12mnd[, c('ShNavn', 'Mnd')]))
      colnames(tabAvd12mnd) <- substring(colnames(tabAvd12mnd),1,3)
      xtable::xtable(tabAvd12mnd)
}

tabAntOpphSh5Aar <- function(RegData, datoTil){
      AarNaa <- as.numeric(format(datoTil, "%Y"))
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      xtable::xtable(tabAvdAarN)
}

tabAntPasSh5Aar <- function(RegData, personIDvar='PasientID' , datoTil){
      AarNaa <- as.numeric(format(datoTil, "%Y"))
      Data <- RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar', personIDvar)]
      tabPasAvdAarN <- tapply(Data$PasientID, Data[ c('ShNavn','Aar')], FUN=function(x) length(unique(x)))
      tabPasAvdAarN[is.na(tabPasAvdAarN)] <- 0
      tabPasAvdAarN <- addmargins(tabPasAvdAarN) #, FUN = function(x) sum(x, na.rm=T))
      rownames(tabPasAvdAarN)[dim(tabPasAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabPasAvdAarN)[dim(tabPasAvdAarN)[2] ]<- 'TOTALT'
      xtable::xtable(tabPasAvdAarN)
}
tabBelegg <- function(RegData, personIDvar='PasientID' , tidsenhet='Aar') {
      Mtid <- SorterOgNavngiTidsEnhet(RegData, tidsenhet='Kvartal')
      RegData <- Mtid$RegData
      tabBelegg <- rbind('Ferdigstilte intensivopphald' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,		
                         'Registrerte pasientar' = tapply(RegData$PasientID, RegData$TidsEnhet, 
                                                          FUN=function(x) length(unique(x))),	
                         'Antal intensivdøger' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0)	
                         #sum(RegDataEget$DaysAdmittedIntensiv, na.rm=T))
      )
      
      tabBelegg <- tabBelegg[, max(1, dim(tabBelegg)[2]-11) : dim(tabBelegg)[2]] #Tar med 12 siste
      # overskr <- dimnames(tabAvdNEget)[[2]]
      # aar <- substr(overskr, 1,2)
      # mnd <- as.numeric(substr(overskr, 4,5))
      # mndTxt <- c('jan', 'feb', 'mar', 'apr', 'mai', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'des') 
      # colnames(tabAvdNEget) <- paste0(mndTxt[mnd], aar)
      xtable(tabBelegg)
}
finnDblReg <- function(RegData, reshID=114240){
      #Registreringer kor same pasient har fått registrert to innleggingar med mindre enn 2 timars mellomrom.
      #RegData må inneholde PasientID, Innleggelsestidspunkt og SkjemaGUID
      #Evt. legge til utvalg på tidsrom
      sortVar <- c('PasientID','Innleggelsestidspunkt', "SkjemaGUID")
      Data <- RegData[which(RegData$ReshId == reshID), sortVar]
      RegDataSort <- RegData[order(RegData$PasientID, RegData$Innleggelsestidspunkt), ]
      RegDataSort$OpphNr <- ave(RegDataSort[ ,'PasientID'], RegDataSort[ ,'PasientID'], FUN=seq_along)
      indPasFlereOpph <- which(RegDataSort$OpphNr>1) 
      RegDataSort$TidInn <- NA
      RegDataSort$TidInn[indPasFlereOpph] <- 
            difftime(RegDataSort$Innleggelsestidspunkt[indPasFlereOpph], 
                     RegDataSort$Innleggelsestidspunkt[indPasFlereOpph-1], 
                     units = 'hour')
      
      indDbl <- which(abs(RegDataSort$TidInn) <2 )
      tabDbl <- RegDataSort[sort(c(indDbl, indDbl-1)), 
                            c('PasientID','Innleggelsestidspunkt', "SkjemaGUID")]
}
