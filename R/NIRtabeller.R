#' Funksjoner for å lage tabeller Group of functions page title
#' 
#' Fil som beregner div tabeller.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
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
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
# @inheritParams NIRFigAndeler
#' @return Div tabeller
#' @name NIRtabeller
NULL
#' @rdname NIRtabeller
#' @export

#' @section Belegg (antall opphold, pasienter og intensivdøgn)
#' Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @rdname NIRtabeller
#' @export
tabBelegg <- function(RegData, personIDvar='PasientID' , tidsenhet='Aar') {
      Mtid <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet)
      RegData <- Mtid$RegData
      tabBeleggAnt <- rbind('Ferdigstilte intensivopphald' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,		
                            'Registrerte pasientar' = tapply(RegData$PasientID, RegData$TidsEnhet, 
                                                             FUN=function(x) length(unique(x))),	
                            'Antal intensivdøger' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0)	
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
#' @section tabAntOpphSh12mnd (antall opphold siste 12 mnd)
#' @rdname NIRtabeller
#' @export
tabAntOpphSh12mnd <- function(RegData, datoTil){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive 
      RegData$Mnd <- format(RegData$InnDato, '%b%y')
      datoFraMnd <- as.Date(paste0(1900+datoTil$year,'-', ifelse(datoTil$mon==0, 11, datoTil$mon), '-', '01')) #dato - 
      datoFra12 <- as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      RegData12mnd <- RegData[RegData$InnDato < as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra12, tz='UTC'), ]
      tabAvd12mnd <- addmargins(table(RegData12mnd[, c('ShNavn', 'Mnd')]))
      colnames(tabAvd12mnd) <- substring(colnames(tabAvd12mnd),1,3)
      tab <- xtable::xtable(tabAvd12mnd)
	return(tab)
}

#' @section Antall opphold siste 5 år
#' Hmmm
#' @rdname NIRtabeller
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}

#' @section Antall registreringer siste 5 år:
#' Hmmm
#' @rdname NIRtabeller
#' @export
tabAntPasSh5Aar <- function(RegData, personIDvar='PasientID' , datoTil){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
      Data <- RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar', personIDvar)]
      tabPasAvdAarN <- tapply(Data$PasientID, Data[ c('ShNavn','Aar')], FUN=function(x) length(unique(x)))
      tabPasAvdAarN[is.na(tabPasAvdAarN)] <- 0
      tabPasAvdAarN <- addmargins(tabPasAvdAarN) #, FUN = function(x) sum(x, na.rm=T))
      rownames(tabPasAvdAarN)[dim(tabPasAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabPasAvdAarN)[dim(tabPasAvdAarN)[2] ]<- 'TOTALT'
      tabPasAvdAarN <- xtable::xtable(tabPasAvdAarN)
      return(tabPasAvdAarN)
}

#' @section Finn eventuelle dobbeltregistreringer
#' @rdname NIRtabeller
#' @export
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
      return(tabDbl)
}
