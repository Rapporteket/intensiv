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
#' @rdname NIRtabeller
#' Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @export
tabBelegg <- function(RegData, tidsenhet='Aar', datoTil, enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet, 
                        Mnd = floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil, 
                             enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet)$RegData
      #RegData <- Mtid$RegData
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
#' @section tabAntOpphShMnd antall opphold siste X (antMnd) mnd
#' @rdname NIRtabeller
#' @export
tabAntOpphShMnd <- function(RegData, datoTil, antMnd=6){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive 
      datoFra <- floor_date(as.Date(datoTil)%m-% months(antMnd, abbreviate = T), 'month') #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(ymd(colnames(tabAvdMnd1)), '%b%y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd1 <- addmargins((tabAvdMnd1))
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1)
	return(tabAvdMnd1)
}

#' @section Antall opphold siste 5 år
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

#' @section Antall registreringer/pasienter siste 5 år:
#' Hmmm
#' @rdname NIRtabeller
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

#' @section Finn eventuelle dobbeltregistreringer
#' @rdname NIRtabeller
#' @export
finnDblReg <- function(RegData, datoFra='2017-01-01', datoTil=Sys.Date(), reshID=114240){
      #Registreringer kor same pasient har fått registrert to innleggingar med mindre enn 2 timars mellomrom.
      #RegData må inneholde PasientID, Innleggelsestidspunkt og SkjemaGUID
      #Evt. legge til utvalg på tidsrom
      sortVar <- c('PasientID','Innleggelsestidspunkt', "SkjemaGUID")
      RegData <- RegData[which(RegData$ReshId == reshID), sortVar]
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
      if (dim(tabDbl)[1] == 0) {
            tabDbl <- 'Ingen dobbeltregistreringar'
      } else {tabDbl <- xtable::xtable(tabDbl)}
      #print(paste('Dim RegDATA: ',  dim(RegData), min(RegData$InnDato)))
      return(tabDbl)
}


#' @section Nøkkeltall (antall opph., pasienter,  intensivdøgn, samt div oversiktstall)
#' @rdname NIRtabeller
#' Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @export
tabNokkeltall <- function(RegData, tidsenhet='Mnd', datoTil, enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet, 
                        Mnd = floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil, 
                              enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet)$RegData
      #NB: sjekk riktige utvalg!!!
      indLigget <- which(RegData$liggetid>0)
      indRespt <- which(RegData$respiratortid>0)
      indSAPS <- which(RegData$SAPSII > 0)
      indNEMS <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))
      RegData <- FinnReinnleggelser(RegData=RegData, PasientID = 'PasientID')
      indReinn <- intersect(which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC')), which(RegData$Overf==1))
      ind1708 <- union(which(RegData$DateDischargedIntensive$hour<8), which(RegData$DateDischargedIntensive$hour>=17))
      RegData$Ut1708 <- 0
      RegData$Ut1708[ind1708]<-1
      
      tabNokkeltall <- rbind(
            'Antall opphold' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,		
            'Antall pasienter' = tapply(RegData$PasientID, RegData$TidsEnhet, 
                                             FUN=function(x) length(unique(x))),	
            'Antall intensivdøgn' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0),
            'Liggetid (median)' = tapply(RegData$liggetid[indLigget], RegData$TidsEnhet[indLigget], FUN=median, na.rm=T),
            'Respiratorstøtte (%)' = tapply(RegData$respiratortid>0, RegData$TidsEnhet, 
                                            FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            'Respiratortid (median)' = tapply(RegData$respiratortid[indRespt], RegData$TidsEnhet[indRespt], 
                                              FUN=median, na.rm=T),
            'SAPS II (median)' = tapply(RegData$SAPSII[indSAPS], RegData$TidsEnhet[indSAPS], FUN=median, na.rm=T),
            'NEMS per opph. (median)' = tapply(RegData$NEMS[indNEMS], 
                                               RegData$TidsEnhet[indNEMS], FUN=median, na.rm=T),
            'Døde (%)' = tapply((RegData$DischargedIntensiveStatus==1), RegData$TidsEnhet, 
                                FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            'Reinnleggelser (<72t)' = tapply(RegData$Reinn==1, RegData$TidsEnhet, 
                                             FUN=function(x) sum(x, na.rm=T)/length(x)*100),
            'Utskrevet 17-08 (%)' = tapply(RegData$Ut1708, RegData$TidsEnhet, 
                                           FUN=function(x) sum(x, na.rm=T)/length(x)*100)
      )
      #tabNokkeltall[,4:11] <- round(tabNokkeltall[,4:11],1)
      #dplyr::mutate_at(as.table(tabNokkeltall), vars(), funs(round(., 1)))
      #antTidsenh <- ifelse(tidsenhet=='Aar', 4, 11)
      #tabBeleggAnt <- tabBeleggAnt[, max(1, dim(tabBeleggAnt)[2]-antTidsenh) : dim(tabBeleggAnt)[2]] #Tar med 12 siste
      
      return(tabNokkeltall)
}
      
