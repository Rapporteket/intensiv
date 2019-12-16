# Må det kanskje komme en overordnet tittel her?
#---------------------------------------------

#' Hjelpefunksjoner. Group of functions page title
#' 
#' Fil med div hjelpefunksjoner.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' Fil som inneholder hjelpefunksjoner. 
#' FinnReinnleggelser beregner reinnleggelser fra DateAdmittedIntensive og DateDischargedIntensive
#' SorterOgNavngiTidsEnhet Legger til tidsenhetene Aar, Halvaar, Mnd og Kvartal
#' 
#' 
#' @param RegData data
#' @param PasientID Variabelen som angir pasientidentifikasjon
#' @inheritParams NIRFigAndeler
#' @return Div hjelpefunksjoner
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
            difftime(as.POSIXlt(RegDataSort$DateAdmittedIntensive[indPasFlereOpph], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
                     as.POSIXlt(RegDataSort$DateDischargedIntensive[indPasFlereOpph-1], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
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

#' Tilrettelegge tidsenhetvariabel:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar', tab=0) {
      #Lager sorteringsvariabel for tidsenhet:
      RegData$TidsEnhetSort <- switch(tidsenhet,
                                      Aar = RegData$Aar-min(RegData$Aar)+1,
                                      Mnd = RegData$MndNum-min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                          +(RegData$Aar-min(RegData$Aar))*12, #format(RegData$InnDato, '%b%y'), #
                                      Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*4,
                                      Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*2
      )
      format.Date(seq(from=as.Date('2018-01-01'), 
                      to=as.Date('2018-09-01'), by='month'), format = '%b%y')
      
      tidtxt <- switch(tidsenhet,
                       #Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                        #           sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       #Mnd = RegData$MndAar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)],
                       # Mnd = format.Date(seq(from=min(as.Date(RegData$InnDato), na.rm = T), 
                       #                       to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%b%y'),
                       #Henter fullt månedsnavn og forkorter etterpå.
                       Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$InnDato), na.rm = T)), 'month'), 
                                         to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))
      
      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[tab], substrRight(tidtxt, 2))}
      #RegData$TidsEnhetSort <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)
      #RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE, labels=tidtxt)
      #a <- factor(c(1:10,3,2,4,3,7,9,4), levels=1:11, labels = letters[1:11])
#table(a)

#      måned og år som faktor i riktig rekkefølge og med alle måneder inkludert
#      RegData$MndAar <- factor(format(RegData $HovedDato, format='%b-%y'), 
      #levels = format(seq(as.Date(datoFra),as.Date(datoTil), by="month"), "%b-%y"))
      
      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}

#' Lage tulledata (simulerte data)
#'
#' @param RegData Dataramme
#' @param varBort variable som skal fjernes fra RegData
#' @param antSh antall simulerte sykehus
#' @param antObs antall rader i det fiktive datasettet
#'
#' @export
lageTulleData <- function(RegData, varBort='', antSh=26, antObs=20000) {
      library(synthpop)
      library(dplyr)
      #ForlopsID <- RegData$ForlopsID
      RegData <- RegData[,-which(names(RegData) %in% varBort)]
      RegData <- RegData[sample(1:dim(RegData)[1], antObs, replace = T),]
      sykehus <- paste('Sykehus', LETTERS[1:antSh])
      fordelingPasienter <- sample(1:10,antSh, replace = TRUE)
      RegData$ShNavn <- sample(sykehus, prob=fordelingPasienter/sum(fordelingPasienter), size=dim(RegData)[1], replace = TRUE)
      RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500) #Trekker med tilbakelegging
      RegData <- data.frame(RegDataSyn$syn) 
	  return(RegData)
}

#' Legge til indikator for intervensjon, pårørendeoppfølging
#' @param RegData Dataramme
#' @param startDatoIntervensjon startdato for intervensjon. Foreslått verdi '2016-01-01' basert 
#' på de første studiene som ble gjort med pårørendeskjema
#' @param sluttDatoIntervensjon sluttdato for 
#' @export
leggTilIntervensjon <- function(RegData, #startDatoPre='2011-01-01', sluttDatoPre='2016-10-01', 
                                 startDatoIntervensjon='2016-10-01', sluttDatoIntervensjon=Sys.Date()){
      RegData$Intervensjon <- 0
      RegData$Intervensjon[intersect(which(RegData$InnDato >= as.Date(startDatoIntervensjon)),
                                     which(RegData$InnDato <= as.Date(sluttDatoIntervensjon)))] <- 1
      return(RegData)
} 

#' Automatisk linjebryting av lange tekstetiketter
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#' @export
delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
{sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
        USE.NAMES = FALSE)
}

#' Legge til indikator for intervensjon, pårørendeoppfølging
#' @param HovedSkjema Registerets hovedskjma (Main..)
#' @param Skjema2 Skjemaet som skal kobles til hovedskjema. (Pårørendeskjema, Influensaskjema)
#' @param alleHovedskjema TRUE/FALSE. standard: FALSE. I praksis om vi skal ha en left? join eller ikke
#' @param alleSkjema2 TRUE/FALSE.standard: FALSE I praksis om vi skal ha en right? join eller ikke
#' @export
KobleMedHoved <- function(HovedSkjema, Skjema2, alleHovedskjema=F, alleSkjema2=F) {
  #HovedSkjema <- plyr::rename(HovedSkjema, c('FormDate' = 'FormDateHoved'))
  varBegge <- intersect(names(HovedSkjema),names(Skjema2)) ##Variabelnavn som finnes i begge datasett
  Skjema2 <- Skjema2[ , c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",   
  data <- merge(HovedSkjema, Skjema2, suffixes = c('','_S2'),
                by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = alleHovedskjema, all.y=alleSkjema2)
  return(data)
}

    
#' Funksjon som produserer rapporten som skal sendes til mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending (\emph{dvs uten  ".Rnw"})
#' @param reshID Aktuell reshid
#' @param filnavn sdf  
#' @param datoFra dato
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#'
#' @return Full path of file produced
#' @export

henteSamlerapporter <- function(filnavn, rnwFil, reshID=0, 
                                datoFra=Sys.Date()-180, datoTil=Sys.Date()) {
    Rpakke <- 'intensiv'
    tmpFile <- paste0('tmp',rnwFil)
    src <- normalizePath(system.file(rnwFil, package=Rpakke))
    # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
    #owd <- 
      setwd(tempdir())
    file.copy(src, tmpFile, overwrite = TRUE)
    
    knitr::knit2pdf(tmpFile)
    
    gc() #Opprydning gc-"garbage collection"
    file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
    # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
}

#' Funksjon som produserer rapporten som skal sendes til mottager.
#' (The actual call to this function is made through do.call and 
#' has the effect of providing the parameters as class
#' \emph{list}. Verdier gis inn som listeparametre 
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis MED filending (\emph{dvs "filnavn.Rnw"})
#' @param reshID Aktuell reshid
#' @param datoFra dato
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#'
#' @return Full path of file produced
#' @export

abonnement <- function(rnwFil, brukernavn='tullebukk', reshID=0, 
                                datoFra=Sys.Date()-180, datoTil=Sys.Date()) {
  
  #function(baseName, reshId, registryName,author, hospitalName, type) {
    
  raplog::subLogger(author = brukernavn, registryName = 'Norsk Intensivregister',
                    reshId = reshID[[1]],
                    msg = "Abonnement: månedsrapport")
  # raplog::subLogger(author = author[[1]], registryName = registryName[[1]],
  #                     reshId = reshId[[1]],
  #                     msg = "Subscription report: stent/prosedyre")
  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='intensiv'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #owd <- 
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile) 
  
  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')
  #utfil <- file.copy(from = paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), 
   #         to = paste0(filbase, digest::digest(brukernavn),'.pdf')) #filnavn)
  return(utfil)
}
  