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
#' @return Div hjelpefunksjoner
#' @export

FinnReinnleggelser <- function(RegData, PasientID='PasientID'){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive og PasientID
      #SJEKK Bare innleggelser fra 2016 som skal ha reinnleggelse??
      #RegData <- RegData[
      #     as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d %H:%M:%S") >= as.POSIXlt('2016-01-01'), ]
      N <- dim(RegData)[1]
      RegData$PasientID <- RegData[ ,PasientID]
      #TabAntOpph <- table(RegData$PasientID) #Tar relativt lang tid.
      #TabFlereOpph <- TabAntOpph[TabAntOpph>1]
      #indPasFlereOpph <- which(RegData$PasientID %in% names(TabFlereOpph))  #Tar relativt lang tid.
      RegDataSort <- RegData[order(RegData$PasientID, RegData$DateAdmittedIntensive,   #Denne tar mest tid
                                   RegData$DateDischargedIntensive), ]
      #RegDataSort$AntOpph <- ave(RegDataSort$PasientID, RegDataSort$PasientID, FUN=length)
      RegDataSort$OpphNr <- ave(RegDataSort$PasientID, RegDataSort$PasientID, FUN=seq_along)
      indPasFlereOpph <- which(RegDataSort$OpphNr>1) #intersect(which(RegDataSort$AntOpph>1), which(RegDataSort$OpphNr>1))
      RegDataSort$TidUtInn <- NA
      RegDataSort$TidUtInn[indPasFlereOpph] <-
            difftime(as.POSIXlt(RegDataSort$DateAdmittedIntensive[indPasFlereOpph], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
                     as.POSIXlt(RegDataSort$DateDischargedIntensive[indPasFlereOpph-1], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
                     units = 'hour')
      RegDataSort$SmResh <- c(FALSE, RegDataSort$ReshId[2:N] == RegDataSort$ReshId[1:N-1])
      RegDataSort$Reinn <- 2 #Ikke reinnleggelse
      RegDataSort$Reinn[RegDataSort$TidUtInn<72 & RegDataSort$TidUtInn >= 0] <- 1 #Reinnleggelse
      RegDataSort$Reinn[!(RegDataSort$SmResh)] <- 2

      #Div testing:
      #RegDataSort[indPasFlereOpph[1:20], c('OpphNr',"TidUtInn", 'ReshId','SmResh','Reinn', 'PasientID')]
      #indNeg <- which(RegDataSort$TidUtInn < 0)
      #TabDobbeltRegSjekk <- RegDataSort[sort(c(indNeg-1,indNeg)), ]
      #write.table(TabDobbeltRegSjekk, file='TabDobbeltRegSjekk.csv', row.names = F, sep = ';')
      #write.table(RegDataSort, file='RegDataSort.csv', row.names = F, sep = ';')
      #RegDataSort[1:20,]
      #table(RegDataSort$ReAdmitted)
      #table(RegDataSort$Reinn)
      #RegDataSort$PasientID[1:20,]
      #RegDataSort$TidUtInn[indNeg[1:5]]
      return(RegDataSort)
}

#' Tilrettelegge tidsenhetvariabel:
#' @param RegData dataramme
#' @param tidsenhet 'Aar' (standard), 'Halvaar', 'Kvartal', 'Mnd'
#' @param tab husker ikke hva denne gjør...
#'
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
      #format.Date(seq(from=as.Date('2018-01-01'),
                     # to=as.Date('2018-09-01'), by='month'), format = '%b%y')
      tidtxt <- switch(tidsenhet,
                       #Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                        #          sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       #Mnd = RegData$MndAar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)],
                       #Mnd = format.Date(seq(from=min(as.Date(RegData$InnDato), na.rm = T),
                       #                      to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%b%y'),
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

#     måned og år som faktor i riktig rekkefølge og med alle måneder inkludert
#     RegData$MndAar <- factor(format(RegData $HovedDato, format='%b-%y'),
      #levels = format(seq(as.Date(datoFra),as.Date(datoTil), by="month"), "%b-%y"))

      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}

#' Lage tulledata (simulerte data)
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

#' Koble et annet skjema til hovedskjema
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

#' Telle antall influensaskjema for en gitt periode
#'
#' @param datoFra fra og med dato
#' @param datoTil til og med dato
#' @param reshID resh for aktuell enhet
#'
#' @return Tekst med ant. influensaskjema
#' @export
#'
tellInfluensa <- function(datoFra='2020-09-01', datoTil=Sys.Date(), reshID=0){
  q <- paste0('SELECT *
  FROM InfluensaFormDataContract
WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  RegData <- rapbase::loadRegData(registryName= "nir", query=q)

  antReg <- sum(RegData$UnitId == reshID)

  txt <- paste0('Antall influensaskjema registrert i perioden ', datoFra, ' - ', datoTil,
                ' for ReshId ', reshID, ' er: ', antReg )
  return(txt)
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
    Rpakke <- ifelse(rnwFil==as.character('NIRinfluensa.Rnw'), 'intensivberedskap', 'intensiv')
    tmpFile <- paste0('tmp',rnwFil)
    src <- normalizePath(system.file(rnwFil, package=Rpakke))
    #gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
    #owd <-
      setwd(tempdir())
    file.copy(src, tmpFile, overwrite = TRUE)

    knitr::knit2pdf(tmpFile)

    gc() #Opprydning gc-"garbage collection"
    file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
    #file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
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

  Rpakke <- ifelse(rnwFil==as.character('NIRinfluensa.Rnw'), 'intensivberedskap', 'intensiv')
  #rapbase::subLogger(author = brukernavn, registryName = 'Norsk Intensivregister',
  #                  reshId = reshID,
  #                  msg = paste0("1 starter Abonnement:", rnwFil ))
  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package=Rpakke))
  #gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  #rapbase::subLogger(author = brukernavn, registryName = 'Norsk Intensivregister',
  #                  reshId = reshID,
  #                  msg = paste0("2 Klar til å strikke", tmpFile))
  knitr::knit2pdf(input=tmpFile)
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  #rapbase::subLogger(author = brukernavn, registryName = 'Norsk Intensivregister',
  #                  reshId = reshID,
  #                  msg = paste("3 Leverer: ", utfil))
  return(utfil)
}


#' Generere data til interaktive nettsider SKDE
#' @param filUt tilnavn for utdatatabell (fjern?)
#' @param valgtVar - beinsmLavPre, peropKompDura, sympVarighUtstr, p.t. 10 kvalitetsind.
#' @param indID indikator-id, eks. 'ind1', 'ind2', osv.
#' @inheritParams NIRUtvalgEnh
#' @return Datafil til interaktive nettsider, SKDE
#' @export

dataTilOffVisning <- function(RegData = RegData, valgtVar, #datoFra='2016-01-01', datoTil=Sys.Date(),
                              aar=0,
                              indID = 'indDummy', filUt='dummy'){

  filUt <- paste0('Intensiv_', filUt, '.csv')
  resultatVariabler <- c('Aar', "ShNavn", "ReshId", "Variabel")
  DataVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')$RegData
  RegDataUt <- NIRUtvalgEnh(RegData=DataVarSpes, aar = aar)$RegData[ , resultatVariabler]
  #nytt navn = gammelt navn
    RegDataUt <- dplyr::rename(RegDataUt,
                               year = Aar,
                               var = Variabel)
                  
                          
                     
                 
    #Legge på orgID ("Sykehusviser")
    #ReshId	orgnr	RapporteketNavn	SKDEnavn
  nyID <- c('102090'='974706490', #AHUS - Intensiv
  '4215368' = '974706490', #AHUS - Kongsvinger
  '4207303' = '974706490', #AHUS - Med. Overvåkning 
  '4205696'='974706490', #AHUS - Postop
  '100089'='974706490', #Akershus universitetssykehus HF
  '111487'='974588951', #Aker
  '104450'='974631091', #Arendal
  '4210053'='974795361', #Bodø
  '103090'='974705788', #Bærum
  '108897'='974116804', #Diakonhjemmet
  '103620'='974631326', #Drammen
  '108609'='974631768', #Elverum
  '105282'='974744570', #Førde
  '108618'='974632535', #Gjøvik
  '108610'='974724960', #Hamar
  '101858'='974795833', #Hammerfest
  '100180'='974316285', #Haraldplass
  '700617'='974795639', #Harstad
  '100273'='974724774', #Haugesund
  '109636'='974557746', #Haukel. Brannsk, Riktig resh
  '109363'='974557746', #Haukel. Brannsk, Feil resh
  '112044'='974557746', #Haukel. KSK Int.
  '105048'='974557746', #Haukel. MIO
  '107930'='974557746', #Haukel. MOE  
  '106271'='974557746', #Haukel. Postop
  '107717'='974557746', #Haukel. ROE
  '106285'='974557746', #Haukel. TIO
  '100082'='983974724', #Helse Bergen HF  NB: HF-resh
  '4209889'='974633752', #KalnesØstf.
  '101830'='974795930', #Kirkenes
  '4208715'='974631385', #Kongsberg
  '114240'='974733013', #Kristiansand
  '706078'='974746948', #Kristiansund
  '102250'='974754118', #Levanger
  '108626'='874632562', #Lillehammer
  '4208892'='974207532', #Lovisenberg Diakonale Sykehus
  '103015'='974795515', #Mo i Rana
  '706079'='974745569', #Molde
  '103141'='974795485', #Mosjøen
  '4208039'='974633698', #Moss
  '105893'='974753898', # Namsos
  '700618'='974795396', #Narvik
  '705757'='974707152', #Radiumhospitalet
  '705576'='874716782', #RH Barneintensiv
  '705577'='874716782', #RH Gen Int 1
  '706929'='874716782', #RH Gen Int 2
 '106033' = '874716782', #RH Hjertemed int og overvåkn  
  '700419'='874716782', #RH samlet
  '705699'='874716782', #RH Thorax 1 
  '103539'='974631407', #Ringerike
  '103149'='974795477', #Sandnessjøen
  '102026'='974633191', #Skien
  '4201313'='974749025', #St. Olav Hovedint
 '4208928'='974749025', #St. Olavs Intensiv barn
 '106572'='974749025', #St. Olav Med int
  '4205574'='974749025', #St. Olav Med lunge
  '105790' = '974749025', #St. Olavs Nevrointensiv
  '706032' = '974749025', #St. Olavs  Thoraxkirurgisk intensivavdeling
  '114282'='974703300', #Stavanger
  '701785'='974703300', #Stavanger univ.sjukehus - Postop. 1G
  '4207985'='974742985', #Stord 
  '100132'='983975267', #Sykehuset Telemark HF NB: HF-resh
  '700720'='974795787', #Tromsø Intensivmedisinsk
  '700619'='974795787', #Tromsø Kir. int.
  '601302'='974795787', #Tromsø Med int
  '700620'='974795787', #Tromsø Postop
  '4217939' ='974795787', #Tromsø, Oppvåkning
  '103948'='974633574', #Tønsberg
  '105101'='974633574', #Tønsberg Med. Over.
  '109870'='974589095', #Ullevål Akuttmed Int
  '111449'='974589095', #Ullevål Barneint
  '109773'='974589095', #Ullevål Gen int
  '109877'='974589095', #Ullevål Hjerte-PO
  '4205969'='974589095', #Ullevål Hjertemed Int
  '109779'='974589095', #Ullevål Nevroint
  '109778'='974589095', #Ullevål Postop
  '110867'='974795574', #Vesterål.Stokm.
  '4209764'='974747545', #Volda
  '4213593'='974747545', #Volda
  '108308'='974747138', #Ålesund Kir
  '102673'='974747138') #Ålesund Med

nyResh <- setdiff(unique(RegDataUt$ReshId), names(nyID))
#RegDataUt[match(nyResh, RegDataUt$ReshId), 'ShNavn']
  RegDataUt$orgnr <- as.character(nyID[as.character(RegDataUt$ReshId)])
  RegDataUt$ind_id <- indID
  RegDataUt$denominator <- 1
  RegDataUt$context <- 'caregiver'

  names(RegDataUt)
  RegDataUt <- RegDataUt[ ,c('year', 'orgnr', 'var', 'denominator', 'ind_id', "context")]

write.table(RegDataUt, file = filUt, sep = ';', row.names = F) #, fileEncoding = 'UTF-8')
return(invisible(RegDataUt)) #return(IntensivKvalInd)
}

