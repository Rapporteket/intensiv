#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'     \item 3: Egen enhet mot egen sykehustype
#'     \item 4: Egen sykehustype
#'     \item 5: Egen sykehustype mot resten av landet
#'     \item 6: Egen enhet mot egen region
#'     \item 7: Egen region
#'	 \item 8: Egen region mot resten
#'    	}
#'    	Hvis egen avdeling ikke har registreringer, vises hele landet.
#'
#' @inheritParams NIRFigAndeler
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren). Standard 2019-01-01.
#' Registeret inneholder registreringer f.o.m. 2011
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 110)
#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param InnMaate
#'				0: Elektivt,
#'				6: Akutt medisinsk,
#'				8: Akutt kirurgisk,
#'				standard: alle (alt unntatt 0,6,8 / ikke spesifisert)
#' @param dodInt Levende/død ut fra intensiv.
#'				0: i live,
#'				1: død,
#'				alle: standard (alle andre verdier)
#' @param overfPas Overført under pågående intensivbehandling?
#'				1 = Pasienten er ikke overført
#'				2 = Pasienten er overført
#'	@param velgDiag Velge diagnose. 0-alle, 1-Bekreftede Covid-pasienter, 2-alle unntatt Covid-pasienter
#'	@param nivaa Intensivavdelingens "nivå": 1-5: 'Overvåk', 'Postop', 'Gen<50%','Gen>50%', 'Barn'
#'	Flervalgsparameter
#' @param grType Gjør gruppeutvalg på sykehustype - fases ut !!
#'                      1: lokal-/sentralsykehus
#'                      2: lokal-/sentralsykehus
#'                      3: regionsykehus
#'                      99: alle (standard)
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalgEnh <- function(RegData, datoFra='2011-01-01', datoTil=Sys.Date(), aar=0,
                         minald=0, maxald=110, erMann='', InnMaate='', dodInt='',
                         velgDiag=0, nivaa = 0, overfPas = 0, # grType=99,
                         enhetsUtvalg=0, reshID=0, velgAvd=0,
                         fargepalett='BlaaOff')
{
      #OffAlleFarger <- c('#c6dbef', '#6baed6', '#4292c6', '#2171b5', '#084594', '#000059', '#FF7260', '#4D4D4D', '#737373', '#A6A6A6', '#DADADA')
      #BlaaOff = OffAlleFarger[rev(c(1,2,4,5))]


      # Definer intersect-operator
      "%i%" <- intersect
      dodInt <- as.numeric(dodInt)

      #Velge hva som er eget sykehus
      if ((reshID!=0) & (length(velgAvd)==1) & (velgAvd != 0)) {
         reshID <- velgAvd}

      #Velge hvilke sykehus som skal være med:
      if (velgAvd[1] != 0 & reshID==0) {
         #if (enhetsUtvalg !=0) {stop("enhetsUtvalg må være 0 (alle)")}
         #Utvalg på avdelinger:
         RegData <- RegData[which(as.numeric(RegData$ReshId) %in% as.numeric(velgAvd)),]
         RegData$ShNavn <- as.factor(RegData$ShNavn)
      }

      #Enhetsutvalg:
      #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
      #trengs ikke data for hele landet:
      reshID <- as.numeric(reshID)
      indEgen1 <- match(reshID, RegData$ReshId)
      enhetsUtvalg <- ifelse(reshID==0 | is.na(indEgen1), 0, enhetsUtvalg )
      #grTypeEgen <- RegData$ShType[indEgen1]
      shNivaaEgen <- RegData$Nivaa[indEgen1] #
      shNivaaEgenNum <- RegData$NivaaNum[indEgen1] #
      if (enhetsUtvalg %in% c(2,3,4,6,7)) {
            RegData <- switch(as.character(enhetsUtvalg),
                              '2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
                              '3' = subset(RegData,Nivaa==shNivaaEgen),
                              '4' = RegData[which(RegData$Nivaa == shNivaaEgen),],	#kun egen shgruppe
                              '6' = RegData[which(RegData$RHF == as.character(RegData$RHF[indEgen1])),],	#mot eget RHF
                              '7' = RegData[which(RegData$RHF == as.character(RegData$RHF[indEgen1])),])	#kun egen RHF
      }

      Ninn <- dim(RegData)[1]
      # indGrType <- switch(grType, '1' = which(RegData$ShType %in% 1:2),
      #                             '2' = which(RegData$ShType %in% 1:2),
      #                             '3' = which(RegData$ShType == 3))
      # if (grType %in% 1:3) {RegData <- RegData[indGrType,]} #For utvalg ved visning av flere sykehus

      indAld <- if(minald>0 | maxald<110) {
            which(RegData$Alder >= minald & RegData$Alder <= maxald)} else {1:Ninn}
      indDatoFra <- if(datoFra!=0) {
            which(RegData$InnDato >= as.Date(datoFra, tz= 'UTC'))
            } else {1:Ninn}
      indDatoTil <- if(datoTil!=0) {
            which(RegData$InnDato <= as.Date(datoTil, tz= 'UTC'))
            } else {1:Ninn}
      indAar <- if (aar[1] != 0) {which(RegData$Aar %in% aar)} else {1:Ninn}
      indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {1:Ninn}
      indInnMaate <- if (InnMaate %in% c(0,6,8)) {which(RegData$InnMaate == InnMaate)
      } else {1:Ninn}
      indDod <- if (dodInt %in% 0:1) {
        which(as.numeric(RegData$DischargedIntensiveStatus)==dodInt) } else {1:Ninn}
      indDiag <- if (velgDiag %in% 1:2) {
        switch(velgDiag,
               '1' = which(RegData$Bekreftet %in% 0:1),
               '2' = which(!(RegData$Bekreftet %in% 0:1)))
      } else {1:Ninn}
      nivaaKort <- c('1a', '1b', '2b', '3', '3c')
      nivaaTxt <- c('Overvåk', 'Postop', 'Gen<50%','Gen>50%', 'Barn')
      indNivaa <- if (min(nivaa) > 0 ) {which(RegData$Nivaa %in% nivaaKort[nivaa])
      } else {1:Ninn}
      indOverf <- if (overfPas %in% 1:2) {which(RegData$Overf == overfPas)} else {1:Ninn}
      indMed <- indDatoFra %i% indDatoTil %i% indAld %i% indKj %i% indInnMaate %i% indDod %i% indDiag %i% indAar %i% indNivaa %i% indOverf

      RegData <- RegData[indMed,]

      N <- dim(RegData)[1]	#N=0 gir feilmelding
     # grTypetextstreng <- c('lokal-/sentral', 'lokal-/sentral', 'region')
      #if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}
      if (nivaa %in% 1:5) {shNivaaTxt <- shNivaaTxt[nivaa]} else {shNivaaTxt <- 'alle '}


      utvalgTxt <- c(
            if(datoFra!=0 | datoTil!=0) {paste0(
                  'Innleggelsesdatoer: ', if (N>0) {min(as.Date(RegData$InnDato), na.rm=T)} else {datoFra},
                  ' til ', if (N>0) {max(as.Date(RegData$InnDato), na.rm=T)} else {datoTil})} else {NULL},
            if (aar[1] > 0){paste0('Innleggelsesår: ', paste0(aar, collapse=', '))},
            if ((minald>0) | (maxald<110)) {
               paste0('Pasienter fra ', if (N>0) {round(min(RegData$Alder, na.rm=T))} else {round(minald)},
                      ' til ', if (N>0) {round(max(RegData$Alder, na.rm=T))} else {round(maxald)}, ' år')},
           if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
            if (velgDiag %in% 1:2) {paste0('Diagnose: ', c('Covid-19', 'Alle uten Covid-19')[velgDiag])},
            if (InnMaate %in% c(0,6,8)) {paste('Innmåte: ',
                                               c('Elektivt',0,0,0,0,0, 'Akutt medisinsk',0, 'Akutt kirurgi')[InnMaate+1], sep='')},
  #          if (grType %in% 1:3) {paste0('Sykehustype: ', grTypetextstreng[grType])},
            if (min(nivaa) > 0 ) {paste0('Nivå: ', paste(nivaaTxt[nivaa], collapse = ', '))},
            if (overfPas %in% 1:2) {c('Ikke-overførte pas.', 'Overførte pasienter')[overfPas]},
            if (dodInt %in% 0:1) {paste0('Status ut fra intensiv: ', c('Levende','Død')[as.numeric(dodInt)+1])},
            if (velgAvd[1] != 0 & reshID==0) {'Viser valgte sykehus'}
      )


      #Enhetsutvalg:
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
            hovedgrTxt <- as.character(RegData$ShNavn[indEgen1]) } else {
                  hovedgrTxt <- switch(as.character(enhetsUtvalg),
                                       '0' = 'Hele landet',
                                       '4' = paste0(nivaaTxt[shNivaaEgenNum], '-enheter'),
                                       '5' = paste0(nivaaTxt[shNivaaEgenNum], '-enheter'),
                                       '7' = as.character(RegData$RHF[indEgen1]),
                                       '8' = as.character(RegData$RHF[indEgen1]))
            }

      if ((velgAvd[1] != 0) & (reshID==0)) {hovedgrTxt <-'Valgte sykehus'}

      ind <- list(Hoved=0, Rest=0, ShNivaaEgen=0) # ShTypeEgen=0)
      smltxt <- ''
      if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
            medSml <- 0
            ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
            ind$Rest <- NULL
      } else {						#Skal gjøre sammenlikning
            medSml <- 1
            if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
                  ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
                        ind$Hoved <- switch(as.character(enhetsUtvalg),
                                            '5' = which(RegData$NivaaNum == shNivaaEgenNum),	#shgr
                                            '8' = which(RegData$RHF == RegData$RHF[indEgen1]))}	#RHF
            smltxt <- switch(as.character(enhetsUtvalg),
                             '1' = 'landet forøvrig',
                             '3' = paste0('andre ', nivaaTxt[shNivaaEgenNum], 'enhet'),	#RegData inneh. kun egen shgruppe
                             '5' = 'andre enhetsnivå',
                             '6' = paste0(RegData$RHF[indEgen1], ' forøvrig'),	#RegData inneh. kun egen RHF
                             '8' = 'andre RHF')
            ind$Rest <- switch(as.character(enhetsUtvalg),
                               '1' = which(as.numeric(RegData$ReshId) != reshID),
                               '3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
                               '5' = which(RegData$NivaaNum != shNivaaEgenNum),
                               '6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen RHF
                               '8' = which(RegData$RHF != RegData$RHF[indEgen1]))
      }
#      ind$ShTypeEgen =  which(RegData$ShType == RegData$ShType[indEgen1]) #Funker ikke hvis gjort utvalg på annen sykehustype
      ind$ShNivaaEgen =  which(RegData$NivaaNum == shNivaaEgenNum) #Funker ikke hvis gjort utvalg på annen sykehustype




      UtData <- list(utvalgTxt=utvalgTxt, fargepalett=fargepalett, ind=ind, medSml=medSml,
                     smltxt=smltxt, hovedgrTxt=hovedgrTxt, shNivaaTxt=shNivaaTxt, RegData=RegData) # grTypeTxt=shNivaaTxt,
      return(invisible(UtData))
}
