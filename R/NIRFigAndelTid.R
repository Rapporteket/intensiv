#' Tidstrend av andel opphold
#'
#' Denne funksjonen lager et linjediagram som viser utvikling over tid  for andeler av valgt variabel,
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år
#'     \item alder_over80: Pasienter over 80 år (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga.
#'     \item ExtendedHemodynamicMonitoring: Utvidet hemodyn. overvåkning
#'	 \item isolering: Isolasjon av pasient
#'	 \item liggetidDod: Liggetid, døde
#'	 \item nyreBeh: Nyrebehandling
#'     \item reinn: Andel reinnlagte (fjerner ukjente) Kvalitetsindikator
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item respiratortidInvMoverf: Respiratortid, inv. < 2,5d m/overføringer
#'     \item respiratortidInvUoverf: Respiratortid, inv. < 2,5d u/overføringer
#'     \item respiratortidDod: Respiratortid, døde
#'     \item Trakeostomi: Andel som har fått trakeostomi (kat 2 og 3)
#'     \item trakAapen: Trakeostomi, åpen
#'    }
#'
#' @inheritParams NIRFigAndeler
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år.
#'
#' @export
NIRFigAndelTid <- function(RegData, valgtVar='alder_u18', datoFra='2011-01-01', datoTil=Sys.Date(), tidsenhet='Aar',
                        minald=0, maxald=110, erMann='', InnMaate='', dodInt='', velgDiag=0,
                        reshID=0, outfile='',
                        enhetsUtvalg=0, preprosess=1, hentData=0, lagFig=1, ...) {

   # if ("session" %in% names(list(...))) {
   #    rapbase::repLogger(session = list(...)[["session"]], msg = paste0("AndelTid: ", valgtVar))
   # }
   if (hentData == 1) {
            RegData <- NIRRegDataSQL(datoFra, datoTil)
      }

      # Preprosessering av data. I samledokument gjøre dette i samledokumentet. Off01-data er preprosessert.
      if (preprosess==1){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }


      #------- Tilrettelegge variable
      varTxt <- ''
        if (valgtVar %in% c('beredMpand_opph', 'beredMpand_pers')) {
          NIRVarSpes <- intensivberedskap::NIRberedskVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
          } else {
            NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
            }
            RegData <- NIRVarSpes$RegData
            sortAvtagende <- NIRVarSpes$sortAvtagende
            varTxt <- NIRVarSpes$varTxt
            KImaal <- NIRVarSpes$KImaal
            KImaaltxt <- ifelse(NIRVarSpes$KImaaltxt=='', '', paste0('Mål: ',NIRVarSpes$KImaaltxt))
            tittel <- NIRVarSpes$tittel

      #------- Gjøre utvalg
      smltxt <- ''
      medSml <- 0

            if (reshID==0) {enhetsUtvalg <- 0}
            NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                                      minald=minald, maxald=maxald, erMann=erMann, velgDiag=velgDiag, #aar=0,
                                      InnMaate=InnMaate, dodInt=dodInt, enhetsUtvalg=enhetsUtvalg)
            medSml <- NIRUtvalg$medSml
            utvalgTxt <- NIRUtvalg$utvalgTxt
            ind <- NIRUtvalg$ind

      RegData <- NIRUtvalg$RegData
      Ngrense <-ifelse(valgtVar %in% c('OrganDonationCompletedStatus', 'OrganDonationCompletedCirc'),
                       0,10)
      #------------------------Klargjøre tidsenhet--------------
      N <- list(Hoved = dim(RegData)[1], Rest=0)
      if (N$Hoved>Ngrense) {
        if (class(RegData$Aar) == "factor") {
          RegData$Aar <- as.numeric(levels(RegData$Aar)[RegData$Aar])}
            RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
            RegData <- RegDataFunk$RegData
            #tidtxt <- RegDataFunk$tidtxt

            #--------------- Gjøre beregninger ------------------------------

            AggVerdier <- list(Hoved = 0, Rest =0)
            Ngr <- list(Hoved = 0, Rest =0)
			N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))


            NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per tidsenhet
            Ngr$Hoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per tidsenhet
            AggVerdier$Hoved <- Ngr$Hoved/NAarHoved*100
            NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
            Ngr$Rest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
            AggVerdier$Rest <- Ngr$Rest/NAarRest*100
            #Ngr <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)

            if (valgtVar %in% c('liggetidDod','respiratortidDod')) {
                  #Kommentar: for liggetid og respiratortid vises antall pasienter og ikke antall liggedøgn for døde
                  Ngr$Hoved<-tapply(RegData[ind$Hoved, 'DischargedIntensiveStatus'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T)    #         liggetid i døgn, navnene blir litt villedende men enklest å gjøre dette på denne måten
                  Ngr$Rest<- tapply(RegData$DischargedIntensiveStatus[ind$Rest], RegData$TidsEnhet[ind$Rest], sum)
                  SUMAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], sum,na.rm=T)
                  SUMAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel2'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T)
                  AggVerdier$Hoved <- SUMAarHendHoved/SUMAarHoved*100
                  SUMAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], sum,na.rm=T)
                  SUMAarHendRest <- tapply(RegData$Variabel2[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
                  AggVerdier$Rest <- SUMAarHendRest/SUMAarRest*100
            }

            grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
            yAkseTxt <- 'Andel (%)'
            vektor <- c('Aar','Halvaar','Kvartal','Mnd')
            xAkseTxt <- c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal',
                          'Innleggelsesmåned')[which(tidsenhet==vektor)]

            FigDataParam <- list(AggVerdier=AggVerdier, N=N,
                                 Ngr=Ngr,
                                 KImaal <- KImaal,
                                 KImaaltxt <- KImaaltxt,
                                 grtxt=levels(RegData$TidsEnhet),
                                 grtxt2=grtxt2,
                                 varTxt=varTxt,
                                 tittel=tittel,
                                 retn='V',
                                 xAkseTxt=xAkseTxt,
                                 yAkseTxt=yAkseTxt,
                                 utvalgTxt=NIRUtvalg$utvalgTxt,
                                 fargepalett=NIRUtvalg$fargepalett,
                                 medSml=medSml,
                                 hovedgrTxt=NIRUtvalg$hovedgrTxt,
                                 smltxt=NIRUtvalg$smltxt)

      }
      FigAndelTid <- function(RegData, AggVerdier, AggTot=0, Ngr, tittel='mangler tittel', smltxt='', N, retn='H',
                              yAkseTxt='', utvalgTxt='', shNivaaTxt='', varTxt='', grtxt2='', hovedgrTxt='', #tidtxt,
                              valgtMaal='Andel', cexgr=1, medSml=0, fargepalett='BlaaOff', xAkseTxt='',
                              medKI=0, KImaal = NA, KImaaltxt = '', outfile='') { #Ngr=list(Hoved=0), grVar='',

            #-----------Figur---------------------------------------
            #Hvis for f? observasjoner..
            if (N$Hoved < Ngrense | (medSml ==1 & N$Rest < Ngrense)) {
                  FigTypUt <- rapFigurer::figtype(outfile)
                  farger <- FigTypUt$farger
                  plot.new()
                  title(main=paste0('variabel: ', valgtVar))	#, line=-6)
                  legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
                  text(0.5, 0.65, 'F?rre enn 10 registreringer i hoved-', cex=1.2)
                  text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
                  if ( outfile != '') {dev.off()}

            } else {

                  #Plottspesifikke parametre:
                  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=fargepalett)
                  farger <- FigTypUt$farger
                  fargeHoved <- farger[3]
                  fargeRest <- farger[1]
                  NutvTxt <- length(utvalgTxt)
                  hmarg <- 0.04+0.01*NutvTxt
                  par('fig' = c(0,1,0,1-hmarg))
                  cexleg <- 1	#St?rrelse p? legendtekst
                  ylabtext <- "Andel (%)"
                  xskala <- 1:length(levels(RegData$TidsEnhet)) #length(tidtxt)
                  xmax <- max(xskala)


                  ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
                  plot(xskala, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
                       xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,
                       cex=2, xlab=xAkseTxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

                  #Legge på linjer i plottet.
                  grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

                  axis(side=1, at = xskala, labels = levels(RegData$TidsEnhet)) #tidtxt)

                  title(tittel, line=1, font.main=1)


                  lines(xskala, AggVerdier$Hoved, col=fargeHoved, lwd=3)
                  points(xskala, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
                  text(xskala, AggVerdier$Hoved, pos=3, Ngr$Hoved, cex=0.9, col=fargeHoved)

                  lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
                  points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)

                  #KImål
                  lines(xskala,rep(KImaal,length(xskala)), col= '#FF7260', lwd=3)
                  mtext(text=KImaaltxt, at=KImaal, side=4, las=0, adj=0.5,  cex=0.9, col='#FF7260')
                  #text(max(xskala), KImaal, pos=4, paste0('Mål:',KImaaltxt), cex=0.9, col='#FF7260')

                  Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
                  if (medSml == 1) {
                        text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
                        legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                                       paste0(smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1,
                               col=c(fargeHoved, fargeRest, NA), lwd=3, cex=cexleg)
                  } else {
                        legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt),
                               col=c(fargeHoved, NA), lwd=3, bty='n')
                  }

                  #Tekst som angir hvilket utvalg som er gjort
                  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

                  par('fig'=c(0, 1, 0, 1))
                  if ( outfile != '') {dev.off()}
                  #------------------------------------------------------------------------------

            }	#end else statement
      }

      if (lagFig == 1) {
            FigAndelTid(RegData, AggVerdier, Ngr, tittel=tittel, hovedgrTxt=NIRUtvalg$hovedgrTxt,
                        smltxt=NIRUtvalg$smltxt, Ngr = Ngr, KImaal = KImaal, KImaaltxt=KImaaltxt, N=N, retn='V',
                        utvalgTxt=utvalgTxt, varTxt=varTxt, grtxt2=grtxt2, medSml=medSml,
                        xAkseTxt=xAkseTxt, yAkseTxt=yAkseTxt,
                        outfile=outfile)
      }

      return(invisible(FigDataParam))
}	#end function



