#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NIRFigAndeler
#' @inheritParams NIRFigAndelerGrVar
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Pasientens alders
#'     \item liggetid: Liggetid
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score). Per døgn.
#'     \item NEMS: Skår for ressursbruk per opphold. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item NEMS24: NEMS-skår per døgn.
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item respiratortidNonInv: Respiratortid, ikke-invasiv
#'     \item respiratortidInvMoverf: Respiratortid, invasiv m/overf.
#'     \item respiratortidInvUoverf: Respiratortid, invasiv u/overf.
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'     \item SMR: (SAPSII-estimert dødelighet) Standardisert mortalitetsratio (Gir annen figurtype)
#'     \item PIMdod: PIM-estimert dødelighet (Gir annen figurtype)
#'    }
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NIRFigGjsnGrVar <- function(RegData, valgtVar, preprosess=1, hentData=0, valgtMaal='Gjsn',
                            minald=0, maxald=110, datoFra='2011-01-01', datoTil='3000-01-01', aar=0,
                            nivaa = 0, #grType=99,
                            InnMaate=99, dodInt='', erMann='', grVar='ShNavn', medKI=1,
                            overfPas=99, velgDiag=0, lagFig=1, outfile='',...) {

  # if ("session" %in% names(list(...))) {
  #   rapbase::repLogger(session = list(...)[["session"]], msg = paste0("AndelGrVar: ", valgtVar))
  # }
  if (hentData == 1) {
    RegData <- NIRRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess){
    RegData <- NIRPreprosess(RegData=RegData) #, reshID=reshID)
  }

  #------- Tilrettelegge variable
  NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
  KImaal <- NIRVarSpes$KImaal
  KImaaltxt <- NIRVarSpes$KImaaltxt
  RegData <- NIRVarSpes$RegData

  #------- Gjøre utvalg
  minald <- max(NIRVarSpes$minald, minald)
  maxald <- min(NIRVarSpes$maxald, maxald)
  NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar,
                            minald=minald, maxald=maxald, erMann=erMann, InnMaate=InnMaate,
                            overfPas = overfPas, dodInt=dodInt, nivaa=nivaa, velgDiag=velgDiag)
  RegData <- NIRUtvalg$RegData
  utvalgTxt <- NIRUtvalg$utvalgTxt

  Ngrense <- 10
  '%i%' <- intersect

  RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
  #Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
  #grupper komme med uansett om de ikke har registreringer.

  if(dim(RegData)[1]>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

  if (valgtVar %in% c('PIMdod', 'SMR')) {valgtMaal <- 'Gjsn'}
  t1 <- switch(valgtMaal,
               Med = 'Median ',
               Gjsn = 'Gjennomsnittlig ')


  tittel <- paste0(t1, NIRVarSpes$tittel)

  if( valgtVar =='SMR') {tittel <- c(paste0('SMR, ', NIRUtvalg$shNivaaTxt, '-enheter'),
                                     '(uten reinnlagte pasienter)')}
  if( valgtVar =='PIMdod') {tittel <- paste0('PIM, ', NIRUtvalg$shNivaaTxt, '-enheter')}

  Ngrtxt <- paste0(' (', as.character(Ngr),')')
  indGrUt <- which(Ngr < Ngrense)
  if (length(indGrUt)==0) { indGrUt <- 0}
  Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')
  N <- dim(RegData)[1]

  KIHele <- c(0,0)
  KIned <- c(0,0)
  KIhele <- c(0,0)


  dummy0 <- NA #-0.0001
  #Kommer ut ferdig sortert!
  if (valgtMaal=='Med') {
    MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
    MedIQR$stats[ ,indGrUt] <- dummy0
    MedIQR$conf[ ,indGrUt] <- dummy0
    sortInd <- order( MedIQR$stats[3,], decreasing=NIRVarSpes$sortAvtagende, na.last = FALSE)
    Midt <- as.numeric(MedIQR$stats[3, sortInd])
    KIned <- MedIQR$conf[1, sortInd]
    KIopp <- MedIQR$conf[2, sortInd]
    MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
    MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
    KIHele <- MedIQRHele$conf
    #Hvis vil bruke vanlige konf.int:
    #j <- ceiling(N/2 - 1.96*sqrt(N/4))
    #k <- ceiling(N/2 + 1.96*sqrt(N/4))
    #KIHele <- sort(RegData$Variabel)[c(j,k)]
    #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
    #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
    #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
    #roughly a 95% confidence interval for the difference in two medians.
  }

  if (valgtMaal=='Gjsn') {	#Gjennomsnitt er standard, men må velges.
    if (valgtVar %in% c('PIMdod', 'SMR')) { #Bør tas ut av Gjsn...?
      medKI <- 0
      ObsGr <- tapply(RegData$Dod30, RegData[ ,grVar], mean, na.rm=T)
      #EstGr <- tapply(RegData$SMR, RegData[ ,grVar], mean, na.rm=T)
      EstGr <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
      ind0 <- which(EstGr == 0)
      Gjsn <- 100*ObsGr/EstGr
      if (length(ind0)>0) {Gjsn[ind0] <- 0}#Unngå å dele på 0
      #Vi benytter ikke konf.int for SMR. Setter alle SE lik 0
      #TestPoGr <- which((Ngr*ObsGr-3*sqrt(Ngr*ObsGr*(1-ObsGr)) <= 0) | (Ngr*ObsGr+3*sqrt(Ngr*ObsGr*(1-ObsGr)) > Ngr))
      #SE <- sqrt(ObsGr*(1-ObsGr))*100/(sqrt(Ngr)*EstGr)
      #if (length(TestPoGr)>0) {SE[TestPoGr] <- 0}
      SE <- rep(0, length(Ngr))
      Obs <-  mean(RegData$Dod30)	#Kun 0 og 1
      Est <- mean(RegData$Variabel, na.rm=T)
      MidtHele <- ifelse(Est ==0, 0, 100*Obs/Est)
    } else {
      Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
      SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
      MidtHele <- mean(RegData$Variabel)	#mean(RegData$Variabel)
      KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
    }

    Gjsn[indGrUt] <- dummy0
    SE[indGrUt] <- 0
    sortInd <- order(Gjsn, decreasing=NIRVarSpes$sortAvtagende, na.last = FALSE)
    Midt <- Gjsn[sortInd] #as.numeric(Gjsn[sortInd])
    KIned <- Gjsn[sortInd] - 2*SE[sortInd]
    KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
  }


  GrNavnSort <- paste0(names(Ngr)[sortInd], Ngrtxt[sortInd])
  if (valgtVar %in% c('SMR', 'PIMdod')) {AntDes <- 2} else {AntDes <- 1}
  soyletxt <- sprintf(paste0('%.', AntDes,'f'), Midt)
  indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
  soyletxt[indUT] <- ''
  KIned[indUT] <- NA
  KIopp[indUT] <- NA

  AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
  Ngr <- list(Hoved=Ngr[sortInd], Rest=0)
  AggTot <- MidtHele
  KImaal <- NIRVarSpes$KImaal
  KImaaltxt <- NIRVarSpes$KImaaltxt
  xAkseTxt <- NIRVarSpes$xAkseTxt
  shNivaaTxt <- NIRUtvalg$shNivaaTxt
  utvalgTxt <- NIRUtvalg$utvalgTxt
  fargepalett <- NIRUtvalg$fargepalett
  smltxt <- NIRUtvalg$smltxt
  N <- list(Hoved=N)
  grtxt <- GrNavnSort


  #Se NIRFigSoyler for forklaring av innhold i lista GjsnGrVarData
  GjsnGrVarData <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                        N=list(Hoved=N),
                        Ngr=Ngr,
                        grtxt2='',
                        medKI=medKI,
                        soyletxt=soyletxt,
                        valgtMaal=valgtMaal,
                        tittel=tittel,    #NIRVarSpes$tittel,
                        retn='H',
                        utvalgTxt = utvalgTxt,
                        medSml=NIRUtvalg$medSml)


  #FigDataParam skal inn som enkeltparametre i funksjonskallet
  if (lagFig == 1) {
    cexgr <- 1-length(soyletxt)/200

    if ((N$Hoved < 5) | (dim(RegData)[1]<5)) {
      #-----------Figur---------------------------------------
      FigTypUt <-rapFigurer::figtype(outfile)  #FigTypUt <- rapFigurer::figtype(outfile)
      farger <- FigTypUt$farger
      plot.new()
      title(tittel)	#, line=-6)
      legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
      if (valgtMaal=='Med' & valgtVar %in% c('PIMdod', 'SMR')) {tekst <- 'Ugyldig parameterkombinasjon'   #valgtVar=='SMR'
      } else {tekst <- 'For få registreringer i egen eller sammenligningsgruppe'}
      text(0.5, 0.6, tekst, cex=1.2)
      if ( outfile != '') {dev.off()}

    } else {


      #Plottspesifikke parametre:
      #Høyde må avhenge av antall grupper
      hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
      FigTypUt <- rapFigurer::figtype(outfile, height=hoyde, fargepalett=fargepalett)
      #Tilpasse marger for å kunne skrive utvalgsteksten
      NutvTxt <- length(utvalgTxt)
      vmarg <- min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75))
      #NB: strwidth oppfører seg ulikt avh. av device...
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med


      farger <- FigTypUt$farger
      fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
      fargeRest <- farger[3]
      graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
      antGr <- length(grtxt)
      lwdRest <- 3	#tykkelse på linja som repr. landet
      cexleg <- 0.9	#Størrelse på legendtekst


      #Definerer disse i beregningsfunksjonen?
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- ifelse(valgtMaal=='Andel', min(xmax, 100), xmax) 	#100 som maks bare hvis andelsfigur..
      ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
      ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved)

      #Må def. pos først for å få strek for hele gruppa bak søylene
      ### reverserer for å slippe å gjøre det på konf.int
      pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
                         xlab=xAkseTxt, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
      indOK <- which(AggVerdier$Hoved>=0)
      posOK <- pos[indOK]
      posOver <- max(pos)+0.35*log(max(pos))
      posDiff <- 1.2*(pos[1]-pos[2])
      posOK <- pos[indOK]
      minpos <- min(posOK)-0.7
      maxpos <- max(posOK)+0.7


      shNivaaTxt <- ifelse(nivaa %in% 1:5, NIRUtvalg$shNivaaTxt, 'alle ')
      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
      #Linje for hele landet/utvalget:
      if (medKI == 1) {
        #Legge på konf.int for hele populasjonen
        #options(warn=-1)	#Unngå melding om KI med lengde 0
        KIHele <- AggVerdier$KIHele
        AntGr <- length(which(AggVerdier$Hoved>0))
        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), col=farger[3], border=farger[3],
                c(minpos, maxpos, maxpos, minpos))
      }
      lines(x=rep(AggTot, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55),
      #Linje for kvalitetsindikatormål:
      if ((valgtMaal=='Med' & valgtVar %in% c('respiratortidInvMoverf', 'respiratortidInvUoverf')) |
          valgtVar == 'SMR') {
        lines(x=rep(KImaal, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55),
        text(x=KImaal, y=maxpos+0.6, paste0('Mål:', KImaaltxt), cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0))
      }
      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
              col=fargeHoved, border=NA, cex.names=cexgr)
      soyleXpos <- 1.15*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
      text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus
      if (medKI == 1) {
        #Legge på konf.int for hver enkelt gruppe/sykehus
        arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIopp, y1=pos,
               length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
        arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIned, y1=pos,
               length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
      }


      #------Tegnforklaring (legend)--------
      if (medKI == 0) {
        TXT <- paste0(shNivaaTxt, 'enheter ', sprintf('%.1f', AggTot), ', N=', N$Hoved)
        legend('top', TXT, fill=NA,  border=NA, lwd=2.5,xpd=TRUE, #xmax/4, posOver+3*posDiff
               col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
      } else {
        TXT <- c(paste0(shNivaaTxt, 'enheter ', sprintf('%.1f', AggTot), ', N=', N$Hoved),
                 paste0('95% konf.int., ', shNivaaTxt, 'enheter (',
                        sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'))
        legend(x=xmax/4, y=posOver, TXT, yjust=0, xpd=TRUE, fill=c(NA, farger[3]),  border=NA, lwd=2.5,
               col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n') #+2*posDiff
      }

      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)


      title(tittel, line=1.5) #cex.main=1.3)

      #Tekst som angir hvilket utvalg som er gjort
      avst <- 0.8
      utvpos <- 3	#Startlinje for teksten
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}

    }  #Figur
  } #Funksjon

  return(invisible(GjsnGrVarData))

}



