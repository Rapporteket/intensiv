#' Funksjon for å tilrettelegge variable for beregning.
#'
#'
#' @inheritParams NIRFigAndeler
#' @param prePost Før (1) eller etter (2) intervensjon
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'
NIRFigGjsnPaaror  <- function(RegData, valgtVar, valgtMaal='Gjsn', prePost=2, grVar='ShNavn',outfile='',...)
{

   # if ("session" %in% names(list(...))) {
   #   rapbase::repLogger(session = list(...)[["session"]], msg = paste0("GjsnPaaror: ", valgtVar))
   # }

      #RegData <- NIRPreprosess(RegData=RegData) #Gjort tidl
      RegData <- RegData[RegData$PrePost==prePost,]
      #-----------------Tilrettelegge variable--------------------------
      #            !!!!!!!!!!! PRIORITER TOTALSKÅRER!!!

      NIRVarSpes <- NIRVarTilretteleggPaaror(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
      RegData <- NIRVarSpes$RegData
      #xAkseTxt <- 'Gjennomsnittlig skår'
      sortAvtagende <- TRUE      #Rekkefølge
      #grtxt <- NIRVarSpes$grtxt


      #RegData$VariabelGr <- RegData[ ,valgtVar]
      #RegData$VariabelGr[RegData$VariabelGr==-1] <- 9
      #RegData$VariabelGr[RegData$VariabelGr==6] <- 9
      #RegData$VariabelGr[RegData$VariabelGr %in% c(-1,6)] <- 9

      #RegData$VariabelGr <- factor(RegData$VariabelGr, levels=c(1:(length(grtxt)-2),8:9))



      #----------------------Beregninger---------------------

      medKI=1
      Ngrense <- 10
      '%i%' <- intersect
      RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
      #Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
      #grupper komme med uansett om de ikke har registreringer.

      if(dim(RegData)[1]>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

      ppTxt <- c('før', 'etter')
      xAkseTxt <- switch(valgtMaal,
                         Med = paste('Median skår, ', ppTxt[prePost], ' intervensjon'),
                         Gjsn = paste('Gjennomsnittlig skår, ', ppTxt[prePost], ' intervensjon')
      )

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
            Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
            SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
            MidtHele <- mean(RegData$Variabel, na.rm=T)	#mean(RegData$Variabel)
            KIHele <- MidtHele + sd(RegData$Variabel, na.rm=T)/sqrt(N)*c(-2,2)
            Gjsn[indGrUt] <- dummy0
            SE[indGrUt] <- 0
            sortInd <- order(Gjsn, decreasing=sortAvtagende, na.last = FALSE)
            Midt <- Gjsn[sortInd] #as.numeric(Gjsn[sortInd])
            KIned <- Gjsn[sortInd] - 2*SE[sortInd]
            KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
      }


      GrNavnSort <- paste0(names(Ngr)[sortInd], Ngrtxt[sortInd])
      soyletxt <- sprintf(paste0('%.', 1,'f'), Midt)
      #soyletxt <- c(sprintf(paste0('%.', AntDes,'f'), Midt[1:AntGr]), rep('',length(Ngr)-AntGr))
      indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
      soyletxt[indUT] <- ''
      KIned[indUT] <- NA
      KIopp[indUT] <- NA

      AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
      Ngr <- list(Hoved=Ngr[sortInd], Rest=0)
      AggTot <- MidtHele
      N <- list(Hoved=N)
      grtxt <- GrNavnSort
      cexgr <- 1

      #Plottspesifikke parametre:
      #Høyde må avhenge av antall grupper
      hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
      FigTypUt <- rapFigurer::figtype(outfile, height=hoyde, fargepalett='BlaaOff')
      #Tilpasse marger for å kunne skrive utvalgsteksten
      #NutvTxt <- 1 #length(utvalgTxt)
      vmarg <- min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75))
      #NB: strwidth oppfører seg ulikt avh. av device...
      par('fig'=c(vmarg, 1, 0, 1 )) #1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med


      farger <- FigTypUt$farger
      fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
      fargeSml <- farger[3]
      antGr <- length(grtxt)
      #lwdRest <- 3	#tykkelse på linja som repr. landet
      cexleg <- 0.9	#Størrelse på legendtekst


      #Definerer disse i beregningsfunksjonen?
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Sml),na.rm=T)*1.2
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


      shNivaaTxt <- 'alle ' #smltxt
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
      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
              col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
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
            TXT <- paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved)
            legend(xmax/4, posOver+posDiff, TXT, fill=NA,  border=NA, lwd=2.5,xpd=TRUE,
                   col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
      } else {
            TXT <- c(paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved),
                     paste0('95% konf.int., ', shNivaaTxt, 'sykehus (',
                            sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'))
            legend(xmax/4, posOver, TXT, yjust=0, xpd=TRUE, fill=c(NA, farger[3]),  border=NA, lwd=2.5,  #inset=c(-0.1,0),
                   col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n') #+2*posDiff
      }

      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)


      title(NIRVarSpes$tittel, line=1.5) #cex.main=1.3)

      #Tekst som angir hvilket utvalg som er gjort
      avst <- 0.8
      utvpos <- 3	#Startlinje for teksten
      #mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}

} #Funksjon
