#' Søylediagram med fordeling før og ved intervensjon
#'
#' Funksjon som genererer en fordelingsfigur for to tidsperioder som skal sammenlignes
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NIRFigAndeler
#' @inheritParams NIRFigAndelerGrVar
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt
#'
#' Nytt skjema tatt i bruk 7.nov 2023
#' Detajer: Her bør man liste opp hvilke variabler funksjonen benytter.
#'
#' @return Søylediagram med fordeling før og ved intervensjon
#'
#' @export

NIRFigPrePostPaaror  <- function(
    RegData=0, valgtVar, preprosess=1, hentData=0, reshID=0, enhetsUtvalg=0,
    datoFra='2023-11-07', datoTil=Sys.Date(), aar=0,
    startDatoIntervensjon='2025-01-01', sluttDatoIntervensjon=Sys.Date(),
    minald=0, maxald=110, erMann='',InnMaate='', dodInt='',nivaa = 0,
    outfile='', lagFig=1,...){ #overfPas=0,

  # if ("session" %in% names(list(...))) {
   #    rapbase::repLogger(session = list(...)[["session"]], msg = paste0("FigPrePostPaaror: ", valgtVar))
   # }
      if (hentData == 1) {
            RegData <- NIRRegDataSQL(datoFra, datoTil) #minald=0, maxald=110, erMann='',InnMaate='', dodInt=''
      }

      if (preprosess){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }

      #--------------- Definere variable ------------------------------

      NIRVarSpes <- NIRVarTilretteleggPaaror(RegData=RegData, valgtVar=valgtVar, figurtype='andeler')
      RegData <- NIRVarSpes$RegData
      flerevar <- NIRVarSpes$flerevar

      datoFra <- max(as.Date(datoFra), as.Date('2023-11-07'))
      NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar,
                                minald=minald, maxald=maxald,
                                erMann=erMann, nivaa=nivaa, dodInt=dodInt,
                                reshID=reshID, enhetsUtvalg=enhetsUtvalg) #overfPas = overfPas,
      RegData <- NIRUtvalg$RegData
      utvalgTxt <- NIRUtvalg$utvalgTxt
      RegData <- leggTilIntervensjon(RegData,
                                     startDatoIntervensjon = startDatoIntervensjon,
                                     sluttDatoIntervensjon=sluttDatoIntervensjon)
      utvalgTxt <- c(utvalgTxt, paste0('Startdato, intervensjon: ', startDatoIntervensjon))

      #Skal sammenlikne før og etter intervensjon. Definert i variabelen PrePost.
      AggVerdier <- list(Pre = 0, Post =0)
      N <- list(Pre = 0, Post =0)
      Ngr <- list(Pre = 0, Post =0)
      TotSkaar <- list(Pre = 0, Post =0)
      ind <- list(Pre = which(RegData$Intervensjon==0), Post = which(RegData$Intervensjon==1))

      Ngr$Pre <- table(RegData$VariabelGr[ind$Pre])
      N$Pre <- sum(Ngr$Pre)	#length(ind$Pre)- Kan inneholde NA
      AggVerdier$Pre <- 100*Ngr$Pre/N$Pre

      Ngr$Post <- table(RegData$VariabelGr[ind$Post])
      N$Post <- sum(Ngr$Post)
      AggVerdier$Post <- 100*Ngr$Post/N$Post

      grtxt <- NIRVarSpes$grtxt
      grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Pre),' / ', sprintf('%.1f',AggVerdier$Post),'%')
      grtxt2[match('', grtxt)] <- ''
      tittel <- NIRVarSpes$tittel


     # AndelerPP <- list(Pre=0, Post=0)
      NPre <- N$Pre
      NPost <- N$Post
      AndelerPP <- cbind(AggVerdier$Pre, AggVerdier$Post)

      #-----------Figur---------------------------------------

      #Plottspesifikke parametre:
      FigTypUt <- rapFigurer::figtype(outfile, fargepalett='BlaaOff')
      NutvTxt <- length(utvalgTxt)
      vmarg <- 0
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

      farger <- FigTypUt$farger
      antGr <- length(grtxt)
      lwdPost <- 3	#tykkelse på linja som repr. landet
      cexleg <- 1	#Størrelse på legendtekst
      cexpt <- 2	#Størrelse på punkter (resten av landet)

      #Vertikale søyler eller linje
      ymax <- min(max(AndelerPP,na.rm=T)*1.25, 110)
      pos <- barplot(t(AndelerPP), beside=TRUE, las=1, ylab="Andel pårørende (%)",
                     cex.names=1, col=farger[c(3,1)],
                     names.arg=rep('', length(grtxt)), border='white', ylim=c(0, ymax))
     mtext(at=pos[1,], grtxt2, side=1, las=1, cex=1, adj=0.2, line=0)
      grtxt <- delTekst(grtxt, 15)
      mtext(at=pos[1,], grtxt, side=1, las=1, cex=1, adj=0.2, line=2)
      legend('top', c(paste0('Før intervensjon, N=', NPre),
                      paste0('Etter intervensjon, N=', NPost)
                      ),
             bty='n', fill=farger[c(3,1)], border=NA, ncol=2, cex=cexleg)

      #Tekst som angir hvilket utvalg som er gjort
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      title(tittel, font.main=1.1, sub=NIRUtvalg$hovedgrTxt, col.sub= farger[2])

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}
}

