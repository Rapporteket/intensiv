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
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Pasientens alders
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'    }
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med fordeling før og ved intervensjon
#'
#' @export

NIRFigPrePostPaaror  <- function(RegData=0, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', aar=0,
                                 startDatoIntervensjon='2016-10-01', sluttDatoIntervensjon=Sys.Date(),
                                 minald=0, maxald=110, erMann='',InnMaate='', dodInt='',outfile='', grType=99,
                                 preprosess=1, hentData=0, reshID=0, enhetsUtvalg=0, lagFig=1,...){ #overfPas=0,
   if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]], msg = paste0("FigPrePostPaaror: ", valgtVar))
   }
      if (hentData == 1) {
            RegData <- NIRRegDataSQL(datoFra, datoTil) #minald=0, maxald=110, erMann='',InnMaate='', dodInt=''
      }

      if (preprosess){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }


      #     "%i%" <- intersect
      #--------------- Definere variable ------------------------------

      NIRVarSpes <- NIRVarTilretteleggPaaror(RegData=RegData, valgtVar=valgtVar, figurtype='andeler')
      RegData <- NIRVarSpes$RegData
      flerevar <- NIRVarSpes$flerevar

      datoFra <- max(as.Date(datoFra), as.Date('2015-01-01'))
      NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar,
                                minald=minald, maxald=maxald,
                                erMann=erMann, InnMaate=InnMaate, dodInt=dodInt,
                                reshID=reshID, grType=grType, enhetsUtvalg=enhetsUtvalg) #overfPas = overfPas,
      RegData <- NIRUtvalg$RegData
      utvalgTxt <- NIRUtvalg$utvalgTxt
      RegData <- leggTilIntervensjon(RegData, startDatoIntervensjon=startDatoIntervensjon,
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
      #Gjennomsnittsskårer
      varSkaar <- ifelse(grepl('Sum',valgtVar), valgtVar, paste0(valgtVar,'Skaaring'))
      TotSkaar$Pre <- sprintf('%.1f', mean(RegData[ind$Pre, varSkaar], na.rm = T))
      TotSkaar$Post <- sprintf('%.1f', mean(RegData[ind$Post, varSkaar], na.rm = T))



      #grtxt <- paste0(rev(NIRVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Pre)), '%)')
      grtxt <- NIRVarSpes$grtxt
      grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Pre),' / ', sprintf('%.1f',AggVerdier$Post),'%')
      grtxt2[match('', grtxt)] <- ''
      tittel <- NIRVarSpes$tittel


     # AndelerPP <- list(Pre=0, Post=0)
      NPre <- N$Pre
      NPost <- N$Post
      AndelerPP <- cbind(AggVerdier$Pre, AggVerdier$Post)

      #-----------Figur---------------------------------------

      # delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
      #       {sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
      #               USE.NAMES = FALSE)
      #       }

      #Plottspesifikke parametre:
      FigTypUt <- rapFigurer::figtype(outfile, fargepalett='BlaaOff')
      NutvTxt <- length(utvalgTxt)
      vmarg <- 0 #switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

      farger <- FigTypUt$farger
      antGr <- length(grtxt)
      #Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
      lwdPost <- 3	#tykkelse på linja som repr. landet
      cexleg <- 0.9	#Størrelse på legendtekst
      cexpt <- 2	#Størrelse på punkter (resten av landet)

      #Vertikale søyler eller linje
      ymax <- min(max(AndelerPP,na.rm=T)*1.25, 110)
      pos <- barplot(t(AndelerPP), beside=TRUE, las=1, ylab="Andel pårørende (%)",
                     cex.names=0.8, col=farger[c(3,1)], names.arg=rep('', length(grtxt)), border='white', ylim=c(0, ymax))	# names.arg=grtxt,
      # pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab=yAkseTxt,
      #                sub=xAkseTxt,	col=fargeHoved, border='white', ylim=c(0, ymax))
      mtext(at=pos[1,], grtxt2, side=1, las=1, cex=0.75, adj=0.2, line=0)
      grtxt <- delTekst(grtxt, 13)
      mtext(at=pos[1,], grtxt, side=1, las=1, cex=0.75, adj=0.2, line=2)
      legend('top', c(paste0('Før intervensjon, N=', NPre), paste0('Gj.sn. skår: ',TotSkaar$Pre),
                      paste0('Etter intervensjon, N=', NPost), paste0('Gj.sn. skår: ',TotSkaar$Post)),
             bty='n', fill=farger[c(3,NA,1,NA)], border=NA, ncol=2, cex=cexleg)

      #Tekst som angir hvilket utvalg som er gjort
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      title(tittel, font.main=1, sub=NIRUtvalg$hovedgrTxt, col.sub= farger[2])	#line=0.5,

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}
}

