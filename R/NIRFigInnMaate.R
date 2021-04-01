#' Funksjon som genererer en figur med fordeling av innkomsttype
#for hvert sykehus. Funksjon som genererer en figur med AggVerdier av en gitt variabel for ei valgt gruppering,
#' f.eks. enheter. I øyeblikket benytter funksjonen bare 'ShNavn' som grupperingsvariabel, men
#' andre valg kan lett inkluderes. Funksjonen er tilrettelagt for også å kunne benytte "01-data",
#' dvs. kodede, anonymiserte data som benyttes til offentlige kvalitetsindikatorer.
#'
#' Funksjonen benytter funksjonene: NIRRegDataSQL, NIRPreprosess, NIRVarTilrettelegg, NIRUtvalgEnh
#' og NIRFigSoyler.
#' Figurtypen som genereres er avhengig av valgtVar. Ved valgtVar='InnMaate', får man ei oversikt oversikt
#' over andel av innkomsttype ved hver enhet, dvs. ei fordeling av de tre innkomsttypene for hver enhet.
#' For andre "valgtVar" viser figuren andel av den valgte variabelen for hver enhet.
#' @inheritParams NIRFigAndeler
#' @param tittel: Hvis vil angi tittel direkte
#' @param utvalgsInfo: Hvis datafil lagret med utvalgsinfo
#'
#' @return Søylediagram med AggVerdier av valgt variabel for hvert sykehus
#'
#' @export


NIRFigInnMaate <- function(RegData, valgtVar='InnMaate', datoFra='2010-01-01', datoTil='3000-01-01', aar=0,
                            minald=0, maxald=110, velgDiag=0,
                            grType=99, grVar='ShNavn', InnMaate=99, dodInt='', erMann='', hentData=0,
                            preprosess=1, outfile='', ...)
{
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = paste0("FigInnMaate: ", valgtVar))
  }
      if (hentData == 1) {
            RegData <- NIRRegDataSQL(datoFra, datoTil)
      }

      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess){
            RegData <- NIRPreprosess(RegData=RegData) #, reshID=reshID)
      }

#------- Tilrettelegge variable
#NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
#RegData <- NIRVarSpes$RegData
  RegData <- RegData[which(RegData$InnMaate %in% c(0,6,8)), ]
      #0:Planlagt operasjon, 6:Akutt nonoperativ, 8:Akutt operasjon
    #Rekoder variablene 0->1, 6->2, 8->3
  RegData$Variabel <- factor(RegData$InnMaate, levels = c(0,6,8)) #ifelse(RegData$InnMaate == 0, 1, ifelse(RegData$InnMaate==6, 2, 3))
	#gr <- c(0,6,8)
 	#RegData$VariabelGr <- factor(RegData$Variabel, levels=gr)
	tittel <-'Type opphold'
      grtxt <- c('Planlagt operasjon','Akutt non-operativ', 'Akutt operasjon') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
      subtxt <- 'Type opphold'

#------- Gjøre utvalg
 #     NIRUtvalg <- NIRUtvalgEnh(RegData=RegData,velgDiag = velgDiag)
NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar,
                          minald=minald, maxald=maxald, velgDiag = velgDiag,
                          erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, grType=grType) #overfPas=overfPas,
RegData <- NIRUtvalg$RegData
utvalgTxt <- NIRUtvalg$utvalgTxt

RegData[ ,grVar] <- as.factor(RegData[ ,grVar])


if (dim(RegData)[1] >= 0) {
	grVar <- 'ShNavn'
	RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
	Nsh <- table(RegData[ ,grVar])
} else {
	Nsh <- 0}

Ngrense <- 10


#-----------Figur---------------------------------------
if 	( max(Nsh) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
	rapFigurer::figtype(outfile)
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
	title(main=tittel, cex=0.95)
	legend('topleft',utvalgTxt, bty='n', cex=0.9)
	} else {
	tekst <- 'Ingen registrerte data for dette utvalget' }
	text(0.5, 0.5, tekst,cex=1)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {
#--------------------------------------------------------

Nshtxt <- paste0('N=', as.character(Nsh))
indShUt <- which(Nsh < Ngrense)
if (length(indShUt)==0) { indShUt <- 0}
Nshtxt[indShUt] <- paste0('N<', Ngrense)	#paste('N<', Ngrense,sep='')
N <- dim(RegData)[1]

AntInnMaate <- length(unique(RegData$Variabel))
	dataTab <- ftable(RegData[ ,c('ShNavn', 'Variabel')])/rep(Nsh,3)*100 #AntInnMaate
	dataTab[indShUt,] <-NA
	sortInd <- order(dataTab[,2])
	dataAlle <- table(RegData$Variabel)/N*100
ShNavnSort <- names(Nsh)[sortInd]	#c(names(Nsh)[sortInd],'')
NshtxtSort <- Nshtxt[sortInd]

#--------------------------FIGUR---------------------------------------------------
if (grType %in% 1:3) {xkr <- 1} else {xkr <- 0.75}
cexShNavn <- 1.2

FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=NIRUtvalg$fargepalett)
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, 0.7*strwidth(ShNavnSort, units='figure', cex=xkr*cexShNavn))
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

ymin <- 0.5/xkr^4	#Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
ymax <- 0.2+1.2*length(Nsh)
tittelpos <- 1

	#Legger til resultat for hele gruppa. Og legger til en tom etter for å få plass til legend
	pos <- barplot(cbind(as.numeric(dataAlle), rep(0,3), t(dataTab[sortInd,])), horiz=T, beside=FALSE,
			border=NA, col=farger[1:3],
			main='', font.main=1, xlab='', ylim=c(ymin, 1.05*ymax+2), las=1, cex.names=xkr) 	# ylim=c(0.05, 1.24)*length(Nsh),xlim=c(0,ymax), cex.axis=0.9, cex.names=0.8*xkr,
	ShNavnSort <- c('alle i visninga', '', ShNavnSort) #NIRUtvalg$grTypeTxt
	NshtxtSort<- c(paste0('N=', N), '', NshtxtSort)
		legend(x=50, y=1.05*ymax+2, c('Planlagt operasjon','Akutt non-operativ', 'Akutt operasjon'), xjust=0.5, yjust=0.5,	#inset=0.01,# max(pos)*1.01 x=50, y=ymax,
			fill=farger[1:3], border=farger[1:3], ncol=3, bty='n')	#cex=0.9,  ncol=6,
	xmax <- 100
	mtext('(sortert på andel "Akutt non-op.")', line=0.5, cex=1)

	mtext(at=pos, ShNavnSort, side=2, las=1, cex=cexShNavn*xkr, adj=1, line=0.25)	#Sykehusnavn
	text(x=0.005*xmax, y=pos, NshtxtSort, las=1, cex=xkr, adj=0, col=farger[4], lwd=3)	#c(Nshtxt[sortInd],''),
	mtext('Prosent (%)', las=1, side=1, cex=xkr, line=2.2*xkr)
	title(tittel, line=1.5, font.main=1, cex.main=1.5)
	mtext('(Tall på søylene angir antall registreringer)', las=1, side=1, cex=xkr, line=3.2*xkr)

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))



par('fig'=c(0, 1, 0, 1))
#savePlot(outfile, type=filtype)
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}
