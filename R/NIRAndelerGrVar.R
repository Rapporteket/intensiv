#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en grupperingsvariabelen sykehus.
#' Funksjonen er tilrettelagt for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Figurtypen som genereres er avhengig av valgtVar. Ved valgtVar='InnMaate', får man ei oversikt oversikt
#' over andel av innkomsttype ved hver enhet.
#' For valgtVar lik reinn eller DodeInt viser figuren andel av hhv. reinnleggelser og døde for hver enhet.
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år 
#'     \item alder_over80: Pasienter over 80 år (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga. 
#'     \item innMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'		Dette valget viser en annen figurtype.
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item reinn: Andel reinnlagte (kun hvor dette er registrert, dvs. fjerner ukjente)
#'    }
#'
#' @inheritParams NIRFigAndeler 


#' @inheritParams NIRFigAndelerGrVar
#' @param valgtVar Hvilken variabel som skal visualiseres
#' @param grType Gjør gruppeutvalg for
#'                 1 eller 2: lokal-/sentralsykehus
#'				   3: regionsykehus
#' @return Søylediagram med andeler av valgt variabel for hvert sykehus
#'
#' @export
NIRFigAndelerGrVar <- function(RegData, valgtVar, minald=0, maxald=130, datoFra='2011-01-01', datoTil='3000-01-01', 
	grType=99, grVar='', InnMaate=99, dodInt='', erMann='', hentData=0, preprosess=1, outfile='', lagFig=1) 
{
#NB: Tomme grVar fjernes så dette kan ikke være standard
if (hentData == 1) {		
  RegData <- NIRRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
     }

#------- Tilrettelegge variable
NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
RegData <- NIRVarSpes$RegData

#------- Gjøre utvalg
NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
                       overfPas=overfPas, erMann=erMann, InnMaate=InnMaate, dodInt=dodInt)
RegData <- NIRUtvalg$RegData
utvalgTxt <- NIRUtvalg$utvalgTxt



if (dim(RegData)[1] >= 0) {
	RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
	RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
	Ngr <- table(RegData[ ,grVar])
	} else {
		Ngr <- 0}

    Ngrense <- 10	
	Andeler <- list(Hoved = 0, Rest =0)
	N <- list(Hoved = 0, Rest =0)
	Ant <- list(Hoved = 0, Rest =0)

    N$Hoved <- dim(RegData)[1]
	if(N$Hoved > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
	AntGr <- length(which(Ngr >= Ngrense))	
	AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	
	
	indGrUt <- as.numeric(which(Ngr < Ngrense))
	if (length(indGrUt)==0) { indGrUt <- 0}
	AndelerGr[indGrUt] <- -0.001	#Endre til NA
	sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE) 
	
	AndelerGrSort <- AndelerGr[sortInd]
	AndelHele <- sum(RegData$Variabel==1)/N$Hoved*100
	Ngrtxt <- as.character(Ngr)	
	Ngrtxt[indGrUt] <- paste0('<', Ngrense) 
	grtxt <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')

	soyletxt <- paste0(sprintf('%.1f',AndelerGrSort), ' %') 	
	if (length(indGrUt)>0) {soyletxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}


#grTypetextstreng <- c('lokal-/sentral', 'lokal-/sentral', 'region')				
#if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}


FigDataParam <- list(Andeler=Andeler, N=N, 
					 Ant=Ant,
                     grtxt2='', 
					 soyletxt=soyletxt,
                     grtxt=NIRVarSpes$grtxt,
                     tittel=NIRVarSpes$tittel, 
                     retn='H', 
                     subtxt=NIRVarSpes$subtxt,
                     utvalgTxt=NIRUtvalg$utvalgTxt, 
                     fargepalett=NIRUtvalg$fargepalett, 
                     medSml=NIRUtvalg$medSml, 
                     smltxt=NIRUtvalg$smltxt)

#FigDataParam skal inn som enkeltparametre i funksjonskallet
if (lagFig == 1) {
      NIRFigSoyler(RegData, FigDataParam=FigDataParam, outfile)
      }





#--------------------------FIGUR---------------------------------------------------
#----------- Hvis få observasjoner ---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
	figtype(outfile)
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

if (grType %in% 1:3) {xkr <- 1} else {xkr <- 0.85}
cexGrNavn <- 0.9



FigTypUt <- figtype(outfile, height=3*800, fargepalett=NIRutvalg$fargepalett)	
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, 0.85*strwidth(grtxt, units='figure', cex=xkr*cexGrNavn))
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

ymin <- 0.5/xkr^4	#Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
ymax <- 0.2+1.2*length(Ngr)
		
	xmax <- max(AndelerGr)*1.15
	pos <- barplot(as.numeric(AndelerGrSort), horiz=T, beside=FALSE, border=NA, col=farger[3], 
			main='', font.main=1, xlab='', las=1, cex.names=xkr, cex.axis=1, xlim=c(0,xmax),
			ylim = c(ymin, ymax))	#c(0, 1+length(AndelerGr)*1.21)) 	
	lines(x=rep(AndelHele, 2), y=c(0, max(pos)+0.55), col=farger[2], lwd=3)
	legend(x=xmax, y=1.02*ymax, xjust=1, yjust=0.5, col=farger[2], border=NA, lty=c(1,NA), lwd=3, bty='n', 	#box.col='white')
		paste('total andel, ', grTypeTxt, 'sykehus: ', sprintf('%.1f', AndelHele), '%, N=', N, sep=''))

	text(x=0.02*max(AndelerGr, na.rm=T), y=pos+0.1, soyletxt, las=1, cex=xkr, adj=0, col=farger[1])	#Andeler, hvert sykehus	
	  
	mtext(at=pos, grtxt, side=2, las=1, cex=cexGrNavn*xkr, adj=1, line=0.25)	#Sykehusnavn, inkl. N
	title(tittel, line=1.5, font.main=1, cex.main=1.3)
	mtext('Prosent (%)', las=1, side=1, cex=xkr, line=2.2*xkr)
	mtext('(Tall på søylene angir antall registreringer)', las=1, side=1, cex=xkr, line=3.2*xkr)

mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=xkr, adj=1, line=0.25)

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))




par('fig'=c(0, 1, 0, 1)) 
#savePlot(outfile, type=filtype)
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}
