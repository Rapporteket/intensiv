#' Lager søylefigur
#'
#' bLA, BAL
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
#'     \item SMR: Standardisert mortalitetsratio (Gir annen figurtype)
#'     \item liggetid: Liggetid 
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score). Per døgn.
#'     \item NEMS: Skår for ressursbruk per opphold. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item NEMS24: NEMS-skår per døgn. 
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'    }
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NIRFigSoyler <- function(RegData, Andeler, tittel='mangler tittel', smltxt, N=0, retn='H', yAkseTxt='',
					utvalgTxt, grtxt, grtxt2, cexgr=1, medSml, subtxt='', outfile='') {
xAkseTxt <- ''
pktTxt <- '' #(evt. søyletekst)
txtEtiketter  <- ''	#legend
#verdier <- ''	#andeler, gjennomsnitt, ...
verdiTxt <- '' 	#pstTxt, ...

#---------------------------------------FRA FIGANDELER og AndelGrVar--------------------------
#Hvis for få observasjoner..

if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) {
	#-----------Figur---------------------------------------
	figtype(outfile)  #FigTypUt <- figtype(outfile)
	farger <- FigTypUt$farger
	plot.new()
	#title(paste0('variabel: ', valgtVar))
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'For få registreringer', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {

	

#Plottspesifikke parametre:
#Høyde må avhenge av antall grupper
hoyde <- ifelse(length(Andeler$Hoved)>20, 3*800, 3*600)
FigTypUt <- figtype(outfile, height=hoyde, fargepalett=NIRUtvalg$fargepalett)	
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75))
#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
	
		
farger <- FigTypUt$farger
fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst


#Horisontale søyler
if (retn == 'H') {
	xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)	#max(AndelerGr)*1.15
	ymin <- 0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	ymax <- 0.2+1.2*length(Andeler$Hoved) #1.4*antGr

	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
		col=fargeHoved, border=NA, cex.names=cexgr, xlim=c(0, xmax), ylim=c(ymin,ymax))	
	#Avvik i FigAndelGrVar:
		#as.numeric(Andeler$Hoved),
		#beside=FALSE, col=farger[3], 

	if (grVar %in% c('ShNavn')	){	#Må si noe om den "gamle figurtypen"
	      grtxt <- rev(grtxt)
	      grTypeTxt <- smltxt
	      mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
	      
	      lines(x=rep(AndelHele, 2), y=c(0, max(pos)+0.55), col=farger[2], lwd=3)
	      legend(x=xmax, y=1.02*ymax, xjust=1, yjust=0.5, col=farger[2], border=NA, lty=c(1,NA), lwd=3, 
	             bty='n', cex = 0.9,	#box.col='white')
	             paste0('total andel, ', grTypeTxt, 'sykehus: ', sprintf('%.1f', AndelHele), '%, N=', N$Hoved)) 
	      text(x=0.02*max(AndelerGr, na.rm=T), y=pos+0.1, rev(soyletxt), las=1, cex=cexgr, adj=0, col=farger[1])	#Andeler, hvert sykehus	
	}

	#Legge på gruppe/søylenavn
	mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
 
	
	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
		legend('top', c(paste0(shtxt, ' (N=', N$hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
			border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest, lty=NA, ncol=1)
		} else {	
		legend('top', paste0(shtxt, ' (N=', N$hoved,')'), 
			border=NA, fill=fargeSh, bty='n', ncol=1)
		}
	}


	
if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 115)
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab=yAkseTxt,	
		sub=subtxt,	col=fargeSh, border='white', ylim=c(0, ymax))	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=0.9*cexgr, adj=0.5, line=1.5)

if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste(shtxt, ' (N=', N$Hoved,')', sep=''), paste(smltxt, ' (N=', N$Rest,')', sep='')), 
		border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {	
	legend('top', paste0(shtxt, ' (N=', N$Hoved,')'), 
		border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
	}
} 


title(tittel, line=1) #line=1.5, cex.main=1.3)

#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}

}
}