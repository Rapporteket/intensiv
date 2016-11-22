



#---------------------------------------FRA ANDELER--------------------------
#Hvis for få observasjoner..
if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) {
	#-----------Figur---------------------------------------
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 5 egne registreringer eller færre enn 10 totalt', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {



#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NIRUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Hoved)), '%)', sep='')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
#vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
	
farger <- FigTypUt$farger
fargeSh <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
		col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#  
	mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
			border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {	
		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
			border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
		}
	}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 115)
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
		sub=subtxt,	col=fargeSh, border='white', ylim=c(0, ymax))	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)

if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
		border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {	
	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
		border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
	}
} 


title(tittel, line=1, font.main=1)

#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}

}
