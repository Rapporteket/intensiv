#' Søylediagram som fordeling av valgt variabel, målt både før og etter hjerneslaget
#'
#' Funksjon som genererer en figur med som viser endring i en variabels fordeling fra før til etter hjerneslaget.
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NIRFigAndeler
#' @param valgtVar Hvilken variabel som skal visualiseres
#'          Boligforh: Boligforhold ved innleggelse og 3 mnd etter slaget
#'          Bosituasjon: Bosituasjon ved innleggelse og 3 mnd etter slaget
#'          Toalett: Toalettbesok før innleggelse og 3 mnd etter slaget
#'          Forflytning: Evne til forflytning før innleggelse og 3 mnd etter slaget
#'
#' @return Søylediagram som fordeling av valgt variabel, målt både før og etter hjerneslaget
#'
#' @export
NIRFigPrePost  <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil=Sys.Date(),
		minald=0, maxald=110, erMann='', diagnose='', innl4t='', NIHSSinn='', outfile='',
		reshID, enhetsUtvalg=1, preprosess=1, hentData=0,...)
{

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0("NIRFigPrePost: ", valgtVar))
  }

  if (hentData == 1) {
    RegData <- NIRRegDataSQL(datoFra, datoTil)
  }

  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess==1){
    RegData <- NIRPreprosess(RegData=RegData)
  }


#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(2,6,7)) {
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
						'7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
	}



PrePostVar <- switch(valgtVar,
				Boligforh = c('BoligforholdPre','Boligforhold3mnd'),
				Bosituasjon = c('BosituasjonPre','Bosituasjon3mnd'),
				Paakledning = c('PaakledningPre', 'Paakledning3mnd')
				)

RegData$VarPre <- RegData[ ,PrePostVar[1]]
RegData$VarPost <- RegData[ ,PrePostVar[2]]
RegData$Variabel <- as.numeric(RegData$VarPost)-as.numeric(RegData$VarPre)	#For å ta ut manglende reg i en av variablene.
#Viktig å ta ut de som ikke har registrering både før og etter.
#Følgende gjøres nå (30.04.2015) i lib-fila. Ønsker å fase det ut av lib-fila og inkluderer det derfor her.
RegData <-  RegData[intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')),
				which(RegData$Variabel != '')), ]
#Finn bedre måte!

tittel <- switch(valgtVar,
				Boligforh = c('Boligforhold ved innleggelse og 3 mnd etter slaget'),
				Bosituasjon = c('Bosituasjon ved innleggelse og 3 mnd etter slaget')
				)
#					NIHSSetterTrombektomi = c('NIHSS 24t etter trombektomi',
#						'(fordeling for pasienter som har fått trombektomi)'))

#Kun de som har hatt oppfølging:
if (valgtVar %in% c('Boligforh', 'Bosituasjon', 'Toalett', 'Forflytning', 'Paakledning',
				'Bilkjoring', 'Yrkesaktiv', 'Roykestatus', 'MRS')) {
	RegData <- RegData[which(RegData$OppfolgUtf==1), ]	#OppfolgUtf: 1-ja, 2-nei.
	datoTil <- min(datoTil, as.character(Sys.Date()-90))
}

if (valgtVar %in% c('NIHSSTrombolyse','NIHSSTrombektomi')) {
	RegData <-  switch(valgtVar,
			NIHSSTrombolyse = RegData[which(RegData$Trombolyse %in% c(1,3)), ],
			NIHSSTrombektomi = RegData[which(RegData$Trombektomi %in% c(1,3)), ])
}

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann)
RegData <- NIRUtvalg$RegData
utvalgTxt <- NIRUtvalg$utvalgTxt



#-----------Definisjoner----------------------------
cexgr <- 0.9
retn <- 'V'
txtretn <- 1
grtxt <- ''
grtxt2 <- ''
subtxt <- ''

#Alle variabele sjekket mot MRS4 (metadata) 29.april 2015
if (valgtVar == 'Boligforh') {	#Sjekket MRS4 29.april 2015
#	gr <- c(1:4,9,999)
	gr <- c(1:4,9)
	grtxt <- c('Egen bolig u/hjelp', 'Egen bolig, m/hjelp', 'Omsorgsbolig', 'Sykehjem', 'Ukjent')
	retn <- 'H'
}
if (valgtVar == 'Bosituasjon') {	#Sjekket MRS4 29.april 2015
#	gr <- c(1:3,9,999)
	gr <- c(1:3,9)
	grtxt <- c('Bor alene', 'Bor med andre', 'Institusjon', 'Ukjent')
}
if (valgtVar == 'Toalett') {	#Sjekket MRS4 29.april 2015
#	gr <- c(1:2,9,999)
	gr <- c(1:2,9)
	grtxt <- c('Klarer selv', 'Trenger hjelp', 'Ukjent')
}
if (valgtVar == 'Forflytning') {	#Sjekket MRS4 29.april 2015
#	gr <- c(1:2,9,999)
	gr <- c(1:3,9)
	grtxt <- c('Alene, overalt', 'Alene, inne', 'Trenger hjelp', 'Ukjent')
}
if (valgtVar == 'Paakledning') {
#	gr <- c(1:2,9,999)
	gr <- c(1:2,9)
	grtxt <- c('Klarer alt selv', 'Trenger hjelp', 'Ukjent')
}
if (valgtVar == 'Yrkesaktiv') {
	#Her har vi en stor andel 0-verdier
#	gr <- c(1:2,9,999)
	gr <- c(1:2,9)
	grtxt <- c('Yrkesaktiv', 'Ikke yrkesaktiv', 'Ukjent')
}
if (valgtVar == 'Roykestatus') {
	#NB: Inneholder 0 både Aldri og døde?
	#Verdier -1,0,1,2,9
#	gr <- c(0:2,9,999)
	gr <- c(0:2,9)
	grtxt <- c('Aldri', 'Røyker', 'Eks-røyker', 'Ukjent')
}
if (valgtVar == 'Bilkjoring') {
	#NB: Inneholder 0 både Aldri og døde?
#	gr <- c(1:2,9,999)
	gr <- c(1:2,9)
	grtxt <- c('Kjører bil', 'Kjører ikke bil', 'Ukjent')
}

if (!(valgtVar %in% c('NIHSSTrombolyse', 'NIHSSTrombektomi','MRS'))) {	#Dvs alle def. over
	#grNivaa <- gr[1:(length(gr)-1)]
	RegData <- RegData[RegData$VarPre %in% gr, ]
	RegData <- RegData[RegData$VarPost %in% gr, ]
	RegData$VarPre <- factor(as.numeric(RegData$VarPre), levels=gr, labels = grtxt)
	RegData$VarPost <- factor(as.numeric(RegData$VarPost), levels=gr, labels = grtxt)
}
if (valgtVar == 'MRS') {
	#Hvis tar ut de som ikke har oppfølgingsskjema og MRS3mnd=0, mens døde uten oppf.skjema er med:
	#	RegData <- RegData[setdiff(1:dim(RegData)[1], which(RegData$OppfolgUtf ==2 & RegData$MRS3mnd == 0 )), ]
	#	gr <- c(0:6,999)
	#Har kun med levende oppfølging:
	gr <- c(0,3,5,5.1)
	RegData$VarPre <- cut(as.numeric(RegData$VarPre), breaks=gr, include.lowest=TRUE, right=FALSE)
	RegData$VarPost <- cut(as.numeric(RegData$VarPost), breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c('0-2', '3-4', '5')
	subtxt <- 'MRS-grad'
#0 – Ingen symptomer.
#1 – Ingen signifikant funksjonshemning. Personen er i stand til å utføre alle vanlige aktiviteter trass i symptomene.
#2 – Lett funksjonshemning. Personen er i stand til å ivareta egen livsførsel uten assistanse, men er ikke i stand til utføre alle aktiviteter som før sykdomsdebut.
#3 – Moderat funksjonshemning. Personen behøver noe hjelp, men kan gå uten assistanse.
#4 – Moderat alvorlig funksjonshemning. Personen er ute av stand til å ivareta kroppslige behov uten assistanse, og ute av stand til å gå uten assistanse.
#5 – Alvorlig funksjonshemning. Krever konstant pleie, sengebundet, inkontinent.
#6 – Død.
}
if (valgtVar %in% c('NIHSSTrombolyse','NIHSSTrombektomi')) {
	#Utvalg på de som har fått trombolyse/trombektomi.
	#Her vet vi ikke om det er utført NIHSS el om 0 er default
	#RegData <-  switch(valgtVar,
	#		NIHSSTrombolyse = RegData[which(RegData$Trombolyse %in% c(1,3)), ],
	#		NIHSSTrombektomi = RegData[which(RegData$Trombektomi %in% c(1,3)), ])
	gr <- c(0,6,11,16,21,100)
	RegData$VarPre <- cut(as.numeric(RegData$VarPre), breaks=gr, include.lowest=TRUE, right=FALSE)
	RegData$VarPost <- cut(as.numeric(RegData$VarPost), breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c('0-5','6-10','11-15','16-20','21+')
	cexgr <- 0.8
	subtxt <- 'Totalscore'
}
#---------------BEREGNINGER --------------------------

indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,6)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$Avdeling[indEgen1]) } else {
		shtxt <- switch(as.character(enhetsUtvalg),
			'0' = 'Hele landet',
			'7' = as.character(RegData$Region[indEgen1]),
			'8' = as.character(RegData$Region[indEgen1]))
			}

if (enhetsUtvalg %in% c(0,2,7)) {		#Ikke sammenlikning
			medSml <- 0
			indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,7. (+ 6)
			indRest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			if (enhetsUtvalg %in% c(1,6)) {	#Involverer egen enhet
				indHoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
				indHoved <- switch(as.character(enhetsUtvalg),
						'8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
			smltxt <- switch(as.character(enhetsUtvalg),
				'1' = 'landet forøvrig',
				'6' = paste(RegData$Region[indEgen1], ' forøvrig', sep=''),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}




utvalg <- c('Hoved','Rest')
AndelerPP <- list(Hoved = 0, Rest =0)

#Andeler$Hoved <- round(table(RegData$VariabelGr)/length(RegData$VariabelGr)*100,2)
AntHovedPre <- table(RegData$VarPre[indHoved]) #table(cut(RegData$VarPre, gr, right=F)) #cut sikrer at har med alle kategorier
AntHovedPost <- table(RegData$VarPost[indHoved])
NHoved <- sum(AntHovedPre)	#length(indHoved)
Nrest <- 0
AndelerPP$Hoved <- cbind(AntHovedPre, AntHovedPost)/NHoved*100

if (medSml == 1) {
	AntRestPre <- table(RegData$VarPre[indRest]) #table(cut(RegData$VarPre, gr, right=F)) #cut sikrer at har med alle kategorier
	AntRestPost <- table(RegData$VarPost[indRest])
	Nrest <- length(indRest)
	AndelerPP$Rest <- cbind(AntRestPre, AntRestPost)/Nrest*100
}


#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg == 1)) {
if (NHoved < 10 | (medSml ==1 & Nrest<10)) {
FigTypUt <- rapFigurer::figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=paste('variabel: ', valgtVar, sep=''))
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {

#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler

#Plottspesifikke parametre:
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NIRUtvalg$fargepalett)
NutvTxt <- length(utvalgTxt)
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
#Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 0.9	#Størrelse på legendtekst
cexpt <- 2	#Størrelse på punkter (resten av landet)

if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(AndelerPP$Hoved, AndelerPP$Rest),na.rm=T)*1.25, 110)
	pos <- barplot(t(AndelerPP$Hoved), beside=TRUE, las=txtretn, ylab="Andel pasienter (%)",
		sub=subtxt,	names.arg=grtxt, cex.names=cexgr,
		col=farger[c(2,1)], border='white', ylim=c(0, ymax))	#
	if (medSml == 1) {
		points(pos, as.numeric(t(AndelerPP$Rest)), col=fargeRest,  cex=cexpt, pch=18) #c("p","b","o"),
		legend('top', c(paste(c('Før, N=', 'Etter, N='), NHoved , sep=''),
			paste(smltxt, ' N=', Nrest, sep='')), text.width = c(2,2,2),
			bty='n', pch=c(15,15,18), pt.cex=cexpt, #lty=c(NA,NA,NA),
			col=farger[c(2,1,3)], border=farger[c(2,1,3)], ncol=3, cex=cexleg)
		} else {
		legend('top', c('Før', 'Etter', paste('N=', NHoved , sep='')), bty='n',
			fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
		}
}

if (retn == 'H') {
#Horisontale søyler
	ymax <- 2*antGr*1.6
	xmax <- min(max(c(AndelerPP$Hoved, AndelerPP$Rest),na.rm=T)*1.25, 100)
pos <- barplot(t(AndelerPP$Hoved), beside=TRUE, horiz=TRUE, main='', las=1,
		col=farger[c(2,1)], border='white', font.main=1,  xlim=c(0,xmax), ylim=c(0.25, 3.3)*antGr,
		names.arg=grtxt, cex.names=cexgr, xlab="Andel pasienter (%)")
#text(1,pos, paste('N=', t(Ngr), sep=''), adj=0, cex=0.8, col=farger[3])	#Antall obs i hver søyle
	if (medSml == 1) {
		points(as.numeric(t(AndelerPP$Rest)), y=pos+0.1,  col=fargeRest,  cex=cexpt, pch=18) #c("p","b","o"),
		legend('topleft', c(paste(c('Før, N=', 'Etter, N='), NHoved , sep=''),
			paste(smltxt, ' N=', Nrest, sep='')), text.width = c(0.2,0.2,0.21)*xmax,
			bty='n', pch=c(15,15,18), pt.cex=cexpt, #lty=c(NA,NA,NA),
			col=farger[c(2,1,3)], border=farger[c(2,1,3)], ncol=3, cex=cexleg)
		} else {
		legend('top', c('Før', 'Etter',paste('N=',NHoved,sep='')), bty='n',
			fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
		}
}

title(tittel, font.main=1)	#line=0.5,
title(shtxt, font.main=1, line=0.5)
#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3+length(tittel)-1	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}

}
}
