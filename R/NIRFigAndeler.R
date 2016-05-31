#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling, 10-årige grupper 
#'     \item InnMaate: 
#'     \item liggetid: Liggetid 
#'     \item NEMS:  
#'     \item Nas: 
#'     \item respiratortid: 
#'     \item SAPSII: 
#'    }
#'    				
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Standard)
#'                 2: Egen enhet
#'					3: Egen sykehustype
#'				   6: Egen enhet mot egen region 
#'				   7: Egen region 
#'				   8: Egen region mot resten
#' @param preprosess Preprosesser data
#'                 FALSE: Nei
#'                 TRUE: Ja (Standard)
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param InnMaate 
#'				0: Elektivt, 
#'				6: Akutt medisinsk, 
#'				8: Akutt kirurgisk, 
#'				standard: alle (alt unntatt 0,6,8 / ikke spesifisert)
#' @param dodInt Levende/død ut fra intensiv. 
#'				0: i live, 
#'				1 -død, standard,  
#'				alle (alle andre verdier)
#'	@param ShType Må velge en av: region, sentral, lokal, alle 
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'

FigAndeler  <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='3000-12-31', 
		minald=0, maxald=130, erMann='',InnMaate='', dodInt='',outfile='', 
		reshID, enhetsUtvalg=1)	
{
#------------Gjøre utvalg-------------------------

RegData$Variabel <- 0

shtxt <- switch(as.character(enhetsUtvalg), 	'0' = 'Hele landet',
								'1' = as.character(RegData$ShNavn[match(reshID, RegData$AvdRESH)]),
								'2' = as.character(RegData$ShNavn[match(reshID, RegData$AvdRESH)]),
								'3' = as.character(RegData$ShNavn[match(reshID, RegData$AvdRESH)]))

#Hvilken sykehustype
shType=as.character(RegData$ShType[match(reshID, RegData$AvdRESH)])

#Text for figuren 
shTypetext=NA
if (shType =='lokal') {
  shTypetext='Lokale'}
if (shType =='region') {
  shTypetext='Regionale'}
if (shType =='sentral') {
  shTypetext='Sentrale'}

	
if (valgtVar == 'SAPSII') {minald <- max(18, minald)}
#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
NIRUtvalg <- NIRLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann,InnMaate=InnMaate,dodInt=dodInt)
RegData <- NIRUtvalg$RegData

utvalgTxt <- NIRUtvalg$utvalgTxt


if (valgtVar %in% c('alder','liggetid','respiratortid','SAPSII','NEMS','Nas','InnMaate')) {
RegData$Variabel <- RegData[ ,valgtVar]
}

if (valgtVar %in% c('liggetid','respiratortid')) {   
  RegData <- RegData[which(RegData$Variabel > 0), ] #Liggetid og respiratortid bare for tid>0
}

if (valgtVar =='SAPSII') {
  RegData <- RegData[which(as.numeric(RegData$SAPSII) > 0), ]
#  RegData <- RegData[which(as.numeric(RegData$alder) >= 18), ] Tas ut i NIRUtvalg
}

if (valgtVar == 'NEMS'){
  indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
  NIRdata1 <- RegData[indMed, ]
  RegData <- NIRdata1
  RegData$Variabel <- RegData$NEMS/RegData$liggetid  
}
  
if (valgtVar == 'Nas'){
  RegData$Variabel <- RegData$Nas/RegData$liggetid
  indMed <- intersect(which(RegData$Variabel <= 177), 
                      which( (RegData$liggetid > 8/24) & (RegData$Nas>0)))
  RegData <- RegData[indMed, ]  
}	

if (valgtVar == 'InnMaate'){
  indMed <- which( (RegData$InnMaate %in% c(0,6,8)))  #Maybe not neccesary just want to make sure that no other values than 0,6,8 
  NIRdata1 <- RegData[indMed, ]             
  RegData <- NIRdata1                     
}



#Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}	

#----------- Figurparametre ------------------------------
cexgr <- 1	#Kan endres for enkeltvariable
retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0


#Hvis for få observasjoner..
if (dim(RegData)[1] < 10 | (length(which(RegData$AvdRESH == reshID))<5 & enhetsUtvalg %in% c(1,3))) {
	#-----------Figur---------------------------------------
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 5 egne registreringer eller færre 10 totalt', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {


#--------------- Gjøre beregninger ------------------------------
medSml <- 0
utvalg <- c('Sh', 'Rest')	#Sh vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
Andeler <- list(Sh = 0, Rest =0)


if (enhetsUtvalg == 3) {
  RegData=subset(RegData,ShTypeTxt==shType)
}

#Hvis det skal gjøres sammenligning:
if (enhetsUtvalg %in% c(1,3)) {
	indSh <-which(RegData$AvdRESH == reshID) 	
	indRest <- which(RegData$AvdRESH != reshID)
	RegDataLand <- RegData
	ind <- list(Sh=indSh, Rest=indRest)
	medSml <- 1
}

for (teller in 1:(medSml+1)) {
if (medSml == 1) {
		RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]
	}
	#Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.

if (valgtVar=='alder') {
	tittel <- 'Aldersfordeling'
	gr <- c(seq(0, 100, 10),150)	#c(0,16,31,46,61,76,200)	
	#RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
	RegData$VariabelGr <- cut(RegData$alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
  grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
	subtxt <- 'Aldersgrupper'
}


if (valgtVar=='liggetid') {
  tittel <- 'Fordeling av liggetid'
  gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)	
  grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
  subtxt <- 'Liggetid (døgn)'
}


if (valgtVar=='respiratortid') {
  tittel <- 'Fordeling av respiratortid'
  gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)  
  grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
  subtxt <- 'Respiratortid (døgn)'
}


if (valgtVar=='SAPSII') {
  tittel <- 'Fordeling av SAPSII'
  gr <- c(seq(0, 100,10), 500) 
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
  grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')  
  subtxt <- 'SAPSII'
}



if (valgtVar=='NEMS') {
  tittel <- 'Fordeling av NEMS'  
  gr <- c(seq(0, 60,10), 500) 
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
  grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','60+')  
  #grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')  
  subtxt <- 'NEMS per døgn'
}




if (valgtVar=='Nas') {
  tittel <- 'Fordeling av Nas'   
  gr <- c(seq(0, 160, 20),500)
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
  grtxt <- c('(0-20)','[20-40)','[40-60)','[60-80)','[80-100)','[100-120)','[120-140)','[140-160)',  '160+')  
  subtxt <- 'Nas per døgn'
}




if (valgtVar=='InnMaate') {
  tittel <- 'Fordeling av Innkomstmåte'   
  gr <- c(0,6,8)
  RegData$VariabelGr <- factor(RegData$Variabel, levels=gr)
  grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
  subtxt <- 'Innkomstmåte'
}

	if (teller == 1) {Andeler$Sh <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
			Nsh <- dim(RegData)[1]}
	if (teller == 2) {Andeler$Rest <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
			Nrest <- dim(RegData)[1]}

	
}	#for-løkke


#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NIRUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Sh)), '%)', sep='')
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
	xmax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
	pos <- barplot(rev(as.numeric(Andeler$Sh)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
		col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#  
	mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

	if (medSml == 1 & enhetsUtvalg==1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
		legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')), 
			border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} 
  
	if (medSml == 1 & enhetsUtvalg==3) {
	  points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	  legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste(shTypetext,' forøvrig (N=', Nrest,')', sep='')), 
	         border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
	         lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
	}  
  
	if (enhetsUtvalg==2){  	
		legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''), 
			border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 115)
	pos <- barplot(as.numeric(Andeler$Sh), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
		sub=subtxt,	col=fargeSh, border='white', ylim=c(0, ymax))	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1 & enhetsUtvalg==1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')), 
		border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} 
if (medSml == 1 & enhetsUtvalg==3) {
  points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
  legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste(shTypetext,' forøvrig (N=', Nrest,')', sep='')), 
         border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
         lwd=lwdRest, ncol=2, cex=cexleg)
} 
if (medSml==0){	#(enhetsUtvalg==2){	
	legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''), 
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
}
