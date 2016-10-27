#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling, 10-årige grupper 
#'     \item InnMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'     \item liggetid: Liggetid 
#'     \item NEMS: Skår for ressursbruk. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score)
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'    }
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'     \item 3: Egen enhet mot egen sykehustype
#'     \item 4: Egen sykehustype
#'     \item 5: Egen sykehustype mot resten av landet
#'     \item 6: Egen enhet mot egen region [NB: Intensivregiisteret mangler pt. variabel for region]
#'     \item 7: Egen region [NB: Mangler pt. variabel for region]
#'	   \item 8: Egen region mot resten [NB: Mangler pt. variabel for region]
#'    	}							
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
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.
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
#'				1: død,   
#'				alle, standard (alle andre verdier)
#' @param overfPas Overført under pågående intensivbehandling? 
#'				1 = Pasienten er ikke overført
#'				2 = Pasienten er overført
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'

NIRFigAndelerHC  <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', 
		minald=0, maxald=130, erMann='',InnMaate='', dodInt='',outfile='', 
		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)	
{


if (hentData == 1) {		
  RegData <- NIRRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
     }

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(2,3,4,6,7)) {	
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'3' = subset(RegData,ShType==ShType[indEgen1]),
						'4' = RegData[which(RegData$ShType == RegData$ShType[indEgen1]),],	#kun egen shgruppe
						'6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
						'7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
	}

NIRUtvalg <- NIRUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
                       erMann=erMann,InnMaate=InnMaate,dodInt=dodInt)
RegData <- NIRUtvalg$RegData


#----------- Figurparametre ------------------------------
cexgr <- 1	#Kan endres for enkeltvariable
retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
"%i%" <- intersect


#--------------- Definere variable ------------------------------

if (valgtVar=='alder') {
	tittel <- 'Aldersfordeling'
	gr <- c(seq(0, 100, 10),150)	#c(0,16,31,46,61,76,200)	
	RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
    grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
	subtxt <- 'Aldersgrupper'
}

if (valgtVar=='InnMaate') {
  tittel <- 'Fordeling av Innkomstmåte'   
  indMed <- which((RegData$InnMaate %in% c(0,6,8)))  #Maybe not neccesary just want to make sure that no other values than 0,6,8 
  RegData <- RegData[indMed, ]             
  gr <- c(0,6,8)
  RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
  grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
  subtxt <- 'Innkomstmåte'
}

if (valgtVar=='liggetid') {
  tittel <- 'Fordeling av liggetid'
  RegData <- RegData[which(RegData$liggetid > 0), ] #Liggetid og respiratortid bare for tid>0
  gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
  RegData$VariabelGr <- cut(RegData$liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)	
  grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
  subtxt <- 'Liggetid (døgn)'
}

if (valgtVar=='Nas') {
  tittel <- 'Fordeling av Nas'   
  RegData$Variabel <- RegData$Nas/RegData$liggetid
  indMed <- which(RegData$Variabel <= 177) %i% which( (RegData$liggetid > 8/24) & (RegData$Nas>0))
  RegData <- RegData[indMed, ]  
  gr <- c(seq(0, 160, 20),500)
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
  grtxt <- c('(0-20)','[20-40)','[40-60)','[60-80)','[80-100)','[100-120)','[120-140)','[140-160)',  '160+')  
  subtxt <- 'Nas per døgn'
}

if (valgtVar=='NEMS') {
  tittel <- 'Fordeling av NEMS per døgn'  
  indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
  RegData <- RegData[indMed, ]
  gr <- c(seq(0, 60,10), 500) 
  RegData$Variabel <- RegData$NEMS/RegData$liggetid  
  RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
  grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','60+')  
  subtxt <- 'NEMS per døgn'
}

if (valgtVar=='respiratortid') {
  tittel <- 'Fordeling av respiratortid'
  RegData <- RegData[which(RegData$respiratortid > 0), ] #Bare for tid>0
  gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
  RegData$VariabelGr <- cut(RegData$respiratortid, breaks=gr, include.lowest=TRUE, right=FALSE)  
  grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
  subtxt <- 'Respiratortid (døgn)'
}

if (valgtVar=='SAPSII') {
  tittel <- 'Fordeling av SAPSII'
  minald <- max(18, minald)     #Bare voksne skal skåres
   RegData <- RegData[which(as.numeric(RegData$SAPSII) > 0), ]
  gr <- c(seq(0, 100,10), 500) 
  RegData$VariabelGr <- cut(RegData$SAPSII, breaks=gr, include.lowest=TRUE, right=FALSE) 
  grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')  
  subtxt <- 'SAPSII'
}



NIRUtvalg <- NIRUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
                       overfPas=overfPas, erMann=erMann, InnMaate=InnMaate, dodInt=dodInt)
RegData <- NIRUtvalg$RegData
utvalgTxt <- NIRUtvalg$utvalgTxt

shTypetext <- c('lokale/sentrale', 'lokale/sentrale', 'regionale')				
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$ShNavn[indEgen1]) } else {
		shtxt <- switch(as.character(enhetsUtvalg), 	
			'0' = 'Hele landet',
			'4' = shTypetext[RegData$ShType[indEgen1]],
			'5' = shTypetext[RegData$ShType[indEgen1]],
			'7' = as.character(RegData$Region[indEgen1]),
			'8' = as.character(RegData$Region[indEgen1]))
			}
			
if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
			medSml <- 0
			indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
			indRest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
				indHoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
				indHoved <- switch(as.character(enhetsUtvalg),
						'5' = which(RegData$ShType == RegData$ShType[indEgen1]),	#shgr
						'8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
			smltxt <- switch(as.character(enhetsUtvalg),
				'1' = 'landet forøvrig',
				'3' = paste('andre ', shTypetext[RegData$ShType[indEgen1]], sep=''),	#RegData inneh. kun egen shgruppe
				'5' = 'andre typer sykehus',
				'6' = paste(RegData$Region[indEgen1], ' forøvrig', sep=''),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
				'5' = which(RegData$ShType != RegData$ShType[indEgen1]),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}								

			
#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0
AntHoved <- switch(as.character(flerevar), 
				'0' = table(RegData$VariabelGr[indHoved]),
				'1' = colSums(sapply(RegData[indHoved ,variable], as.numeric), na.rm=T))
NHoved <- switch(as.character(flerevar), 
				'0' = sum(AntHoved),	#length(indHoved)- Kan inneholde NA
				'1' = length(indHoved))
Andeler$Hoved <- 100*AntHoved/NHoved

if (medSml==1) {
	AntRest <- switch(as.character(flerevar), 
					'0' = table(RegData$VariabelGr[indRest]),
					'1' = colSums(sapply(RegData[indRest ,variable], as.numeric), na.rm=T))
	NRest <- switch(as.character(flerevar), 
					'0' = sum(AntRest),	#length(indRest)- Kan inneholde NA
					'1' = length(indRest))
	Andeler$Rest <- 100*AntRest/NRest
}

			
# #-----------Figur---------------------------------------
# #Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
# 
# #Hvis for få observasjoner..
# if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) {
# 	#-----------Figur---------------------------------------
# FigTypUt <- figtype(outfile)
# farger <- FigTypUt$farger
# 	plot.new()
# 	title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
# 	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
# 	text(0.5, 0.6, 'Færre enn 5 egne registreringer eller færre enn 10 totalt', cex=1.2)
# 	if ( outfile != '') {dev.off()}
# } else {
# 
# 
# 
# #Plottspesifikke parametre:
# FigTypUt <- figtype(outfile, fargepalett=NIRUtvalg$fargepalett)
# #Tilpasse marger for å kunne skrive utvalgsteksten
# NutvTxt <- length(utvalgTxt)
# grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Hoved)), '%)', sep='')
# vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
# #vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
# par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
# 	
# farger <- FigTypUt$farger
# fargeSh <- farger[1]
# fargeRest <- farger[3]
# antGr <- length(grtxt)
# lwdRest <- 3	#tykkelse på linja som repr. landet
# cexleg <- 1	#Størrelse på legendtekst
# 
# #Horisontale søyler
# if (retn == 'H') {
# 	xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
# 	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
# 		col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#  
# 	mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)
# 
# 	if (medSml == 1) {
# 		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
# 		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
# 			border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
# 			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
# 		} else {	
# 		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
# 			border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
# 		}
# 	}
# 
# if (retn == 'V' ) {
# #Vertikale søyler eller linje
# 	ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 115)
# 	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
# 		sub=subtxt,	col=fargeSh, border='white', ylim=c(0, ymax))	
# 	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
# 	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
# 
# if (medSml == 1) {
# 	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
# 	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
# 		border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
# 		lwd=lwdRest, ncol=2, cex=cexleg)
# 	} else {	
# 	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
# 		border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
# 	}
# } 
# 
# 
# title(tittel, line=1, font.main=1)
# 
# #Tekst som angir hvilket utvalg som er gjort
# avst <- 0.8
# utvpos <- 3	#Startlinje for teksten
# mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
# 
# par('fig'=c(0, 1, 0, 1)) 
# if ( outfile != '') {dev.off()}

# }

AndelerHighchart(outfile)
}
