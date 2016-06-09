#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
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
#'     \item NEMS: Skår for ressursbruk. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score)
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'    }
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NIRFigGjsnGrVar <- function(RegData, valgtVar, valgtMaal='Gjsn', minald=0, maxald=130, datoFra='2011-01-01', 
			datoTil='3000-01-01', grType=99, InnMaate=99, dodInt='', erMann='', preprosess=1, hentData=0, 
			outfile) {

#Inngangsparametre:
	#grType - Må velge en av: region, sentral, lokal, alle 
	
if (hentData == 1) {		
  RegData <- NIRRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- NIRPreprosess(RegData=RegData, reshID=reshID)
     }

grVar <- 'ShNavn'
#RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn


Ngrense <- 10		
ben <- NULL		#Benevning

RegData$Variabel  <- as.numeric(RegData[ ,valgtVar])

if (valgtVar == 'SMR') {
#Tar ut reinnlagte og overflyttede, samt de med SAPSII=0 (ikke scorede) 
#og de 	under 18år (tas ut i NIRutvalg)
	minald <- max(18, minald) 
	RegData <- RegData[RegData$DischargedHospitalStatus!= 3, ]
	RegData <- RegData[RegData$Overf==1, ] 
	RegData <- RegData[as.numeric(RegData$SAPSII) > 0, ]
}

if (valgtVar %in% c('respiratortid', 'liggetid')) {
#Liggetid og respiratortid bare >0
#RegData <- RegData[RegData$Overf==1, ] #for ikke overflyttede. Tar ikke lenger ut overflyttede
RegData <- RegData[which(RegData[ ,valgtVar]>0), ] 
ben <- ' (dager)'
} 

if (valgtVar == 'SAPSII') {
#Tar ut SAPSII=0 (ikke scorede)
#og de under 18år (tas ut i NIRutvalg)
minald <- max(18, minald)
RegData <- RegData[as.numeric(RegData$SAPSII) > 0, ]
}

if (valgtVar == 'Nas') {
		#valgtVar <- 'NAS24'
		RegData$NAS24 <- RegData$Nas/RegData$liggetid	#floor(RegData$liggetid)
		indMed <- intersect(which(RegData$NAS24 <= 177), 
							which( (RegData$liggetid > 8/24) & (RegData$Nas>0)))
		RegData <- RegData[indMed, ]
		RegData$Nas <- RegData$NAS24
}

	if (valgtVar=='NEMS') {
	#Inkluderer: opphald lenger enn 24 timar og det faktisk er skåra NEMS-poeng.
	#Dvs. NEMS-poeng totalt/liggjedøger, altså NEMS/24 timar
		indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
		RegData <- RegData[indMed, ]
		RegData$NEMS24 <- RegData$NEMS/RegData$liggetid	#floor(RegData$liggetid)
	}

#Gjøre utvalg
NIRutvalg <- NIRUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, InnMaate=InnMaate, grType=grType, dodInt=dodInt)
RegData <- NIRutvalg$RegData
utvalgTxt <- NIRutvalg$utvalgTxt

RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
#Sykehus som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
#sykehus komme med uansett om de ikke har registreringer.

#	RegData$Variabel <- as.numeric(RegData[ ,valgtVar])
	if(dim(RegData)[1]>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}


if (valgtMaal=='Med') {
t1 <- 'Median ' } else {t1 <- 'Gjennomsnittlig '}

if( valgtVar =='SMR') {t1 <- ''}

grTypetextstreng <- c('lokal-/sentral', 'lokal-/sentral', 'regional')				
if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}
tittel <- c(paste(t1, valgtVar, ', ', grTypeTxt,'sykehus', sep='')) 
			
	
#-----------Figur---------------------------------------
if 	( (max(Ngr) < Ngrense) | (valgtMaal=='Med' & valgtVar=='SMR'))	{
#Dvs. hvis ALLE er mindre enn grensa eller at vi har valgt median for SMR
figtype(outfile)
plot.new()
	if (valgtMaal=='Med' & valgtVar=='SMR') {tekst <- 'Ugyldig parameterkombinasjon'
	} else {
		if (dim(RegData)[1]>0) {
		tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
		title(main=tittel, cex=1)
		legend('topleft',utvalgTxt, bty='n', cex=0.9)
		} else {
		tekst <- 'Ingen registrerte data for dette utvalget' }
	}
	text(0.5, 0.5, tekst,cex=1)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {
#--------------------------------------------------------


Ngrtxt <- paste(' (', as.character(Ngr),')', sep='') #paste('N=', as.character(Ngr), sep='')
indGrUt <- which(Ngr < Ngrense)
if (length(indGrUt)==0) { indGrUt <- 0}
Ngrtxt[indGrUt] <- paste(' (<', Ngrense,')',sep='')	#paste('N<', Ngrense,sep='')
N <- dim(RegData)[1]

dummy0 <- -0.001
#Kommer ut ferdig sortert!
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
	MedIQR$stats[ ,indGrUt] <- dummy0
	MedIQR$conf[ ,indGrUt] <- dummy0
	sortInd <- order( MedIQR$stats[3,], decreasing=TRUE) 
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

	
} else {	#Gjennomsnitt blir standard.
	if (valgtVar=='SMR') { 
		#DischargedIntensiveStatus: 0 i live (0), 1 død (1 og 2) [har tatt ut reinnl (3)]
		RegData$StatusUtGr <- ifelse(RegData$DischargedHospitalStatus == 0, 0, 1) 
		ObsGr <- tapply(RegData$StatusUtGr, RegData[ ,grVar], mean, na.rm=T)
		EstGr <- tapply(RegData$SMR, RegData[ ,grVar], mean, na.rm=T)
		ind0 <- which(EstGr == 0)
		Gjsn <- 100*ObsGr/EstGr  
		if (length(ind0)>0) {Gjsn[ind0] <- 0}#Unngå å dele på 0
		#Vi benytter p.t. ikke konf.int for SMR. Setter alle SE lik 0
		     #TestPoGr <- which((Ngr*ObsGr-3*sqrt(Ngr*ObsGr*(1-ObsGr)) <= 0) | (Ngr*ObsGr+3*sqrt(Ngr*ObsGr*(1-ObsGr)) > Ngr))
		     #SE <- sqrt(ObsGr*(1-ObsGr))*100/(sqrt(Ngr)*EstGr)
		     #if (length(TestPoGr)>0) {SE[TestPoGr] <- 0}
		SE <- rep(0, length(Ngr))
		Obs <-  mean(RegData$StatusUtGr)	#Kun 0 og 1
		Est <- mean(RegData$Variabel, na.rm=T)
		MidtHele <- ifelse(Est ==0, 0, 100*Obs/Est)
		KIHele <- c(0,0)    #MidtHele +  100*sqrt(Obs*(1-Obs)/N)/Est*c(-2,2)
	} else {
		#Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
		#SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
		Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
		SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
		MidtHele <- mean(RegData$Variabel)	#mean(RegData$Variabel)
		KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
		}
	Gjsn[indGrUt] <- dummy0
	SE[indGrUt] <- 0
	sortInd <- order(Gjsn, decreasing=TRUE) 
	Midt <- as.numeric(Gjsn[sortInd])
	KIned <- Gjsn[sortInd] - 2*SE[sortInd]
	KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
	}

GrNavnSort <- paste(names(Ngr)[sortInd], Ngrtxt[sortInd], sep='')
AntGr <- length(which(Midt>=0))
if (valgtVar == 'SMR') {AntDes <- 2} else {AntDes <- 1} 
xmax <-  min(1.1*max(c(Midt, KIned, KIopp, KIHele)), 1.5*max(Midt))
medKI <- ifelse(valgtVar=='SMR', 0, 1)

#DENNE SKAL VÆRE OVERFLØDIG:
# modified to cope in case of crappy SMR data
titleColor <- "black"
if (valgtVar=='SMR') {
  indFiniteMidt <- is.finite(Midt)
  indFiniteKIned <- is.finite(KIned)
  indFiniteKIopp <- is.finite(KIopp)
  indFiniteKIHele <- is.finite(KIHele)
  xmax <- min(1.1*max(c(Midt[indFiniteMidt], KIned[indFiniteKIned],
                        KIopp[indFiniteKIopp], KIHele[indFiniteKIHele])),
              1.5*max(Midt[indFiniteMidt]))
  if (is.element(FALSE, c(indFiniteMidt, indFiniteKIned, indFiniteKIopp,
                              indFiniteKIHele))) {
    titleColor <- "red"
    tittel <- paste(tittel, "\nNB Inneholder udefinerte verdier som kan bety feil i data!")
  }
}

#--------------------------FIGUR---------------------------------------------------
if (grType %in% 1:3) {xkr <- 1} else {xkr <- 0.75}
cexGrNavn <- 1.2

FigTypUt <- figtype(outfile, height=3*800, fargepalett=NIRutvalg$fargepalett)	
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, 0.7*strwidth(GrNavnSort, units='figure', cex=xkr*cexGrNavn))
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
	
	#plot.new()
	pos <- barplot(Midt, horiz=T, border=NA, col=farger[3], #main=tittel,
		xlim=c(0,xmax), ylim=c(0.3/xkr^4, 0.3+1.25*length(Midt)), #0.2+1.2*length(Ngr)
		#xlim=c(0,xmax), ylim=c(0.05*length(Midt), 1.2+1.27*length(Midt)), 
		#font.main=1, cex.lab=0.9, cex.names=0.9, cex.axis=0.9, 
		col.axis='white', xlab='', las=1) 	
	posKI <- pos[1:AntGr]

	if (valgtVar == 'SMR') {	#Dvs. skal ha med konfidensintervall
		legend('top', 	c(paste(grTypeTxt, 'sykehus, ', sprintf('%.2f', MidtHele), ', N=', N, sep=''),
				'(uten reinnlagte og overflyttede pasienter)'),
				col=c(farger[2],NA), lwd=c(2,NA), bty='n')	#, cex=0.8
	} else {
		polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), c(0, max(posKI)+min(posKI), max(posKI)+min(posKI),0), 
			col=farger[4], border=farger[4])
		legend('top', fill=c('white', farger[4]),  border='white', lwd=2, 
			col=c(farger[2], farger[4]), seg.len=0.6, merge=TRUE, bty='n',
			c(paste(t1, valgtVar, ': ', sprintf('%.1f', MidtHele), ', N=', N, sep=''), 
				paste('95% konf.int., ', grTypeTxt, 'sykehus (', 
				sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')', sep='')))   
	}
	lines(x=rep(MidtHele, 2), y=c(0,max(posKI)+min(posKI)), col=farger[2], lwd=2)
	barplot(Midt[1:AntGr], horiz=T, border=NA, col=farger[3], 	#main=tittel,
		xlim=c(0, xmax), add=TRUE, cex.lab=0.9*xkr, 	#cex.axis=0.9, 
			font.main=1, xlab='', las=1) 	
	mtext(paste(t1, valgtVar, ben, sep=''), las=1, side=1, line=2) #cex=xkr, 
	title(tittel, line=1, font.main=1, col.main=titleColor)
	#soyletxt <- c(sprintf(paste('%.', AntDes,'f', sep=''), Midt[1:AntGr]), rep('',length(Ngr)-AntGr))
	text(x=0.005*xmax, y=pos+0.1, las=1, cex=xkr, adj=0, col=farger[1],
				c(sprintf(paste('%.', AntDes,'f', sep=''), Midt[1:AntGr]), rep('',length(Ngr)-AntGr)))
	mtext(at=pos+0.1, GrNavnSort, side=2, las=1, cex=cexGrNavn*xkr, adj=1, line=0.25)	
	
#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
	

if (valgtVar != 'SMR') {	#Skal ha med konf.int.
	#options(warn=-1)	#Unngå melding om KI med lengde 0
	AntGr <- length(which(Midt>0))
	arrows(x0=Midt[1:AntGr], y0=posKI[1:AntGr], x1=KIopp[1:AntGr], y1=posKI[1:AntGr], 
		length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	arrows(x0=Midt[1:AntGr], y0=posKI[1:AntGr], x1=KIned[1:AntGr], y1=posKI[1:AntGr], 
		length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
}
par('fig'=c(0, 1, 0, 1)) 
#if (filtype=='pdf') {savePlot(outfile, type=filtype)}
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}
