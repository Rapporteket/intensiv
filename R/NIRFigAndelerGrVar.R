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
	grType=99, InnMaate=99, dodInt='', erMann='', hentData=0, preprosess=1, outfile='') 
{

# ?Legge til: respiratortid, Nas, NEMS, SAPSII, liggetid over gitte grenser

if (hentData == 1) {		
  RegData <- NIRRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
     }

RegData$Variabel <- 0


if (valgtVar=='alder_u18') {	#endret fra under18
  RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
  RegData$Variabel[which(RegData$Alder<18)] <- 1 
  tittel <- 'Pasienter under 18 år'
}

if (valgtVar=='alder_over80') {	#endret fra over80
  RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
  RegData$Variabel[which(RegData$Alder>=80)] <- 1 #?Endre til >=
  tittel <- 'Pasienter over 80 år'
}

if (valgtVar=='dod30d') {
#Tar bort ukjente og de som ikke er utskrevet, dvs. tar ut 3:reinnlagt
  #RegData <- RegData[which(RegData$DischargedHospitalStatus %in% 0:2), ]    
  #RegData$Variabel[which(RegData$DischargedHospitalStatus!=0)] <- 1 
  RegData$Variabel <- RegData$Dod30
  tittel <-  'Andel opphold der pasienten døde innen 30 dager etter innleggelse'
}

if (valgtVar=='dodeIntensiv') {
	#Andel som dør på intensiv
	RegData$Variabel <- RegData$DischargedIntensiveStatus	#0: I live, 1: Død intensiv
	RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
	tittel <- 'Andel opphold der pasienten døde på intensiv'
}

if (valgtVar=='innMaate') {
	#Innleggelsesmåte. Genererer annen figurtype
      #0:Planlagt operasjon, 6:Akutt nonoperativ, 8:Akutt operasjon
      RegData$Variabel <- RegData$InnMaate	#Gir ikke mening i andelsberegning, men trenger å være tilgengelig.
      RegData <- RegData[which(RegData$InnMaate %in% c(0,6,8)), ]
	tittel <-'Innkomstmåte'
}

if (valgtVar=='respStotte') {
	#Fått respiratorstøtte. Ja=1, nei=2,
  RegData <- RegData[which(RegData$MechanicalRespirator %in% 1:2), ]
  RegData$Variabel[which(RegData$MechanicalRespirator==1)] <- 1  
  tittel <-'Andel med respiratorstøtte'
}

if (valgtVar=='reinn') {
#Andel reinnlagte kun hvor dette er registrert. #Ja=1, nei=2, ukjent=9
RegData <- RegData[which(RegData$ReAdmitted %in% 1:2), ]
RegData$Variabel[which(RegData$ReAdmitted==1)] <- 1  
  tittel <-'Andel reinnleggelser på intensivavdelingen (innen 72t)'
}

#Gjøre utvalg 
NIRutvalg <- NIRUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, InnMaate=InnMaate, grType=grType, dodInt=dodInt)
RegData <- NIRutvalg$RegData
utvalgTxt <- NIRutvalg$utvalgTxt


if (dim(RegData)[1] >= 0) {
	grVar <- 'ShNavn'
	RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
	RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
	Ngr <- table(RegData[ ,grVar])
} else {
	Ngr <- 0}

      Ngrense <- 10	

      N <- dim(RegData)[1]
	#Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
	if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
	AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
	AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	#round(100*Nvar/Ngr,2)
	
	indGrUt <- as.numeric(which(Ngr < Ngrense))
	if (length(indGrUt)==0) { indGrUt <- 0}
	AndelerGr[indGrUt] <- -0.001
	sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE) 
	
	AndelerGrSort <- AndelerGr[sortInd]
	AndelHele <- sum(RegData$Variabel==1)/N*100	#round(100*sum(RegData$Variabel)/N, 2)
	Ngrtxt <- as.character(Ngr)	#
	Ngrtxt[indGrUt] <- paste0('<', Ngrense) #paste(' (<', Ngrense,')',sep='')	#
	GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')

	andeltxt <- paste0(sprintf('%.1f',AndelerGrSort), '%') 	#round(as.numeric(AndelerGrSort),1)
	if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}


#NgrtxtSort <- Ngrtxt[sortInd]

grTypetextstreng <- c('lokal-/sentral', 'lokal-/sentral', 'region')				
if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}

#--------------------------FIGUR---------------------------------------------------
#----------- Hvis få observasjoner ---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
	figtype(outfile)
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
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


#AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
#rownames(AndelerUt) <- c(shtxt, smltxt)
#AntallUt <- rbind(AntHoved, AntRest)
#rownames(AntallUt) <- c(shtxt, smltxt)

#UtData <- list(paste(toString(TittelUt),'.', sep=''), AndelerUt, AntallUt, grtxt )
#names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
#return(invisible(UtData))





FigTypUt <- figtype(outfile, height=3*800, fargepalett=NIRutvalg$fargepalett)	
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, 0.85*strwidth(GrNavnSort, units='figure', cex=xkr*cexGrNavn))
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

ymin <- 0.5/xkr^4	#Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
ymax <- 0.2+1.2*length(Ngr)
		
if (valgtVar=='innMaate') {
#	NB: Annen sortering her. Må bruke egen sortInd !!
      AndelerGr <- table(RegData[ ,c(grVar, 'InnMaate')])/rep(Ngr,3)*100
      rownames(AndelerGr) <- rep('',dim(AndelerGr)[1])
	sortInd <- order(AndelerGr[,2])
	GrNavn <- dimnames(AndelerGr)$ShNavn #=names(Ngr)
	GrNavnSort <- paste0(names(Ngr)[sortInd], ' (', Ngrtxt[sortInd], ')')
	dataAlle <- table(RegData$Variabel)/N*100
	
	#Legger til resultat for hele gruppa. Og legger til en tom etter for å få plass til legend
	pos <- barplot(cbind(as.numeric(dataAlle), rep(0,3), t(AndelerGr[sortInd,])), horiz=T, beside=FALSE, 
			border=NA, col=farger[1:3], 
			main='', font.main=1, xlab='', ylim=c(ymin, 1.05*ymax+2), las=1, cex.names=xkr ) 	# ylim=c(0.05, 1.24)*length(Ngr),xlim=c(0,ymax), cex.axis=0.9, cex.names=0.8*xkr,
	GrNavnSort <- c(paste0(grTypeTxt, 'sykehus (', N, ')' ), ' ', GrNavnSort)
	#NgrtxtSort<- c(N, '', Ngrtxt[sortInd])
		legend(x=50, y=1.05*ymax+2, c('Elektivt','Akutt med.', 'Akutt kir.'), xjust=0.5, yjust=0.5,	#inset=0.01,# max(pos)*1.01 x=50, y=ymax,
			fill=farger[1:3], border=farger[1:3], ncol=3, bty='n')	#cex=0.9,  ncol=6,
	xmax <- 100
	mtext('(sortert på andel "Akutt med.")', line=0.5, cex=1)
} else { 
	xmax <- max(AndelerGr)*1.15
	pos <- barplot(as.numeric(AndelerGrSort), horiz=T, beside=FALSE, border=NA, col=farger[3], 
			main='', font.main=1, xlab='', las=1, cex.names=xkr, cex.axis=1, xlim=c(0,xmax),
			ylim = c(ymin, ymax))	#c(0, 1+length(AndelerGr)*1.21)) 	
	lines(x=rep(AndelHele, 2), y=c(0, max(pos)+0.55), col=farger[1], lwd=3)
	legend(x=xmax, y=1.02*ymax, xjust=1, yjust=0.5, col=farger[1], border=NA, lty=c(1,NA), lwd=3, bty='n', 	#box.col='white')
		paste('total andel, ', grTypeTxt, 'sykehus: ', sprintf('%.1f', AndelHele), '%, N=', N, sep=''))

#	text(x=pmax(AndelerGrSort, max(strwidth(NgrtxtSort, units='user', cex=xkr)))+xmax*0.01, y=pos+0.1, 
#		andeltxt, las=1, cex=xkr, adj=0, col=farger[1])	#Andeler, hvert sykehus	
	text(x=0.02*max(AndelerGr, na.rm=T), y=pos+0.1, andeltxt, las=1, cex=xkr, adj=0, col=farger[1])	#Andeler, hvert sykehus	
      }
	  
	mtext(at=pos, GrNavnSort, side=2, las=1, cex=cexGrNavn*xkr, adj=1, line=0.25)	#Sykehusnavn, inkl. N
	#Nfarge <- ifelse(valgtVar == 'innMaate', farger[4], farger[1]) 
	#if (valgtVar == 'innMaate') {text(x=0.005*xmax, y=pos, NgrtxtSort, las=1, cex=xkr, adj=0, lwd=3, col=farger[4])}
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
