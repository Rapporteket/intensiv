#' Utvikling over tid for gjennomsnitt/median av valgt variabel
#'
#' Figuren viser gjennomsnitt/median per år med konfidensintervall for valgt variabel.
#' I bakgrunn vises konfidensintervall for det man har valt å sammenlikne med.
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
#'     \item respiratortidNonInv: Respiratortid, ikke-invasiv
#'     \item respiratortidInvMoverf: Respiratortid, invasiv m/overf.
#'     \item respiratortidInvUoverf: Respiratortid, invasiv u/overf.
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
#'     \item 6: Egen enhet mot egen region 
#'     \item 7: Egen region 
#'	   \item 8: Egen region mot resten [
#'    	}							
#'
#' @inheritParams NIRFigAndeler 
#' @inheritParams NIRVarTilrettelegg
#' @param valgtMaal
#'        'Gjsn': gir middelverdi (standard)
#'        'Med': gir median
#' @param valgtVar Hvilken variabel som skal visualiseres
#'          
#' @return Linjediagram som viser utvikling over tid for valgt variabel
#'
#' @export
NIRFigGjsnTid <- function(RegData, valgtVar='alder', datoFra='2011-01-01', datoTil='3000-12-31', tidsenhet='Mnd',
                    minald=0, maxald=110, erMann='', reshID=0, InnMaate='', dodInt='', tittel=1, 
                    outfile='',enhetsUtvalg=0, valgtMaal='Gjsn', preprosess=1, hentData=0,...){
  
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = paste0("GjsnTid: ", valgtVar))
  }
  if (hentData == 1) {		
    RegData <- NIRRegDataSQL(datoFra, datoTil)
  }
  
# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess){
    RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
  }
  

  #--------------- Definere variable ------------------------------
  
  NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='gjsnGrVar')
  RegData <- NIRVarSpes$RegData
  
  
  NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, #aar=aar, 
							minald=minald, maxald=maxald, 
							erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, 
							reshID=reshID, enhetsUtvalg=enhetsUtvalg) #overfPas = overfPas,
  RegData <- NIRUtvalg$RegData
  utvalgTxt <- NIRUtvalg$utvalgTxt
  medSml <- NIRUtvalg$medSml
  KImaal <- ifelse(valgtMaal=='Med', NIRVarSpes$KImaal, NA)
  KImaaltxt <- ifelse(NIRVarSpes$KImaaltxt=='', '', 
                      ifelse(valgtMaal=='Med', paste0('Mål:',NIRVarSpes$KImaaltxt), ''))
  
  
#------------------------Klargjøre tidsenhet--------------
  Ntest <- dim(RegData)[1]
  #N <- list(Hoved = 0, Rest =0)
  if (Ntest>2) {
            RegDataFunk <- SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
            RegData <- RegDataFunk$RegData
            #tidtxt <- RegDataFunk$tidtxt
            tidNum <- min(RegData$TidsEnhetSort, na.rm=T):max(RegData$TidsEnhetSort, na.rm = T) #as.numeric(levels(RegData$TidsEnhetSort))
            
#--------------- Gjøre beregninger ------------------------------
KIekstrem <- NULL

AggVerdier <- list(Hoved = 0, Rest =0)
ind <- NIRUtvalg$ind
N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))

#tidtxt <- min(RegData$Aar):max(RegData$Aar)
#Erstattes av TidsEnhet: RegData$Aar <- factor(RegData$Aar, levels=tidtxt)
#Aartxt <- min(RegData$Aar):max(RegData$Aar)
#RegData$Aar <- factor(RegData$Aar, levels=Aartxt)
#AntTidsenh <- length(Aartxt)

#Resultat for hovedgruppe
Ngr <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], length)
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData$TidsEnhet[ind$Hoved],RegData$Variabel[ind$Hoved],  notch=TRUE, plot=FALSE)
	Midt <- as.numeric(MedIQR$stats[3, ])	#as.numeric(MedIQR$stats[3, sortInd])
	Konf <- MedIQR$conf
	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(Ngr/2 - 1.96*sqrt(Ngr/4))
	#k <- ceiling(Ngr/2 + 1.96*sqrt(Ngr/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).) 
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared, 
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give 
#roughly a 95% confidence interval for the difference in two medians. 	
} else {	#Gjennomsnitt blir standard.
	Midt <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], mean, na.rm=T)
	SD <- tapply(RegData[ind$Hoved ,'Variabel'], RegData[ind$Hoved, 'TidsEnhet'], sd, na.rm=T)
	Konf <- rbind(Midt - 2*SD/sqrt(Ngr), Midt + 2*SD/sqrt(Ngr))
}

if (length(KIekstrem) == 0) {	#Hvis ikke KIekstrem definert i variabeldefinisjonen
	KIekstrem <- c(0,max(RegData$Variabel, na.rm=T))
}
	Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
	Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])

#Resten (gruppa det sammenliknes mot)
MidtRest <- NULL
KonfRest <- NULL
if (medSml ==  1) {
Nrest <- tapply(RegData[ind$Rest ,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
	if (valgtMaal=='Med') {
		MedIQRrest <- plot(RegData$TidsEnhet[ind$Rest],RegData$Variabel[ind$Rest],  notch=TRUE, plot=FALSE)
		MidtRest <- as.numeric(MedIQRrest$stats[3, ])
		KonfRest <- MedIQRrest$conf
	} else {
	MidtRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], mean)	#ind$Rest
	SDRest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], sd)
	Nrest <- tapply(RegData[ind$Rest,'Variabel'], RegData[ind$Rest, 'TidsEnhet'], length)
	KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(Nrest), MidtRest + 2*SDRest/sqrt(Nrest))
	}
}

} #Sjekk av N
t1 <- switch(valgtMaal,
             Med = 'Median ',
             Gjsn = 'Gjennomsnittlig ')
tittel <- paste0(t1, NIRVarSpes$tittel) 

if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnitt '}

ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
rownames(ResData) <- c(maaltxt, 'KImin', 'KImaks', 
                       paste0(maaltxt, 'Resten'), 'KImin, Resten', 'KImaks, Resten')[1:(3*(medSml+1))]
#UtData <- list(paste0(toString(NIRVarSpes$tittel),'.'), ResData )
#names(UtData) <- c('tittel', 'Data')

FigDataParam <- list(AggVerdier=ResData, 
                     N=N, 
                     Ngr=Ngr,	
                     KImaal <- KImaal,
                     KImaaltxt <- KImaaltxt,
                     #soyletxt=soyletxt,
                     grtxt=levels(RegData$TidsEnhet),
                     #grtxt2=grtxt2, 
                     #varTxt=varTxt,
                     #tidtxt=tidtxt, #NIRVarSpes$grtxt,
                     tittel=NIRVarSpes$tittel, 
                     #retn='V', 
                    # xAkseTxt=xAkseTxt,
                     #yAkseTxt=yAkseTxt,
                     utvalgTxt=utvalgTxt, 
                     fargepalett=NIRUtvalg$fargepalett, 
                     medSml=medSml,
                     hovedgrTxt=NIRUtvalg$hovedgrTxt,
                     smltxt=NIRUtvalg$smltxt)



    #-----------Figur---------------------------------------
if (length(ind$Hoved)<10 | ((medSml == 1) & (length(ind$Rest) < 10))) {
rapFigurer::figtype(outfile)
	plot.new()
	title(main=tittel)
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
#	text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {

xmin <- min(tidNum)-0.5
xmax <- max(tidNum)+0.5
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
ytxt <- maaltxt #paste0(maaltxt, ytxt1, sep='')

#Plottspesifikke parametre:
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=NIRUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))	
	
farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]
# 
plot(tidNum,Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
		#cex=0.8, cex.lab=0.9, cex.axis=0.9,	
		ylab=c(ytxt,'med 95% konfidensintervall'), 
		xlab='Innleggelsestidspunkt', xaxt='n', 
		sub='(Tall i boksene angir antall innleggelser)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = tidNum, labels = levels(RegData$TidsEnhet))	

#Sammenlikning:
if (medSml==1) {
      # polygon( c(tidtxt[1]-0.01,tidtxt, tidtxt[AntTidsenh]+0.012, 
      #            tidtxt[AntTidsenh]+0.012, tidtxt[AntTidsenh:1], tidtxt[1]-0.01), 
      #          c(KonfRest[1,c(1,1:AntTidsenh, AntTidsenh)], KonfRest[2,c(AntTidsenh,AntTidsenh:1,1)]), 
      #          col=fargeRestRes, border=NA)
      AntTidsenh <- max(which(!is.na(KonfRest[1,])))
            #which(KonfRest==max(KonfRest, na.rm = T)) #Tar bare høyde for at siste tidspunkt har KonfRest=NA
            #length(min(tidNum, na.rm = T):max(tidNum, na.rm = T)) #length(tidNum)
      polygon( c(tidNum[1]-0.01,tidNum[1:AntTidsenh], tidNum[AntTidsenh]+0.012, 
				tidNum[AntTidsenh]+0.012, tidNum[AntTidsenh:1], tidNum[1]-0.01), 
		c(KonfRest[1,c(1,1:AntTidsenh, AntTidsenh)], KonfRest[2,c(AntTidsenh,AntTidsenh:1,1)]), 
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste0('95% konfidensintervall for ', NIRUtvalg$smltxt, ', N=', sum(Nrest, na.rm=T)))
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(Ngr, na.rm=T), cex=cexgr)/2	#length(tidNum)/30
rect(tidNum-b, Midt-h, tidNum+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(tidNum, Midt, Ngr, col=fargeHovedRes, cex=cexgr) 	

#KImål
lines(tidNum, rep(KImaal,length(tidNum)), col= '#FF7260', lwd=3)
mtext(text=KImaaltxt, at=KImaal, side=4, las=0, line=-1, cex=0.9, col='#FF7260') #adj=0.1,  
#mtext(text=KImaaltxt, at=KImaal, side=4, las=0, cex=0.9, col='#FF7260') #adj=0.1,  

#Konfidensintervall:
ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=tidNum, y0=Midt-h, x1=tidNum, length=0.08, code=2, angle=90, 
		y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=tidNum, y0=Midt+h, x1=tidNum, y1=replace(Konf[2, ], ind, Midt[ind]+h), 
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)

title(main=c(tittel, NIRUtvalg$hovedgrTxt), font.main=1, line=1)
#Tekst som angir hvilket utvalg som er gjort
if (length(utvalgTxt)>0) {
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))}

if ( outfile != '') {dev.off()}

}	#end if statement for 0 observations
return(invisible(FigDataParam))

}	#end function
