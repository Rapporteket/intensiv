#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NIRAndeler
#' @inheritParams NIRAndelerGrVar
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


NIRGjsnGrVar <- function(RegData, valgtVar, preprosess=1, hentData=0, valgtMaal='Gjsn', 
                  minald=0, maxald=130, datoFra='2011-01-01', datoTil='3000-01-01', aar=0,
                  grType=99, InnMaate=99, dodInt='', erMann='', grVar='ShNavn', medKI=1, 
                  lagFig=1, outfile) {
      
      
if (hentData == 1) {		
  RegData <- NIRRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- NIRPreprosess(RegData=RegData) #, reshID=reshID)
     }

#------- Tilrettelegge variable
NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
RegData <- NIRVarSpes$RegData

#------- Gjøre utvalg
NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, minald=minald, maxald=maxald, 
                          erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, grType=grType) #overfPas=overfPas, 
RegData <- NIRUtvalg$RegData
utvalgTxt <- NIRUtvalg$utvalgTxt

Ngrense <- 10		
'%i%' <- intersect

RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
#Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
#grupper komme med uansett om de ikke har registreringer.

if(dim(RegData)[1]>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}


t1 <- switch(valgtMaal,
			Med = 'Median ',
			Gjsn = 'Gjennomsnittlig ')


#tittel <- paste0(t1, valgtVar, ', ', NIRUtvalg$grTypeTxt, 'sykehus')
tittel <- paste0(t1, NIRVarSpes$tittel) 
			
if( valgtVar =='SMR') {tittel <- c(paste0('SMR, ', NIRUtvalg$grTypeTxt, 'sykehus'),
								'(uten reinnlagte og overflyttede pasienter)')}

Ngrtxt <- paste0(' (', as.character(Ngr),')') 
indGrUt <- which(Ngr < Ngrense)
if (length(indGrUt)==0) { indGrUt <- 0}
Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')	
N <- dim(RegData)[1]

KIHele <- c(0,0)    
KIned <- c(0,0)
KIhele <- c(0,0)
		

dummy0 <- NA #-0.0001
#Kommer ut ferdig sortert!
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
	MedIQR$stats[ ,indGrUt] <- dummy0
	MedIQR$conf[ ,indGrUt] <- dummy0
	sortInd <- order( MedIQR$stats[3,], decreasing=NIRVarSpes$sortAvtagende, na.last = FALSE) 
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
	} 
	
if (valgtMaal=='Gjsn') {	#Gjennomsnitt er standard, men må velges.
	if (valgtVar=='SMR') { #Bør tas ut av Gjsn...?
		medKI <- 0
		ObsGr <- tapply(RegData$Dod30, RegData[ ,grVar], mean, na.rm=T)
		EstGr <- tapply(RegData$SMR, RegData[ ,grVar], mean, na.rm=T)
		ind0 <- which(EstGr == 0)
		Gjsn <- 100*ObsGr/EstGr  
		if (length(ind0)>0) {Gjsn[ind0] <- 0}#Unngå å dele på 0
		#Vi benytter ikke konf.int for SMR. Setter alle SE lik 0
		     #TestPoGr <- which((Ngr*ObsGr-3*sqrt(Ngr*ObsGr*(1-ObsGr)) <= 0) | (Ngr*ObsGr+3*sqrt(Ngr*ObsGr*(1-ObsGr)) > Ngr))
		     #SE <- sqrt(ObsGr*(1-ObsGr))*100/(sqrt(Ngr)*EstGr)
		     #if (length(TestPoGr)>0) {SE[TestPoGr] <- 0}
		SE <- rep(0, length(Ngr))
		Obs <-  mean(RegData$Dod30)	#Kun 0 og 1
		Est <- mean(RegData$Variabel, na.rm=T)
		MidtHele <- ifelse(Est ==0, 0, 100*Obs/Est)
	} else {
	      Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
		SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
		MidtHele <- mean(RegData$Variabel)	#mean(RegData$Variabel)
		KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
	}
      
	Gjsn[indGrUt] <- dummy0
	SE[indGrUt] <- 0
	sortInd <- order(Gjsn, decreasing=NIRVarSpes$sortAvtagende, na.last = FALSE) 
	Midt <- Gjsn[sortInd] #as.numeric(Gjsn[sortInd])
	KIned <- Gjsn[sortInd] - 2*SE[sortInd]
	KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
	}


#if (sum(which(Ngr < Ngrense))>0) {indGrUt <- as.numeric(which(Ngr<Ngrense))} else {indGrUt <- 0}
#AndelerGr[indGrUt] <- -0.0001

GrNavnSort <- paste0(names(Ngr)[sortInd], Ngrtxt[sortInd])
if (valgtVar == 'SMR') {AntDes <- 2} else {AntDes <- 1} 
soyletxt <- sprintf(paste0('%.', AntDes,'f'), Midt) 
#soyletxt <- c(sprintf(paste0('%.', AntDes,'f'), Midt[1:AntGr]), rep('',length(Ngr)-AntGr))
indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
soyletxt[indUT] <- ''
KIned[indUT] <- NA
KIopp[indUT] <- NA

AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
Ngr <- list(Hoved=Ngr[sortInd], Rest=0)


#Se NIRFigSoyler for forklaring av innhold i lista GjsnGrVarData
GjsnGrVarData <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                         N=list(Hoved=N), 
                         Ngr=Ngr,
                         grtxt2='', 
				 medKI=medKI,
				 KImaal = NIRVarSpes$KImaal,
                         soyletxt=soyletxt,
                         grtxt=GrNavnSort,
				 valgtMaal=valgtMaal,
                         tittel=tittel,    #NIRVarSpes$tittel, 
                         #yAkseTxt=yAkseTxt, 
                         retn='H', 
                         xAkseTxt=NIRVarSpes$xAkseTxt,
                         grTypeTxt=NIRUtvalg$grTypeTxt,			 
                         utvalgTxt=NIRUtvalg$utvalgTxt, 
                         fargepalett=NIRUtvalg$fargepalett, 
                         medSml=NIRUtvalg$medSml, 
                         smltxt=NIRUtvalg$smltxt)


#FigDataParam skal inn som enkeltparametre i funksjonskallet
if (lagFig == 1) {
      cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
      NIRFigSoyler(RegData, AggVerdier=AggVerdier, AggTot=MidtHele, Ngr=Ngr, N=list(Hoved=N), cexgr=cexgr, 
                   tittel=tittel, valgtMaal=valgtMaal,
                   smltxt=NIRUtvalg$smltxt, yAkseTxt=yAkseTxt,utvalgTxt=NIRUtvalg$utvalgTxt, 
                   grTypeTxt=NIRUtvalg$grTypeTxt,  fargepalett=NIRUtvalg$fargepalett, grtxt=GrNavnSort, 
                   soyletxt=soyletxt,  grVar=grVar, medKI=medKI, KImaal = NIRVarSpes$KImaal,
                   medSml=NIRUtvalg$medSml, xAkseTxt=NIRVarSpes$xAkseTxt, outfile=outfile)
}

return(invisible(GjsnGrVarData))

}



