#' Tidstrend av andel pasienter
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år 
#'     \item alder_over80: Pasienter over 80 år  (>80)
#'     \item dodeSykehus: Pasienter som dør under sykehusoppholdet (intensiv/post)
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga. 
#'		\item liggetidDod: Andel av total liggetid brukt på de som dør på intensiv
#'     \item respiratortidDod: Respiratortid brukt på de som dør på intensiv
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item reinn: Andel reinnlagte (kun hvor dette er registrert, dvs. fjerner ukjente)
#'    }
#'
#' @inheritParams NIRFigAndeler 
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år. 
#'
#' @export
NIRFigAndelTid <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', 
                        minald=0, maxald=130, erMann='', InnMaate='', dodInt='', reshID, outfile='', 
                        enhetsUtvalg=1, preprosess=1, hentData=0) {
  
  if (hentData == 1) {		
    RegData <- NIRRegDataSQL(datoFra, datoTil)
  }
  
  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess){
    RegData <- NIRPreprosess(RegData=RegData, reshID=reshID)
  }
  
  
  RegData$Aar <- 1900 + strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
  RegData$Variabel <- 0
  
  
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

#  indEgen1 <- match(reshID, RegData$ReshId)
#  if (enhetsUtvalg %in% c(1,2,6)) {	#Involverer egen enhet
#    shtxt <- as.character(RegData$Avdeling[indEgen1]) } else {
#     shtxt <- switch(as.character(enhetsUtvalg), 	
#                     '0' = 'Hele landet',
#                      '7' = as.character(RegData$Region[indEgen1]),
#                     '8' = as.character(RegData$Region[indEgen1]))
#   }
  
if (valgtVar=='alder_u18') {
  RegData <- RegData[which(RegData$alder>=0), ]    #Tar bort alder<0
  RegData$Variabel[which(RegData$alder<18)] <- 1 
  VarTxt <- 'under 18 år'
  Tittel <- 'Andel under 18 år'
}

if (valgtVar=='alder_over80') {
  RegData <- RegData[which(RegData$alder>=0), ]    #Tar bort alder<0
  RegData$Variabel[which(RegData$alder>=80)] <- 1 
  VarTxt <- 'over 80 år'
  Tittel <- 'Andel over 80 år'
}

if (valgtVar=='dodeSykehus') {
  RegData <- RegData[which(RegData$DischargedHospitalStatus %in% 0:2), ]  	#Tar bort ukjente og de som ikke er utskrevet
  RegData$Variabel[which(RegData$DischargedHospitalStatus!=0)] <- 1 
  VarTxt <- 'pasienter som døde'
  Tittel <- 'Andel pasienter som døde under sykehusoppholdet'
}

if (valgtVar=='dodeIntensiv') {
  RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]		#Tar bort ukjente
  RegData$Variabel[which(RegData$DischargedIntensiveStatus==1)] <- 1 
  VarTxt <- 'pasienter som døde på intensiv'
  Tittel <- 'Andel pasienter som døde på intensiv'
}

if (valgtVar=='reinn') {
  RegData <- RegData[which(RegData$Reinn %in% 1:2), ]  	#Tar bort ukjente
  RegData$Variabel[which(RegData$Reinn==1)] <- 1 
  VarTxt <- 'reinnleggelser'
  Tittel <- 'Andel reinnleggelser'
}

if (valgtVar=='liggetidDod') {
  RegData <- RegData[which(RegData$liggetid>=0), ]    #Tar bort liggetid<0 samt NA
  RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]  	#Tar bort ukjente  
  RegData$Variabel<-RegData$liggetid
  RegData$Variabel2<- as.numeric(RegData$DischargedIntensiveStatus)*RegData$liggetid
  VarTxt <- 'liggedøgn for døde'
  Tittel <- 'Andel av total liggetid brukt på dem som dør på intensiv'
}

if (valgtVar=='respiratortidDod') {
  RegData <- RegData[which(RegData$respiratortid>=0), ]    #Tar bort respiratortid<0 samt NA
  RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]    #Tar bort ukjente  
  RegData$Variabel<-RegData$respiratortid
  RegData$Variabel2<-as.numeric(RegData$DischargedIntensiveStatus)*RegData$respiratortid
  VarTxt <- 'liggedøgn for døde'
  Tittel <- 'Andel av total respiratortid brukt på dem som dør på intensiv'
}


  if (valgtVar=='respStotte') {
        RegData <- RegData[which(RegData$MechanicalRespirator %in% 1:2), ]
        RegData$Variabel[which(RegData$MechanicalRespirator==1)] <- 1 
        VarTxt <- 'pasienter med respiratorstøtte'
        Tittel <-'Andel med respiratorstøtte'
  }
  
  
#if (valgtVar == 'SMR') {
#	minald <- max(minald,18)	#Tatt ut ifm. at utvalg gjøres.
#  #Tar ut reinnlagte og overflyttede, samt de med SAPSII=0 (ikke scorede)
 # RegData <- RegData[RegData$DischargedHospitalStatus!= 3, ]
 # RegData <- RegData[RegData$Overf==1, ] 
 # RegData <- RegData[as.numeric(RegData$SAPSII) > 0, ]
#NB: Ny variabel. DischargedHospitalStatus ikke lenger i bruk...............
# RegData$Variabel[which(RegData$DischargedHospitalStatus!=0)] <- 1 
#  VarTxt <- 'pasienter over 18 år som døde'
#  Tittel <- 'SMR (uten overflyttede og reinnlagte pasienter)'  
#}
  
  
  #-------------------------Forberedelse...-----------------------------------------
  #Gjør utvalg
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

    
    NHovedRes <- length(indHoved)
    NSmlRes <- length(indRest)
    
    
    #-------------------------Beregning av andel-----------------------------------------
    Aartxt <- min(RegData$Aar):max(RegData$Aar)
    RegData$Aar <- factor(RegData$Aar, levels=Aartxt)
    
    NAarRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest], length)	
    NAarHendRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest],sum, na.rm=T)
    AndelRest <- NAarHendRest/NAarRest*100
    NAarHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'], length)
    NAarHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'],sum, na.rm=T)
    AndelHoved <- NAarHendHoved/NAarHoved*100
    Andeler <- rbind(AndelRest, AndelHoved)

if (valgtVar %in% c('liggetidDod','respiratortidDod')) {
	NAarHendRest<-NAarRest      #Komment: for liggetid og respiratortid er det mer naturlig å vise total antall pasienter instedet før total antall 
	NAarHendHoved<-NAarHoved    #         liggetid i døgn, navnene blir litt villedende men enklest å gjøre dette på denne måten 
	SUMAarRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest], sum,na.rm=T)  
	SUMAarHendRest <- tapply(RegData$Variabel2[indRest], RegData$Aar[indRest],sum, na.rm=T)
	AndelRest <- SUMAarHendRest/SUMAarRest*100
	SUMAarHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'], sum,na.rm=T)
	SUMAarHendHoved <- tapply(RegData[indHoved, 'Variabel2'], RegData[indHoved ,'Aar'],sum, na.rm=T)
	AndelHoved <- SUMAarHendHoved/SUMAarHoved*100
	Andeler <- rbind(AndelRest, AndelHoved) 
}  
    
#    if (valgtVar == 'SMR') {
#	  meanSAPS2scoreRest<-tapply(RegData$SMR[indRest], RegData$Aar[indRest], mean) #I NIRUtvalg ser man att Saps2Score kalles for SMR
#	  meanSAPS2scoreHoved<-tapply(RegData$SMR[indHoved], RegData$Aar[indHoved], mean)
#	  AndelRest<-AndelRest/meanSAPS2scoreRest #=andel døde på enheten/gjennomsnittlig Saps2Score på enheten
#	  AndelHoved<-AndelHoved/meanSAPS2scoreHoved
#	  Andeler <- rbind(AndelRest, AndelHoved)
#	}


	#-----------Figur---------------------------------------
  #Hvis for få observasjoner..
  if (length(indHoved) < 10 | (medSml ==1 & length(indRest)<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    
    #Plottspesifikke parametre:
    FigTypUt <- figtype(outfile, fargepalett=NIRUtvalg$fargepalett)
    farger <- FigTypUt$farger
    fargeHoved <- farger[3]
    fargeRest <- farger[1]
    NutvTxt <- length(utvalgTxt)
    hmarg <- 0.04+0.01*NutvTxt
    par('fig' = c(0,1,0,1-hmarg)) 
    cexleg <- 1	#Størrelse på legendtekst
    ylabtext="Andel (%)"

    
    ymax <- min(119, 1.25*max(Andeler,na.rm=T))
    plot(Aartxt, AndelHoved,  font.main=1,  type='o', pch="'", col='white', #type='o', 
         xlim= c(Aartxt[1], max(Aartxt)), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(Aartxt), max(Aartxt),length(Aartxt)-1)
         cex=2, xlab='Innleggelsesår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	
    
    #plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
    #		#cex=0.8, cex.lab=0.9, cex.axis=0.9,	
    #		ylab=c(ytxt,'med 95% konfidensintervall'), 
    #		xlab='Operasjonsår', xaxt='n', 
    #		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
    axis(side=1, at = Aartxt)	
    
    title(Tittel, line=1, font.main=1)
    
    #Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
    if ((ymax > 10) & (ymax < 40)) {lines(range(Aartxt),rep(10,2), col=farger[4])}
    if (ymax > 20) {lines(range(Aartxt),rep(20,2), col=farger[4])}
    if ((ymax > 30) & (ymax < 40)) {lines(range(Aartxt),rep(30,2), col=farger[4])}
    if (ymax > 40) {lines(range(Aartxt),rep(40,2), col=farger[4])}
    if (ymax > 60) {lines(range(Aartxt),rep(60,2), col=farger[4])}
    if (ymax > 80) {lines(range(Aartxt),rep(80,2), col=farger[4])}
    if (ymax > 100) {lines(range(Aartxt),rep(100,2), col=farger[4])}
    #		axis(2, at=c(0,20,40,60,80,100), pos=0),
    
    
    lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
    points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
    text(Aartxt, AndelHoved, pos=3, NAarHendHoved, cex=0.9, col=fargeHoved)
    
    lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
    points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}
    
    Ttxt <- paste('(Tall ved punktene angir antall ', VarTxt, ')', sep='') 
    if (medSml == 1) { 
      text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''),
                                     paste(smltxt, ' (N=', NSmlRes, ')', sep=''), Ttxt), bty='n', ncol=1, cex=cexleg, 
             col=c(fargeHoved, fargeRest, NA), lwd=3)		
    } else {
      legend('top', c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''), Ttxt), 
             col=c(fargeHoved, NA), lwd=3, bty='n')
    }
    
    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))
    
    par('fig'=c(0, 1, 0, 1)) 
    if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------
    
  }	#end else statement
}	#end function



