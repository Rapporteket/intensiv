#' Lager søylefigur
#'
#' Funksjonen genererer søylediagram basert på data som leveres funksjonen. Søylediagrammenes utseende
#' varierer noe for ulike datatyper, dvs. den kan vise både fordelingsfigur, AggVerdier eller sentralmål for 
#' gitt grupperingsvariabel, ...
#' 
#' #' Figurtypen som genereres er avhengig av valgtVar. Argumentet \emph{valgtVar} har følgende valgmuligheter:
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
#' @inheritParams NIRAndeler
#' @inheritParams NIRAndelerGrVar
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt 
#' @param AggVerdier Liste med AggVerdier i hver kategori (aldersgruppe, sykehus, ...). 
#'                AggVerdier$Hoved er enten andelene for hele utvalget i hver kategori eller f.eks. egen enhet
#'                AggVerdier$Rest er andelene for gruppa det sammenlignes mot.
#' @param AndelTot Andelen for hele gruppa (ett tall)
#' @param tittel Figurtittel
#' @param medSml Angir om vi skal gjøre sammenlikning eller ikke (verdier: 1/0)
#' @param smltxt Tekststreng som angir hva vi evt. sammenlikner med. Benyttes også til å angi 
#' 					gruppenavnet (i f.eks. tekstetikett) hvis det er gjort utvalg på f.eks. en sykehustype.
#' @param N Totalantall angitt i N$Hoved og N$Rest
#' @param retn Horisontale eller Vertikale søyler ('H', 'V')
#' @param yAkseTxt Tekst på y-aksen
#' @param utvalgTxt Tekst som angir hvilke utvalg som er gjort
#' @param grTypeTxt Tekst som angir
#' @param soyletxt Tekst PÅ hver søyle
#' @param cexgr Skalering av gruppetekst (trenger mindre font når flere grupper)
#' @param grtxt Navn på gruppene/kategoriene (Eks 40-50 år)
#' @param grtxt2 Evt. undertekst på kategoriene (Eks 28%) 
#' @param hovedgrTxt Angivelse av hovedgruppe, eks. eget sykehus
#' @param fargepalett 'BlaaOff' 
#' @param xAkseTxt tekst på x-aksen
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NIRFigSoyler <- function(RegData, AggVerdier, AndelTot=0, Ngr=0, tittel='mangler tittel', smltxt, N=0, retn='H', 
                         yAkseTxt='', utvalgTxt='', grTypeTxt='', soyletxt='',grtxt, grtxt2, hovedgrTxt='', 
                         grVar='', cexgr=1, medSml=0, fargepalett='BlaaOff', xAkseTxt='', outfile='') {

pktTxt <- '' #(evt. søyletekst)
txtEtiketter  <- ''	#legend
#verdier <- ''	#AggVerdier, gjennomsnitt, ...
verdiTxt <- '' 	#pstTxt, ...
if (valgtMaal %in% c('Med', 'Gjsn') ) {medKI <- 1} else {medKI <- 0}

#---------------------------------------FRA FIGANDELER og AndelGrVar--------------------------
#Hvis for få observasjoner..

if (dim(RegData)[1] < 10 | (max(Ngr$Hoved) < 10) | #Ngrense) | #Denne er skummel. Slår ut hvis <Ngrense i ei gruppe i fordelinga!!!
		(length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) {
	#-----------Figur---------------------------------------
	figtype(outfile)  #FigTypUt <- figtype(outfile)
	farger <- FigTypUt$farger
	plot.new()
	#title(paste0('variabel: ', valgtVar))
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	if (valgtMaal=='Med' & valgtVar=='SMR') {tekst <- 'Ugyldig parameterkombinasjon'
		} else {tekst <- 'For få registreringer'}
	text(0.5, 0.6, tekst, cex=1.2)
	if ( outfile != '') {dev.off()}
	
} else {
	
	
	
	#Plottspesifikke parametre:
	#Høyde må avhenge av antall grupper
	hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
	FigTypUt <- figtype(outfile, height=hoyde, fargepalett=fargepalett)	
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
	cexleg <- 0.9	#Størrelse på legendtekst
	
	
	#Horisontale søyler
if (retn == 'H') {
	  xmax <- min(max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.25, 100)	
	  if (valgtMaal %in% c('Gjsn','Med')) {
			xmax <- min(max(xmax, AggVerdier$KIned, AggVerdier$KIopp, AggVerdier$KIHele, na.rm=T), 100)  #min(1.1*max(c(Midt, KIned, KIopp, KIHele)), 1.5*max(Midt))
		medKI <- ifelse(valgtVar=='SMR', 0, 1) }

	  ymin <- 0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	  ymax <- 0.2+1.2*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt))

	  #Må def. pos først for å få strek for hele gruppa bak søylene
	  ### reverserer for å slippe å gjøre det på konf.int
	  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	                 border=NA, col.axis='white', col='white'))
					# col=farger[3]),  xlab='', las=1) 	
	  if (medKI == 1) {	#Legge på konf.int for hele populasjonen
	        #options(warn=-1)	#Unngå melding om KI med lengde 0
	        KIHele <- AggVerdier$KIHele
	        AntGr <- length(which(AggVerdier$Hoved>0))
	        indOK <- which(AggVerdier$Hoved>=0)
	        posKI <- pos[indOK]
	        
	        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), c(0, max(posKI)+min(posKI), max(posKI)+min(posKI),0), 
	                col=farger[4], border=farger[4])
	        #polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), c(0, max(posKI)+min(posKI), max(posKI)+min(posKI),0), 
	        #        col=farger[4], border=farger[4])
	  }
	  
	         
	if (grVar %in% c('ShNavn')) {	#Må si noe om den "gamle figurtypen"
	      #grtxt <- rev(grtxt)
	      grTypeTxt <- smltxt
	      mtext(at=max(pos)+0.35*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
	      
	      lines(x=rep(AndelTot, 2), y=c(0, max(pos)+0.55), col=farger[2], lwd=2.5)
	      text(x=0.02*max(AggVerdier$Hoved, na.rm=T), y=pos+0.1, rev(soyletxt), las=1, cex=cexgr, adj=0, col=farger[1])	#AggVerdier, hvert sykehus	
	      }

		barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab=xAkseTxt,
					 col=fargeHoved, border=NA, cex.names=cexgr, xlim=c(0, xmax), ylim=c(ymin,ymax))	

      if (valgtVar == 'SMR') {	#Skal ikke ha med konfidensintervall
      	  legend('top', c(paste0(grTypeTxt, 'sykehus, ', sprintf('%.2f', MidtHele), ', N=', N),
      					   '(uten reinnlagte og overflyttede pasienter)'),
      			 col=c(farger[2],NA), lwd=c(2,NA), bty='n')	
      }

		if (medKI == 1) {	#Legge på konf.int for hver enkelt gruppe/sykehus
		      arrows(x0=AggVerdier$Hoved, y0=posKI, x1=AggVerdier$KIopp, y1=posKI, 
		             length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
		      arrows(x0=AggVerdier$Hoved, y0=posKI, x1=AggVerdier$KIned, y1=posKI, 
		             length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
		}
		#------Tegnforklaring (legend)--------
		if (valgtMaal %in% c('Gjsn', 'Med')) { 
		      KItxt <- ifelse(medKI ==1, paste0('95% konf.int., ', grTypeTxt, 'sykehus (', 
		                                        sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'), NULL)
		      TXT <- c(paste0('totalt: ', sprintf('%.1f', AndelTot), ', N=', N), 
		               KItxt)
		      legend('top', TXT, fill=c('white', farger[4]),  border='white', lwd=2, 
		             col=c(farger[2], farger[4]), seg.len=0.6, merge=TRUE, bty='n')
		} else { #Går bare videre til neste vhis ikke valgtMaal er oppfylt
		      legend(x=xmax, y=1.02*ymax, xjust=1, yjust=0.5, col=farger[2], border=NA, lty=c(1,NA), lwd=3, 
		             bty='n', cex = 0.9,	#box.col='white')
		             paste0(grTypeTxt, 'sykehus: ', sprintf('%.1f', AndelTot), '%, N=', N$Hoved)
		      ) 
		}
		#--------------------------------------
		
		


      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
      
      #Fordelingsfigurer:
      if (grVar == '') {
      	if (medSml == 1) { #Legge på prikker for sammenlikning
      		  points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
      		  legend('top', c(paste0(hovedgrTxt, ' (N=', N$hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
      				 border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
      				 lwd=lwdRest, lty=NA, ncol=1)
      	} else {	
      		  legend('top', paste0(hovedgrTxt, ' (N=', N$hoved,')'), 
      				 border=NA, fill=fargeHoved, bty='n', ncol=1)
      	}
      }
      
      
      #mtext(paste0(t1, valgtVar, ben0), las=1, side=1, line=2) #cex=xkr, 
      #text(x=0.005*xmax, y=pos+0.1, las=1, cex=xkr, adj=0, col=farger[1], soyletxt
      #    c(sprintf(paste0('%.', AntDes,'f'), Midt[1:AntGr]), rep('',length(Ngr)-AntGr)))
      #mtext(at=pos+0.1, GrNavnSort, side=2, las=1, cex=cexGrNavn*xkr, adj=1, line=0.25)	
      
      }		#Slutt horisontale søyler
      	
	
	
	if (retn == 'V' ) {
		  #Vertikale søyler eller linje
		  ymax <- min(max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.25, 115)
		  pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab=yAkseTxt,	
						 sub=xAkseTxt,	col=fargeHoved, border='white', ylim=c(0, ymax))	
		  mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
		  mtext(at=pos, grtxt2, side=1, las=1, cex=0.9*cexgr, adj=0.5, line=1.5)
		  
		  if (medSml == 1) {
				points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
				legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
					   border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
					   lwd=lwdRest, ncol=2, cex=cexleg)
		  } else {	
				legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
					   border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		  }
	} 
	
  
				
	title(tittel, line=1) #line=1.5, cex.main=1.3)

	#Tekst som angir hvilket utvalg som er gjort
	avst <- 0.8
	utvpos <- 3	#Startlinje for teksten
	mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
	
	par('fig'=c(0, 1, 0, 1)) 
	if ( outfile != '') {dev.off()}
	
}  #Figur
} #Funksjon





      