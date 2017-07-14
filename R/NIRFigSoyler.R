#' Lager søylefigur
#'
#' Funksjonen genererer søylediagram basert på data som leveres funksjonen. Søylediagrammenes utseende
#' varierer noe for ulike datatyper, dvs. den kan vise både fordelingsfigur, AggVerdier eller sentralmål for 
#' gitt grupperingsvariabel, ...
#' 
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @inheritParams NIRAndeler
#' @inheritParams NIRAndelerGrVar
#' @inheritParams NIRGjsnGrVar
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt 
#' @param AggVerdier Liste med AggVerdier i hver kategori (aldersgruppe, sykehus, ...). 
#'                AggVerdier$Hoved er enten andelene for hele utvalget i hver kategori eller f.eks. egen enhet
#'                AggVerdier$Rest er andelene for gruppa det sammenlignes mot.
#' @param AggTot Aggregert verdi for hele gruppa (ett tall)
#' @param tittel Figurtittel
#' @param medSml Angir om vi skal gjøre sammenlikning eller ikke (verdier: 1/0)
#' @param smltxt Tekststreng som angir hva vi evt. sammenlikner med. Benyttes også til å angi 
#' 					gruppenavnet (i f.eks. tekstetikett) hvis det er gjort utvalg på f.eks. en sykehustype.
#' @param N Totalantall angitt i N$Hoved og N$Rest
#' @param Ngr Antall i hver gruppe
#' @param KImaal Kvalitetsindikatormål
#' @param retn Horisontale eller Vertikale søyler ('H', 'V')
#' @param yAkseTxt Tekst på y-aksen
#' @param utvalgTxt Tekst som angir hvilke utvalg som er gjort
#' @param grTypeTxt Tekst som angir
#' @param soyletxt Tekst PÅ hver søyle
#' @param cexgr Skalering av gruppetekst (trenger mindre font når flere grupper)
#' @param grtxt Navn på gruppene/kategoriene (Eks 40-50 år)
#' @param grtxt2 Evt. undertekst på kategoriene (Eks 28pst) 
#' @param hovedgrTxt Angivelse av hovedgruppe, eks. eget sykehus
#' @param fargepalett 'BlaaOff' 
#' @param xAkseTxt tekst på x-aksen
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NIRFigSoyler <- function(RegData, AggVerdier, AggTot=0, Ngr, tittel='mangler tittel', smltxt='', N, retn='H', 
                         yAkseTxt='', utvalgTxt='', grTypeTxt='', soyletxt='', grtxt, grtxt2='', hovedgrTxt='', 
                         grVar='', valgtMaal='Andel', cexgr=1, medSml=0, fargepalett='BlaaOff', xAkseTxt='', 
                         medKI=0, KImaal = NA, outfile='') { #Ngr=list(Hoved=0)


#---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
#Hvis for få observasjoner..

if (dim(RegData)[1] < 10 )
    #|(grVar=='' & length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) 
    {
	#-----------Figur---------------------------------------
      FigTypUt <-figtype(outfile)  #FigTypUt <- figtype(outfile)
	farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	if (valgtMaal=='Med' & grepl('SMR', tittel)) {tekst <- 'Ugyldig parameterkombinasjon'   #valgtVar=='SMR'
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
	vmarg <- switch(retn, V=0, H=min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)))
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
	#Definerer disse i beregningsfunksjonen?  
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- ifelse(valgtMaal=='Andel', min(xmax, 100), xmax) 	#100 som maks bare hvis andelsfigur..
	  ymin <- 0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	  ymax <- 0.2+1.2*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt))

	  #Må def. pos først for å få strek for hele gruppa bak søylene
	  ### reverserer for å slippe å gjøre det på konf.int
	  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	                     xlab=xAkseTxt, border=NA, col.axis='white', col='white'))
	  indOK <- which(AggVerdier$Hoved>=0)
	  posOK <- pos[indOK]
	  posOver <- max(pos)+0.35*log(max(pos))
	  posDiff <- 1.2*(pos[1]-pos[2])
	  posOK <- pos[indOK]
	  minpos <- min(posOK)-0.7
	  maxpos <- max(posOK)+0.7
	  
	  if (medKI == 1) {	#Legge på konf.int for hele populasjonen
	        #options(warn=-1)	#Unngå melding om KI med lengde 0
	        KIHele <- AggVerdier$KIHele
	        AntGr <- length(which(AggVerdier$Hoved>0))
	        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), col=farger[3], border=farger[3],
	                c(minpos, maxpos, maxpos, minpos))
	  }

	if (grVar %in% c('ShNavn')) {	#Må si noe om den "gamle figurtypen"
	      #grtxt <- rev(grtxt)
	      grTypeTxt <- smltxt
	      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
	      #Linje for hele landet/utvalget:
	      lines(x=rep(AggTot, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55), 
	      #Linje for kvalitetsindikatormål:
	      if (!is.na(KImaal)) { 
	            lines(x=rep(KImaal, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55), 
	            text(x=KImaal, y=maxpos+0.6, 'Mål', cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0)) 
	      }
	      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
	              col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
	      soyleXpos <- 1.1*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
	      text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus
	      }


	if (medKI == 1) {	#Legge på konf.int for hver enkelt gruppe/sykehus
	      arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIopp, y1=pos, 
	             length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	      arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIned, y1=pos, 
	             length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	}
	#------Tegnforklaring (legend)--------
	if (valgtMaal %in% c('Gjsn', 'Med')) { #Sentralmålfigur
	      if (medKI == 0) { #Hopper over hvis ikke valgtMaal er oppfylt
	            TXT <- paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved)
      	      legend(xmax/4, posOver+posDiff, TXT, fill=NA,  border=NA, lwd=2.5, xpd=TRUE, #inset=c(-0.1,0),
      	             col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
	      } else {
	            TXT <- c(paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved), 
	                     paste0('95% konf.int., ', grTypeTxt, 'sykehus (', 
	                            sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'))
      	      legend(xmax/4, posOver+2*posDiff, TXT, fill=c(NA, farger[3]),  border=NA, lwd=2.5,  #inset=c(-0.1,0),
      	             col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
	      }
	} else { 
	      legend(xmax/4, posOver+2.5*posDiff, paste0(grTypeTxt, 'sykehus: ', sprintf('%.1f', AggTot), '%, N=', N$Hoved),
	             col=farger[1], border=NA, lwd=2.5, xpd=TRUE, bty='n', cex = cexleg) 
	}
	  #Fordelingsfigurer:
	  if (grVar == '') {
      	  if (medSml == 1) { #Legge på prikker for sammenlikning
      	        legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
      	               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
      	               lwd=lwdRest, lty=NA, ncol=1)
      	  } else {	
      	        legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
      	               border=NA, fill=fargeHoved, bty='n', ncol=1)
      	  }
	  }
	#--------------------------------------


      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
      
      #Fordelingsfigurer:
      #if (grVar == '') {
      	if (medSml == 1) { #Legge på prikker for sammenlikning
      		  points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
      	}
       #}
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
				legend('top', legend=c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
					   border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
					   lwd=lwdRest, ncol=2, cex=cexleg)
		  } else {	
				legend('top', legend=paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
					   border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		  }
	} 
	
  
				
	title(tittel, line=1.5) #cex.main=1.3)

	#Tekst som angir hvilket utvalg som er gjort
	avst <- 0.8
	utvpos <- 3	#Startlinje for teksten
	mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
	
	par('fig'=c(0, 1, 0, 1)) 
	if ( outfile != '') {dev.off()}
	
}  #Figur
} #Funksjon





      