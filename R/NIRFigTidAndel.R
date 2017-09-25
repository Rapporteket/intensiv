#' Lager tidslinjeplott
#'
#' Funksjonen genererer linjediagram basert p? data som leveres funksjonen. Linjediagrammenes utseende
#' kan variere for ulike datatyper. Forel?pig (2017-05-22) viser den utvikling over tid for andeler.
#' 
#' Detajer: Her b?r man liste opp hvilke variable funksjonen benytter.
#'
#' @inheritParams NIRAndeler
#' @inheritParams NIRAndelTid
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt 
#' @param AggVerdier Liste med AggVerdier i hver kategori (aldersgruppe, sykehus, ...). 
#'                AggVerdier$Hoved er enten andelene for hele utvalget i hver kategori eller f.eks. egen enhet
#'                AggVerdier$Rest er andelene for gruppa det sammenlignes mot.
#' @param AggTot Aggregert verdi for hele gruppa (ett tall)
#' @param tittel Figurtittel
#' @param medSml Angir om vi skal gj?re sammenlikning eller ikke (verdier: 1/0)
#' @param smltxt Tekststreng som angir hva vi evt. sammenlikner med. Benyttes ogs? til ? angi 
#' 					gruppenavnet (i f.eks. tekstetikett) hvis det er gjort utvalg p? f.eks. en sykehustype.
#' @param N Totalantall angitt i N$Hoved og N$Rest
#' @param Ngr Antall i hver gruppe
#' @param KImaal Kvalitetsindikatorm?l
#' @param retn Horisontale eller Vertikale s?yler ('H', 'V')
#' @param yAkseTxt Tekst p? y-aksen
#' @param utvalgTxt Tekst som angir hvilke utvalg som er gjort
#' @param grTypeTxt Tekst som angir
#' @param cexgr Skalering av gruppetekst (trenger mindre font n?r flere grupper)
#' @param grtxt Navn p? gruppene/kategoriene (Eks 40-50 ?r)
#' @param grtxt2 Evt. undertekst p? kategoriene (Eks 28pst) 
#' @param hovedgrTxt Angivelse av hovedgruppe, eks. eget sykehus
#' @param fargepalett 'BlaaOff' 
#' @param xAkseTxt tekst p? x-aksen
#'
#' @return S?ylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


NIRFigTidAndel <- function(RegData, AggVerdier, AggTot=0, Ngr, tittel='mangler tittel', smltxt='', N, retn='H', 
                         yAkseTxt='', utvalgTxt='', grTypeTxt='', tidtxt, varTxt='', grtxt2='', hovedgrTxt='', 
                         valgtMaal='Andel', cexgr=1, medSml=0, fargepalett='BlaaOff', xAkseTxt='', 
                         medKI=0, KImaal = NA, outfile='') { #Ngr=list(Hoved=0), grVar='', 

     	#-----------Figur---------------------------------------
  #Hvis for f? observasjoner..
  if (N$Hoved < 10 | (medSml ==1 & N$Rest<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste0('variabel: ', valgtVar))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'F?rre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}

	} else {
    
    #Plottspesifikke parametre:
    FigTypUt <- figtype(outfile, fargepalett=fargepalett)
    farger <- FigTypUt$farger
    fargeHoved <- farger[3]
    fargeRest <- farger[1]
    NutvTxt <- length(utvalgTxt)
    hmarg <- 0.04+0.01*NutvTxt
    par('fig' = c(0,1,0,1-hmarg)) 
    cexleg <- 1	#St?rrelse p? legendtekst
    ylabtext <- "Andel (%)"
    xskala <- 1:length(tidtxt)
    xmax <- max(xskala)
      
    
    ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
    plot(xskala, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o', 
         xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
         cex=2, xlab='Innleggelsesår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	
    
    #Legge på linjer i plottet. 
    grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")
    
    axis(side=1, at = xskala, labels = tidtxt)
    
    title(tittel, line=1, font.main=1)
    
    
    lines(xskala, AggVerdier$Hoved, col=fargeHoved, lwd=3)
    points(xskala, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
    text(xskala, AggVerdier$Hoved, pos=3, Ngr$Hoved, cex=0.9, col=fargeHoved)
    
    lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
    points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)
    
    #KImål
    lines(xskala,rep(KImaal,length(xskala)), col= '#FF7260', lwd=3)
    text(max(xskala), KImaal, pos=4, 'Mål', cex=0.9, col='#FF7260')
    
    Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')') 
    if (medSml == 1) { 
      text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                     paste0(smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1, cex=cexleg, 
             col=c(fargeHoved, fargeRest, NA), lwd=3)		
    } else {
      legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt), 
             col=c(fargeHoved, NA), lwd=3, bty='n')
    }
    
    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))
    
    par('fig'=c(0, 1, 0, 1)) 
    if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------
    
  }	#end else statement
}