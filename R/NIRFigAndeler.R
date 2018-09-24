#' Funksjon som beregner aggregerte verdier (andeler) for ulike variabler/variabelkombinasjoner
#'
#' Denne funksjonen beregner AggVerdier (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort. Kan trenge funksjonerne:
#' NIRUtvalgEnh (skal endre navn til NIRUtvalg når ferdig)
#' NIRFigSoyler
#' 
#' Funksjonen benytter funksjonene: NIRRegDataSQL, NIRPreprosess, NIRVarTilrettelegg, NIRUtvalgEnh
#' og NIRFigSoyler
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling, 10-årige grupper 
#'     \item ExtendedHemodynamicMonitoring: Hemodynamisk overvåkn.
#'     \item inklKrit: Andeler for de 5 inklusjonskriteriene
#'     \item InnMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'     \item isolering: Isolasjon, type
#'     \item isoleringDogn Isolasjon, varighet
#'     \item liggetid: Liggetid 
#'     \item NEMS: Skår for ressursbruk. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score)
#'     \item nyreBeh: Nyrebeh., type
#'     \item nyreBehTid: Nyrebeh., varighet
#'     \item PrimaryReasonAdmitted: Hovedårsak til intensivopphold
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item respiratortidNonInv: Respiratortid, ikke-invasiv
#'     \item respiratortidInvMoverf: Respiratortid, invasiv m/overf.
#'     \item respiratortidInvUoverf: Respiratortid, invasiv u/overf.
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'     \item spesTiltak: Spesielle tiltak
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
#'	  \item 8: Egen region mot resten [NB: Mangler pt. variabel for region]
#'    	}							
#'    				
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @inheritParams NIRUtvalgEnh
#' @inheritParams NIRVarTilrettelegg
#' @param figurtype Hvilken figurtype som ønskes ut: 
#'                 andel (fordelingsfigurer), 
#'                 andelGrVar (andel i hver kategori av grupperingsvariabel, eks. sykehus), 
#'                 andelTid (andel per tidsenhet, eks. år, måned), 
#'                 andelPP (andel før og etter), 
#'                 gjsnGrVar (sentralmål i hver kategori av grupperingsvariabel, eks. sykehus),
#'                 gjsnTid (sentralmål per tidsenhet, eks. år, måned)
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 110)
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
#'				alle: standard (alle andre verdier)
#' @param overfPas Overført under pågående intensivbehandling? 
#'				1 = Pasienten er ikke overført
#'				2 = Pasienten er overført
#' @param grType Gjør gruppeutvalg på sykehustype
#'                      1: lokal-/sentralsykehus
#'                      2: lokal-/sentralsykehus
#'                      3: regionsykehus
#'                      99: alle (standard)
#' @param lagFig Angir om figur skal lages eller ikke 0-ikke lag, 1-lag
#' 								
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export

NIRFigAndeler  <- function(RegData=0, valgtVar='Alder', datoFra='2011-01-01', datoTil='3000-12-31', aar=0, overfPas=0,
                        minald=0, maxald=110, erMann='',InnMaate='', dodInt='',outfile='', grType=99,  
                        preprosess=1, hentData=0, reshID=0, enhetsUtvalg=0, lagFig=1)	{
      
      
      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil) #minald=0, maxald=110, erMann='',InnMaate='', dodInt=''
      }
      
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøres dette i samledokumentet)
      if (preprosess){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      
      
 #     "%i%" <- intersect
      #--------------- Definere variable ------------------------------
      
      NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='andeler')
      RegData <- NIRVarSpes$RegData
      flerevar <- NIRVarSpes$flerevar
      
      
      NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, 
                                minald=minald, maxald=maxald, 
                                erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, 
                                reshID=reshID, grType=grType, enhetsUtvalg=enhetsUtvalg) #overfPas = overfPas,
      RegData <- NIRUtvalg$RegData
      utvalgTxt <- NIRUtvalg$utvalgTxt
      
      
      
      #--------------- Gjøre beregninger ------------------------------
      #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = 0, Rest =0)
      Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
      Ngr <- list(Hoved = 0, Rest =0)
      ind <- NIRUtvalg$ind
	variable <- NIRVarSpes$variable
      
      Ngr$Hoved <- switch(as.character(flerevar), 
                          '0' = table(RegData$VariabelGr[ind$Hoved]),
                          # '1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm=T))
                          '1' = apply(RegData[ind$Hoved,variable], MARGIN=2, 
                                      FUN=function(x) sum(x == 1, na.rm=T)))
      #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
     N$Hoved <- switch(as.character(flerevar), 
                        '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                  #      '1' = length(ind$Hoved)
                        '1' = apply(RegData[ind$Hoved,variable], MARGIN=2, 
                                 FUN=function(x) sum(x %in% 0:1, na.rm=T)))
          AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved
      
      if (NIRUtvalg$medSml==1) {
           Ngr$Rest <- switch(as.character(flerevar), 
                               '0' = table(RegData$VariabelGr[ind$Rest]),
                              # '1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm=T))
                               '1' = apply(RegData[ind$Rest,variable], MARGIN=2, 
                                           FUN=function(x) sum(x == 1, na.rm=T)))
            N$Rest <- switch(as.character(flerevar), 
                             '0' = sum(Ngr$Rest),	
                             '1' = apply(RegData[ind$Rest,variable], MARGIN=2, 
                                   FUN=function(x) sum(x %in% 0:1, na.rm=T)))
            AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
      }
      
      if(flerevar==1) {
            Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                                 min(N$Hoved[1]), 
                                 paste0(min(N$Hoved),'-',max(N$Hoved)))
            Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                                min(N$Rest[1]), 
                                paste0(min(N$Rest),'-',max(N$Rest)))
      } else {
            Nfig <- N}

      grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Hoved), '%') #paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      
      # grtxt2 <- paste0(paste0('(', sprintf('%.1f',Utdata$AggVerdier$Hoved), '%)'),
#                       paste0('\n(', sprintf('%.1f',Utdata$AggVerdier$Rest), '%)'))
      xAkseTxt <- NIRVarSpes$xAkseTxt
      yAkseTxt <- 'Andel opphold (%)'
      retn <- NIRVarSpes$retn
      tittel <- NIRVarSpes$tittel
      hovedgrTxt <- NIRUtvalg$hovedgrTxt
      medSml <- NIRUtvalg$medSml
      grtxt <- NIRVarSpes$grtxt
      cexgr <- NIRVarSpes$cexgr
      grTypeTxt <- NIRUtvalg$grTypeTxt
      smltxt <- NIRUtvalg$smltxt
      KImaal <- NIRVarSpes$KImaal
      fargepalett <- NIRUtvalg$fargepalett
      
      FigDataParam <- list(AggVerdier=AggVerdier, N=Nfig, 
                           Ngr=Ngr,	
                           KImaal <- NIRVarSpes$KImaal,
                           grtxt2=grtxt2, 
                           grtxt=grtxt,
                           grTypeTxt=grTypeTxt,
                           tittel=tittel, 
                           retn=retn, 
                           xAkseTxt=xAkseTxt,
                           yAkseTxt=yAkseTxt,
                           utvalgTxt=utvalgTxt, 
                           fargepalett=NIRUtvalg$fargepalett, 
                           medSml=medSml,
                           hovedgrTxt=hovedgrTxt,
                           smltxt=smltxt)
      
      
      if (lagFig == 1) {
            #cexgr <- 1-ifelse(AntGr>20, 0.25*AntGr/60, 0)
            # NIRFigSoyler(RegData, AggVerdier, Ngr, tittel=tittel, hovedgrTxt=hovedgrTxt, 
            #              smltxt=smltxt, grTypeTxt=grTypeTxt, Ngr = Ngr, KImaal=KImaal,
            #              N=Nfig, retn=retn, utvalgTxt=utvalgTxt, grtxt=grtxt, grtxt2=grtxt2, 
            #              medSml=medSml, cexgr=cexgr, xAkseTxt=xAkseTxt, yAkseTxt=yAkseTxt, 
            #              outfile=outfile, figurtype=figurtype)	
            # 
            # NIRFigSoyler <- function(RegData, AggVerdier, AggTot=0, Ngr, tittel='mangler tittel', smltxt='', N, retn='H', 
            #                          yAkseTxt='', utvalgTxt='', grTypeTxt='', soyletxt='', grtxt, grtxt2='', hovedgrTxt='', 
            #                          grVar='', valgtMaal='Andel', figurtype='', cexgr=1, medSml=0, fargepalett='BlaaOff', xAkseTxt='', 
            #                          medKI=0, KImaal = NA, KImaaltxt = '', outfile='') { #Ngr=list(Hoved=0)
            #       
            
            #---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
            #Hvis for få observasjoner..
            
            if ((Nfig$Hoved < 5) | (dim(RegData)[1]<5))
                  #| ((enhetsUtvalg %in% c(1,3)) & length(which(RegData$ReshId == reshID))<5)) #(dim(RegData)[1]-N$Hoved <5) )
                  #       if (dim(RegData)[1] < 10 | ((enhetsUtvalg %in% c(1,3)) & length(which(RegData$ReshId == reshID))<5) )
                  #|(grVar=='' & length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) 
            {
                  #-----------Figur---------------------------------------
                  FigTypUt <-figtype(outfile)  #FigTypUt <- figtype(outfile)
                  farger <- FigTypUt$farger
                  plot.new()
                  title(tittel)	#, line=-6)
                  legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
                  tekst <- 'For få registreringer i egen eller sammenligningsgruppe'
                  text(0.5, 0.6, tekst, cex=1.2)
                  if ( outfile != '') {dev.off()}
                  
            } else {
                  
                  
                  #Plottspesifikke parametre:
                  #Høyde må avhenge av antall grupper
                  hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
                  FigTypUt <- figtype(outfile, height=hoyde, fargepalett=fargepalett)	
                  #Tilpasse marger for å kunne skrive utvalgsteksten
                  NutvTxt <- length(utvalgTxt)
                  vmarg <- switch(retn, V=0.05, H=min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)))
                  #NB: strwidth oppfører seg ulikt avh. av device...
                  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
                  
                  
                  farger <- FigTypUt$farger
                  fargeHoved <- farger[1]
                  fargeRest <- farger[3]
                  graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
                  antGr <- length(grtxt)
                  lwdRest <- 3	#tykkelse på linja som repr. landet
                  cexleg <- 0.9	#Størrelse på legendtekst
                  
                  
                  
                  #Horisontale søyler
                  if (retn == 'H') {
                        #Definerer disse i beregningsfunksjonen?  
                        xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
                        #xmax <- ifelse(valgtMaal=='Andel', min(xmax, 100), xmax) 	#100 som maks bare hvis andelsfigur..
                        xmax <- min(xmax, 100)
                        ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
                        ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved) 
                        
                        #Må def. pos først for å få strek for hele gruppa bak søylene
                        ### reverserer for å slippe å gjøre det på konf.int
                        pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
                                           xlab=xAkseTxt, horiz=T, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
                        indOK <- which(AggVerdier$Hoved>=0)
                        posOK <- pos[indOK]
                        posOver <- max(pos)+0.35*log(max(pos))
                        posDiff <- 1.2*(pos[1]-pos[2])
                        posOK <- pos[indOK]
                        minpos <- min(posOK)-0.7
                        maxpos <- max(posOK)+0.7
                        
                        if (medSml == 1) { #Legge på prikker for sammenlikning
                              legend(xmax/4, posOver+0.6*posDiff, 
                                     c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), paste0(smltxt, ' (N=', Nfig$Rest,')')), 
                                     border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), 
                                     pt.cex=2, lwd=lwdRest, lty=NA, ncol=1)
                        } else {	
                              legend(xmax/4, posOver+0.6*posDiff, paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), 
                                     border=NA, fill=fargeHoved, bty='n', ncol=1)
                        }
                        
                        #Legge på gruppe/søylenavn
                        grtxt <- paste(grtxt, grtxt2, sep='\n')
                  
                  mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
                  
                  
                  #Fordelingsfigurer:
                  if (medSml == 1) { #Legge på prikker for sammenlikning
                        points(as.numeric(AggVerdier$Rest), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
                  }
            }		#Slutt horisontale søyler
            
            
            
            if (retn == 'V' ) {
                  #Vertikale søyler. Det er bare andeler som har vertikale søyler.
                  ymax <- min(max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.25, 115)
                  pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab=yAkseTxt,	
                                 sub=xAkseTxt,	col=fargeHoved, border='white', ylim=c(0, ymax))	
                  mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
                  mtext(at=pos, grtxt2, side=1, las=1, cex=0.8*cexgr, adj=0.5, line=1.5, col=graa[2])
                  mtext(at=0,  paste0(hovedgrTxt,': '), side=1, cex=0.8*cexgr, adj=0.9, line=1.5, col=graa[2])
                  #legend(x=0, y=-0.05*ymax, legend=paste0(hovedgrTxt,':'), col=fargeRest,pch=18,bty="n",ncol=2, cex=0.9*cexgr, xpd=TRUE) #pt.cex=0.7,
                  
                  if (medSml == 1) {
                        grtxt3 <- paste0(sprintf('%.1f',AggVerdier$Rest), '%') #paste0('(', sprintf('%.1f',AggVerdier$Rest), '%)')
                        mtext(at=pos, grtxt3, side=1, las=1, cex=0.8*cexgr, adj=0.5, line=2.5, col=graa[2])
                        mtext(at=0,  paste0(smltxt,': '), side=1, cex=0.8*cexgr, adj=0.9, line=2.5, col=graa[2])
                        points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
                        legend('top', legend=c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), paste0(smltxt, ' (N=', Nfig$Rest,')')), 
                               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
                               lwd=lwdRest, ncol=2, cex=cexleg)
                  } else {	
                        legend('top', legend=paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), 
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
            } #Nok observasjoner    
      }  #Figur
      
      
      return(invisible(FigDataParam))
      
}