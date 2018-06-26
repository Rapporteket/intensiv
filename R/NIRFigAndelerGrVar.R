#' Søylediagram med AggVerdier for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med AggVerdier av en gitt variabel for ei valgt gruppering, 
#' f.eks. enheter. I øyeblikket benytter funksjonen bare 'ShNavn' som grupperingsvariabel, men 
#' andre valg kan lett inkluderes. Funksjonen er tilrettelagt for også å kunne benytte "01-data", 
#' dvs. kodede, anonymiserte data som benyttes til offentlige kvalitetsindikatorer.
#'
#' Funksjonen benytter funksjonene: NIRRegDataSQL, NIRPreprosess, NIRVarTilrettelegg, NIRUtvalgEnh
#' og NIRFigSoyler.
#' Figurtypen som genereres er avhengig av valgtVar. Ved valgtVar='InnMaate', får man ei oversikt oversikt
#' over andel av innkomsttype ved hver enhet, dvs. ei fordeling av de tre innkomsttypene for hver enhet.
#' For andre "valgtVar" viser figuren andel av den valgte variabelen for hver enhet.
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år 
#'     \item alder_over80: Pasienter over 80 år (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga. 
#'     \item ExtendedHemodynamicMonitoring: Utvidet hemodyn. overvåkning
#'     \item innMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'		Dette valget viser en annen figurtype.
#'	 \item isolering: Isolasjon av pasient
#'	 \item liggetidDod: Liggetid, døde
#'	 \item nyreBeh: Nyrebehandling
#'     \item reinn: Andel reinnlagte (fjerner ukjente) Kvalitetsindikator
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item respiratortidInvMoverf: Respiratortid, inv. < 2,5d m/overføringer
#'     \item respiratortidInvUoverf: Respiratortid, inv. < 2,5d u/overføringer
#'     \item respiratortidDod: Respiratortid, døde
#'     \item Trakeostomi: Andel som har fått trakeostomi (kat 2 og 3)
#'     \item trakAapen: Trakeostomi, åpen
#'    }
#' @inheritParams NIRFigAndeler
#' @param aldGr: Aldersgrupper. Brukes i offentliggjøringsfigurer
#' @param tittel: Hvis vil angi tittel direkte
#' @param utvalgsInfo: Hvis datafil lagret med utvalgsinfo
#' @param offData: Hvis vi benytter anonymiserte 01-data til offentliggjøring.
#' @param sortAvtagende: sortere søylene i figuren avtagende eller stigende.
#' 
#' @return Søylediagram med AggVerdier av valgt variabel for hvert sykehus
#'
#' @export
NIRFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2010-01-01', datoTil='3000-01-01', aar=0, 
                            minald=0, maxald=110, aldGr=0, medKI=0,
                            grType=99, grVar='ShNavn', InnMaate=99, dodInt='', erMann='', hentData=0,
                            preprosess=1, outfile='', lagFig=1, offData=0)
                            #KImaal = NA, utvalgsInfo = "", tittel = "", sortAvtagende=TRUE,) 
      
      
      
{
      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil)
            
      }
      if (offData == 1) {
            ##DENNE MÅ ENDRES NÅR VI FÅR DATA I PAKKEN!!
            filnavn <-  paste0('NIRdata01', valgtVar)
            #assign('NIRdata01',filnavn)
            utvalgsInfo <- RegData$utvalgsInfo
            KImaal <- RegData$KImaal
            KImaaltxt <- RegData$KImaaltxt
            sortAvtagende <- RegData$sortAvtagende
            tittel <- RegData$tittel
            RegData <- RegData$NIRRegData01Off
      }
      
      # Preprosessering av data. I samledokument gjøre dette i samledokumentet. Off01-data er preprosessert.
      if (offData==1) {preprosess <- 0}
      if (preprosess==1){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      
      #------- Tilrettelegge variable
      if (offData == 0) {
            NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
            RegData <- NIRVarSpes$RegData
            sortAvtagende <- NIRVarSpes$sortAvtagende
            KImaal <- NIRVarSpes$KImaal
            KImaaltxt <- NIRVarSpes$KImaaltxt
            tittel <- NIRVarSpes$tittel
      } 
      
      
      #------- Gjøre utvalg
      smltxt <- ''
      medSml <- 0
      
      if (offData == 0) {
            NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, 
                                      minald=minald, maxald=maxald, aar=aar, erMann=erMann, #overfPas=overfPas, 
                                      InnMaate=InnMaate, dodInt=dodInt, grType=grType)
            smltxt <- NIRUtvalg$smltxt
            medSml=NIRUtvalg$medSml 
            utvalgTxt <- NIRUtvalg$utvalgTxt
      }				
      if (offData == 1) { NIRUtvalg <- NIRUtvalgOff(RegData=RegData, aldGr=aldGr, aar=aar, erMann=erMann, #aldGr
                                                   InnMaate=InnMaate, grType=grType) 
                          utvalgTxt <- NIRUtvalg$utvalgTxt #c(utvalgsInfo, 
                        }
      RegData <- NIRUtvalg$RegData

      

#---------------Beregninger
# Variabelen Variabel er definert som indikatorvariabel for den valgte variabelen. 
if (dim(RegData)[1] >= 0) {
      RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
      RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
      Ngr <- table(RegData[ ,grVar])
} else {
      Ngr <- 0}

Ngrense <- 10	
N <- dim(RegData)[1]
#if(N > 0) {Ngr <- table(RegData[ ,grVar])} else {Ngr <- 0}
AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
AndelHele <- sum(RegData$Variabel==1)/N*100	
AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	#round(100*Nvar/Ngr,2)
if (offData==1) {
      AndelerGr <- c(AndelerGr, AndelHele)
      names(N) <- NIRUtvalg$grTypeTxt
      Ngr <- c(Ngr, N)
	 }

if (sum(which(Ngr < Ngrense))>0) {indGrUt <- as.numeric(which(Ngr<Ngrense))} else {indGrUt <- 0}
AndelerGr[indGrUt] <- NA #-0.0001
Ngrtxt <- as.character(Ngr)	#
Ngrtxt[indGrUt] <- paste0('<', Ngrense) 
andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %') 	
andeltxtUsort[indGrUt] <- ''

sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE) 
AndelerGrSort <- AndelerGr[sortInd]
GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')
Ngr <- Ngr[sortInd]
#GrNavnSort <- switch(as.character(offData),
#                     '0' = paste0(names(Ngr)[sortInd], '(',Ngrtxt[sortInd], ')'),
#                     '1' = paste0(c(names(Ngr), NIRUtvalg$grTypeTxt)[sortInd], '(',c(Ngrtxt, N)[sortInd], ')')
#                        )
andeltxt <- andeltxtUsort[sortInd]


N = list(Hoved=N, Rest=0)
Ngr = list(Hoved=Ngr, Rest=0)
AggVerdier = list(Hoved=AndelerGrSort, Rest=0)
xAkseTxt <- "Andel opphold (%)"	
grtxt <- GrNavnSort
AggTot <- AndelHele
soyletxt <- andeltxt

#-----------------
Nut <- length(indGrUt)
indMed <- (Nut+1):length(sortInd)
  KI <- 100*binomkonf(AndelerGrSort[indMed]/100*Ngr$Hoved[indMed], Ngr$Hoved[indMed], konfnivaa=0.95)
  KIned <- c(rep(NA,Nut), KI[1,])
  KIopp <- c(rep(NA,Nut), KI[2,])
  KIHele <- 100*binomkonf(AndelHele/100*N$Hoved, N$Hoved, konfnivaa=0.95)
  
  #prop.test(x=as.numeric(AndelerGrSort[indMed]/100*Ngr$Hoved[indMed]), n=Ngr$Hoved[indMed])
#-----------


#Se NIRFigSoyler for forklaring av innhold i AndelerGrVarData
AndelerGrVarData <- list(AggVerdier=AggVerdier, 
                         AggTot=AndelHele, 
                         N=N, 
                         Ngr=Ngr,
                         grtxt2='', 
                         soyletxt=andeltxt,
                         grtxt=GrNavnSort,
                         tittel=tittel, 
                         #yAkseTxt=yAkseTxt, 
                         
                         retn='H', 
                         xAkseTxt=xAkseTxt, #NIRVarSpes$xAkseTxt,
                         KImaal = KImaal,
                         KImaaltxt = KImaaltxt,
                         grTypeTxt=NIRUtvalg$grTypeTxt,			 
                         utvalgTxt=utvalgTxt, 
                         fargepalett=NIRUtvalg$fargepalett, 
                         medSml=medSml, 
                         smltxt=smltxt)

#Lagre beregnede data
#if (hentData==1) {
#save(AndelerGrVarData, file='data/AndelerGrVarData.RData')
#}

#FigDataParam skal inn som enkeltparametre i funksjonskallet
if (lagFig == 1) {
      cexgr <- 1 - AntGr/200
      fargepalett <- NIRUtvalg$fargepalett
#      FigurAndelGrVar(RegData, AggVerdier=AggVerdier, AggTot=AndelHele, Ngr=Ngr,N=N, cexgr=cexgr, 
#                   tittel=tittel, smltxt=smltxt, utvalgTxt=utvalgTxt, #yAkseTxt=yAkseTxt,
#                   grTypeTxt=NIRUtvalg$grTypeTxt,  fargepalett=NIRUtvalg$fargepalett, grtxt=GrNavnSort, 
#                   soyletxt=andeltxt,grVar=grVar, KImaal = KImaal, KImaaltxt = KImaaltxt, #medKI = medKI,
#                   medSml=medSml, xAkseTxt=xAkseTxt, outfile=outfile)
#}
 
#FigurAndelGrVar <- function(RegData, AggVerdier, AggTot=0, Ngr, tittel='mangler tittel', smltxt='', N, retn='H', 
#                               yAkseTxt='', utvalgTxt='', grTypeTxt='', soyletxt='', grtxt, grtxt2='', hovedgrTxt='', 
#                               grVar='', valgtMaal='Andel', cexgr=1, medSml=0, fargepalett='BlaaOff', xAkseTxt='', 
#                               medKI=0, KImaal = NA, KImaaltxt = '', outfile='') { #Ngr=list(Hoved=0)
            
            
            #---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
            #Hvis for få observasjoner..
            
            if ((N$Hoved < 5) | (dim(RegData)[1]<5))
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
                  vmarg <- min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75))
                  #NB: strwidth oppfører seg ulikt avh. av device...
                  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
                  
                  
                  farger <- FigTypUt$farger
                  fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
                  fargeRest <- farger[3]
                  graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
                  antGr <- length(grtxt)
                  lwdRest <- 3	#tykkelse på linja som repr. landet
                  cexleg <- 0.9	#Størrelse på legendtekst
                  
                  
                  #Definerer disse i beregningsfunksjonen?  
                  xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest, KIopp),na.rm=T)*1.2
                  xmax <- min(xmax, 100) 	#100 som maks bare hvis andelsfigur..
                  ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
                  ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved) 
                  
                  #Må def. pos først for å få strek for hele gruppa bak søylene
                  ### reverserer for å slippe å gjøre det på konf.int
                  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
                                     xlab=xAkseTxt, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
                  indOK <- which(AggVerdier$Hoved>=0)
                  posOK <- pos[indOK]
                  posOver <- max(pos)+0.35*log(max(pos))
                  posDiff <- 1.2*(pos[1]-pos[2])
                  posOK <- pos[indOK]
                  minpos <- min(posOK)-0.7
                  maxpos <- max(posOK)+0.7
                  
                  if (medKI == 1) {	
                        #Legge på konf.int for hele populasjonen
                        #options(warn=-1)	#Unngå melding om KI med lengde 0
                        #KIHele <- AggVerdier$KIHele
                        AntGr <- length(which(AggVerdier$Hoved>0))
                        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), col=farger[3], border=farger[3],
                                c(minpos, maxpos, maxpos, minpos))
                  }
                        #grtxt <- rev(grtxt)
                        grTypeTxt <- smltxt
                        mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
                        #Linje for hele landet/utvalget:
                        lines(x=rep(AggTot, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55), 
                        #Linje for kvalitetsindikatormål:
                        if (!is.na(KImaal)) { 
                              lines(x=rep(KImaal, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55), 
                              text(x=KImaal, y=maxpos+0.6, paste0('Mål:', KImaaltxt), cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0)) 
                        }
                        barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
                                col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
                        soyleXpos <- 1.14*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
                        text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus

                  
                  #------Tegnforklaring (legend)--------
                  #legend(xmax/4, posOver, yjust=0, col=farger[1], border=NA, lwd=2.5, xpd=TRUE, bty='n', #xpd=TRUE,
                  #       paste0(grTypeTxt, 'sykehus: ', sprintf('%.1f', AggTot), '%, N=', N$Hoved), cex = cexleg) 
                  
                  mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
                        TXT <- paste0(grTypeTxt, 'sykehus: ', sprintf('%.1f', AggTot), '%, N=', N$Hoved) #paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved)
                        
                        if (medKI == 0) { 
                              legend(xmax/4, posOver+posDiff, TXT, fill=NA,  border=NA, lwd=2.5, xpd=TRUE, #inset=c(-0.1,0),
                                     col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
                        } else {
                              TXT <- c(TXT, 
                                       paste0('95% konf.int., hele gruppa (',       #grTypeTxt, 'sykehus (', 
                                              sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'))
                              legend(xmax/4, posOver, TXT, yjust=0.2, fill=c(NA, farger[3]),  border=NA, lwd=2.5,  #inset=c(-0.1,0),
                                     col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n') #+2*posDiff
                        }

				  mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
				  if (medKI == 1) {	
				    #Legge på konf.int for hver enkelt gruppe/sykehus
				    options(warn=-1)	#Unngå melding om KI med lengde 0
				    arrows(x0=AggVerdier$Hoved, y0=pos, x1=KIopp, y1=pos, 
				           length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
				    arrows(x0=AggVerdier$Hoved, y0=pos, x1=KIned, y1=pos, 
				           length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
				  }
				  
				          #Fordelingsfigurer: Aktuelt nå for året før.
                  if (medSml == 1) { #Legge på prikker for sammenlikning
                        points(as.numeric(AggVerdier$Rest), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
                  }
                  
                  title(tittel, line=1.5) #cex.main=1.3)
                  
                  #Tekst som angir hvilket utvalg som er gjort
                  avst <- 0.8
                  utvpos <- 3	#Startlinje for teksten
                  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
                  
                  par('fig'=c(0, 1, 0, 1)) 
                  
                  if ( outfile != '') {dev.off()}
                  
            }  #Figur
}  #Figurfunksjon

return(invisible(AndelerGrVarData))

}
