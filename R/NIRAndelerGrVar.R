#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en gitt variabel for ei valgt gruppering, 
#' f.eks. enheter. I øyeblikket benytter funksjonen bare 'ShNavn' som grupperingsvariabel, men 
#' andre valg kan lett inkluderes. 
#'
#' Figurtypen som genereres er avhengig av valgtVar. Ved valgtVar='InnMaate', får man ei oversikt oversikt
#' over andel av innkomsttype ved hver enhet, dvs. ei fordeling av de tre innkomsttypene for hver enhet.
#' For andre "valgtVar" viser figuren andel av den valgte variabelen for hver enhet.
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
#' Funksjonen benytter funksjonene: NIRRegDataSQL, NIRPreprosess, NIRVarTilrettelegg, NIRUtvalgEnh
#' og NIRFigSoyler
#'
#' @inheritParams NIRAndeler
#' 
#' 
#' @return Søylediagram med andeler av valgt variabel for hvert sykehus
#'
#' @export
NIRAndelerGrVar <- function(RegData, valgtVar, minald=0, maxald=130, datoFra='2011-01-01', datoTil='3000-01-01', 
                            grType=99, grVar='', InnMaate=99, dodInt='', erMann='', hentData=0, preprosess=1, 
                            outfile='', lagFig=1) 
      
                              
{
      #NB: Tomme grVar fjernes så vurder om dette kan være standard...
      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil)
      }
      
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      
      #------- Tilrettelegge variable
      NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
      RegData <- NIRVarSpes$RegData
      
      #------- Gjøre utvalg
      NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
                                overfPas=overfPas, erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, grType=grType)
      RegData <- NIRUtvalg$RegData
      utvalgTxt <- NIRUtvalg$utvalgTxt
      
      
      
      if (dim(RegData)[1] >= 0) {
            RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
            RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
            Ngr <- table(RegData[ ,grVar])
      } else {
            Ngr <- 0}
      
      Ngrense <- 10	
      N <- dim(RegData)[1]
      if(N > 0) {Ngr <- table(RegData[ ,grVar])} else {Ngr <- 0}
      AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
      AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	#round(100*Nvar/Ngr,2)
      
      if (sum(which(Ngr < Ngrense))>0) {indGrUt <- as.numeric(which(Ngr<Ngrense))} else {indGrUt <- 0}
      AndelerGr[indGrUt] <- -0.0001
      sortInd <- order(as.numeric(AndelerGr), decreasing=NIRVarSpes$sortAvtagende) 
      
      AndelerGrSort <- AndelerGr[sortInd]
      AndelHele <- sum(RegData$Variabel==1)/N*100	
      Ngrtxt <- as.character(Ngr)	#
      Ngrtxt[indGrUt] <- paste0('<', Ngrense) 
      GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')
      
      andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), '  %') 	
      andeltxtUsort[indGrUt] <- ''
      andeltxt <- andeltxtUsort[sortInd]
      #if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}
      
      
      #grTypetextstreng <- c('lokal-/sentral', 'lokal-/sentral', 'region')				
      #if (grType %in% 1:3) {grTypeTxt <- grTypetextstreng[grType]} else {grTypeTxt <- 'alle '}
      N = list(Hoved=N, Rest=0)
      Ant = list(Hoved=Ngr, Rest=0)
      Andeler = list(Hoved=AndelerGrSort, Rest=0)
      yAkseTxt <- "Andel pasienter (%)"	#Denne kan avhenge av figurtype
      
      
      #Se NIRAndelerGrVar for forklaring av innhold i AndelerGrVarData
      AndelerGrVarData <- list(Andeler=Andeler, 
                               AndelTot=AndelHele, 
                               N=N, 
                               Ant=Ant,
                               grtxt2='', 
                               soyletxt=andeltxt,
                               grtxt=GrNavnSort,
                               tittel=NIRVarSpes$tittel, 
                               yAkseTxt=yAkseTxt, 
                               retn='H', 
                               xAkseTxt=NIRVarSpes$xAkseTxt,
                               grTypeTxt=NIRUtvalg$grTypeTxt,			 
                               utvalgTxt=NIRUtvalg$utvalgTxt, 
                               fargepalett=NIRUtvalg$fargepalett, 
                               medSml=NIRUtvalg$medSml, 
                               smltxt=NIRUtvalg$smltxt)
      
      #Lagre beregnede data
      #if (hentData==1) {
      save(AndelerGrVarData, file='data/AndelerGrVarData.RData')
      #}
      
      #FigDataParam skal inn som enkeltparametre i funksjonskallet
      if (lagFig == 1) {
            cexgr <- 1-ifelse(AntGr>20, 0.25*AntGr/60, 0)
            NIRFigSoyler(RegData, Andeler=Andeler, AndelTot=AndelHele, N=N, cexgr=cexgr, tittel=NIRVarSpes$tittel, 
                         smltxt=NIRUtvalg$smltxt, yAkseTxt=yAkseTxt,utvalgTxt=NIRUtvalg$utvalgTxt, 
                         grTypeTxt=NIRUtvalg$grTypeTxt,  fargepalett=NIRUtvalg$fargepalett, grtxt=GrNavnSort, 
                         soyletxt=andeltxt,grVar=grVar,
                         medSml=NIRUtvalg$medSml, xAkseTxt=NIRVarSpes$xAkseTxt, outfile=outfile)
      }
      
      return(invisible(AndelerGrVarData))
      
}
