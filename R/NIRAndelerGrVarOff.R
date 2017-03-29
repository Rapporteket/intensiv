#' Søylediagram med AggVerdier for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med AggVerdier av en gitt variabel for ei valgt gruppering, 
#' f.eks. enheter. I øyeblikket benytter funksjonen bare 'ShNavn' som grupperingsvariabel, men 
#' andre valg kan lett inkluderes. 
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
#'     \item innMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'		Dette valget viser en annen figurtype.
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item reinn: Andel reinnlagte (fjerner ukjente) Kvalitetsindikator
#'    } valgtVar vil her bestemme hvilken vedlagt datafil som skal benyttes
#' @inheritParams NIRAndeler
#' @param RegData Anonymisert datasett
#' @param valgtVar Kvalitetsindikatoren det vises resultat for. Angir også i hvilken fil RegData er lagret. 
#'
#' @param hentData Angir om data skal hentes fra pakken. Filnavn er gitt ved 
#' Filnavn gitt ved 'RegData01' + valgtVar + '.RData'
#' 
#' @return Søylediagram med AggVerdier av valgt variabel for hvert sykehus
#'
#' @export
NIRAndelerGrVarOff <- function(RegData, valgtVar='reinn', aar=0, grType=99, grVar='ShNavn', InnMaate=99, 
                               erMann='', aldGr=0, hentData=0, outfile='', lagFig=0)   
{
      
      if (hentData == 1) {
            ##DENNE MÅ ENDRES NÅR VI FÅR DATA I PAKKEN!!
            filnavn <- paste0('C:/Registre/NIR/data/RegData01', valgtVar, '.RData')
            load(filnavn) 
      }
      
      #------- Gjøre utvalg
      NIRUtvalg <- NIRUtvalgOff(RegData=RegData, aldGr=aldGr, aar=aar, erMann=erMann, 
                                InnMaate=InnMaate, grType=grType)
      RegData <- NIRUtvalg$RegData
      utvalgTxt <- c(utvalgsInfo, NIRUtvalg$utvalgTxt)
      
      
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
      if(N > 0) {Ngr <- table(RegData[ ,grVar])} else {Ngr <- 0}
      AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
      AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	#round(100*Nvar/Ngr,2)
      
      if (sum(which(Ngr < Ngrense))>0) {indGrUt <- as.numeric(which(Ngr<Ngrense))} else {indGrUt <- 0}
      AndelerGr[indGrUt] <- NA #-0.0001
      sortInd <- order(as.numeric(AndelerGr), na.last = FALSE) #decreasing=NIRVarSpes$sortAvtagende, 
      
      AndelerGrSort <- AndelerGr[sortInd]
      AndelHele <- sum(RegData$Variabel==1)/N*100	
      Ngrtxt <- as.character(Ngr)	#
      Ngrtxt[indGrUt] <- paste0('<', Ngrense) 
      GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')
      
      andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %') 	
      andeltxtUsort[indGrUt] <- ''
      andeltxt <- andeltxtUsort[sortInd]
      
      
      N = list(Hoved=N, Rest=0)
      Ngr = list(Hoved=Ngr, Rest=0)
      AggVerdier = list(Hoved=AndelerGrSort, Rest=0)
      xAkseTxt <- "Andel pasienter (%)"	#Denne kan avhenge av figurtype
      
      
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
                               grTypeTxt='alle',			 
                               utvalgTxt=utvalgTxt, 
                               fargepalett=NIRUtvalg$fargepalett)
      

      if (lagFig == 1) {
            cexgr <- 1-ifelse(AntGr>20, 0.25*AntGr/60, 0)
            NIRFigSoyler(RegData, AggVerdier=AggVerdier, AggTot=AndelHele, Ngr=Ngr,N=N, cexgr=cexgr, 
                         tittel=tittel, 
                         utvalgTxt=utvalgTxt, #yAkseTxt=yAkseTxt,
                         grTypeTxt=alle,  fargepalett=NIRUtvalg$fargepalett, grtxt=GrNavnSort, 
                         soyletxt=andeltxt,grVar=grVar, KImaal = KImaal, #medKI = medKI,
                         xAkseTxt=xAkseTxt, outfile=outfile)
      }
      
      return(invisible(AndelerGrVarData))
      
}
