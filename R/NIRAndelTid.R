#' Tidstrend av andel pasienter
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år 
#'     \item alder_over80: Pasienter over 80 år  (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse #ut:dodeSykehus
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga. 
#'	 \item liggetidDod: Andel av total liggetid brukt på de som dør på intensiv
#'     \item respiratortidDod: Respiratortid brukt på de som dør på intensiv
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item reinn: Andel reinnlagte (kun hvor dette er registrert, dvs. fjerner ukjente)
#'    }
#'
#' @inheritParams NIRAndeler 
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år. 
#'
#' @export
NIRAndelTid <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', 
                        minald=0, maxald=130, erMann='', InnMaate='', dodInt='', reshID, outfile='', 
                        enhetsUtvalg=1, preprosess=1, hentData=0, lagFig=1, offData=0) {
  
  if (hentData == 1) {		
    RegData <- NIRRegDataSQL(datoFra, datoTil)
  }
       if (offData == 1) {
            ##DENNE MÅ ENDRES NÅR VI FÅR DATA I PAKKEN!!
            filnavn <- paste0('A:/Intensiv/NIRdata01', valgtVar, '.RData')
            load(filnavn) 
            RegData <- NIRdata01$NIRRegData01Off
            utvalgsInfo <- NIRdata01$utvalgsInfo
            KImaal <- NIRdata01$KImaal
            sortAvtagende <- NIRdata01$sortAvtagende
            tittel <- NIRdata01$tittel
      }
 
      # Preprosessering av data. I samledokument gjøre dette i samledokumentet. Off01-data er preprosessert.
      if (offData==1) {preprosess <- 0}
      if (preprosess==1){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      
      #------- Tilrettelegge variable
      varTxt <- ''
      if (offData == 0) {
            NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
            RegData <- NIRVarSpes$RegData
            sortAvtagende <- NIRVarSpes$sortAvtagende
            varTxt <- NIRVarSpes$varTxt
            KImaal <- NIRVarSpes$KImaal
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
			ind <- NIRUtvalg$ind
      }				
      if (offData == 1) {NIRUtvalg <- NIRUtvalgOff(RegData=NIRdata01$NIRRegData01Off, aldGr=aldGr, aar=aar, erMann=erMann, 
                                                   InnMaate=InnMaate, grType=grType)
      
      utvalgTxt <- c(NIRUtvalg$utvalgsTxt, utvalgsInfo)
	  ind <- list(Hoved = 1:dim(RegData)[1], Rest = NULL)
      }
      RegData <- NIRUtvalg$RegData
   
    #--------------- Gjøre beregninger ------------------------------
      
    AggVerdier <- list(Hoved = 0, Rest =0)
    N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))
    
    tidtxt <- min(RegData$Aar):max(RegData$Aar)
    RegData$Aar <- factor(RegData$Aar, levels=tidtxt)
    
    NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'], length) #Tot. ant. per år
    NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'],sum, na.rm=T) #Ant. hendelser per år
    AggVerdier$Hoved <- NAarHendHoved/NAarHoved*100
    NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest], length)	
    NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest],sum, na.rm=T)
    AggVerdier$Rest <- NAarHendRest/NAarRest*100
	Ngr <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)
	
if (valgtVar %in% c('liggetidDod','respiratortidDod')) {
#Kommentar: for liggetid og respiratortid vises antall pasienter og ikke antall liggedøgn for døde
	Ngr$Hoved<-tapply(RegData[ind$Hoved, 'DischargedIntensiveStatus'], RegData[ind$Hoved ,'Aar'],sum, na.rm=T)    #         liggetid i døgn, navnene blir litt villedende men enklest å gjøre dette på denne måten 
	Ngr$Rest<- tapply(RegData$DischargedIntensiveStatus[ind$Rest], RegData$Aar[ind$Rest], sum)      
	SUMAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'Aar'], sum,na.rm=T)
	SUMAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel2'], RegData[ind$Hoved ,'Aar'],sum, na.rm=T)
	AggVerdier$Hoved <- SUMAarHendHoved/SUMAarHoved*100
	SUMAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$Aar[ind$Rest], sum,na.rm=T)  
	SUMAarHendRest <- tapply(RegData$Variabel2[ind$Rest], RegData$Aar[ind$Rest],sum, na.rm=T)
	AggVerdier$Rest <- SUMAarHendRest/SUMAarRest*100
}  
    
 #grtxt <- paste0(rev(NIRVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Hoved)), '%)') 
      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      yAkseTxt <- ''
      xAkseTxt='Andel pasienter (%)'
      
      FigDataParam <- list(AggVerdier=AggVerdier, N=N, 
                           Ngr=Ngr,	
                           KImaal <- KImaal,
                           #soyletxt=soyletxt,
                           grtxt2=grtxt2, 
                           varTxt=varTxt,
                           tidtxt=tidtxt, #NIRVarSpes$grtxt,
                           tittel=tittel, 
                           retn='V', 
                           xAkseTxt=xAkseTxt,
                           yAkseTxt=yAkseTxt,
                           utvalgTxt=NIRUtvalg$utvalgTxt, 
                           fargepalett=NIRUtvalg$fargepalett, 
                           medSml=medSml,
                           hovedgrTxt=NIRUtvalg$hovedgrTxt,
                           smltxt=NIRUtvalg$smltxt)
      
  
      if (lagFig == 1) {
            NIRFigTidAndel(RegData, AggVerdier, Ngr, tittel=tittel, hovedgrTxt=NIRUtvalg$hovedgrTxt, 
                         smltxt=NIRUtvalg$smltxt, Ngr = Ngr, KImaal <- KImaal, N=N, retn='V', 
                         utvalgTxt, tidtxt=tidtxt, varTxt=varTxt, grtxt2=grtxt2, medSml=medSml, 
                         xAkseTxt=xAkseTxt, yAkseTxt=yAkseTxt,
                         outfile=outfile)	
      }
	
}	#end function



