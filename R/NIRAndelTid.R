#' Tidstrend av andel opphold
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
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år. 
#'
#' @export
NIRAndelTid <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', tidsenhet='Aar',
                        minald=0, maxald=130, erMann='', InnMaate='', dodInt='', reshID=0, outfile='', 
                        enhetsUtvalg=1, preprosess=1, hentData=0, lagFig=1, offData=0) {
      
      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil)
      }
      if (offData == 1) {
            utvalgsInfo <- RegData$utvalgsInfo
            KImaal <- RegData$KImaal
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
            if (reshID==0) {enhetsUtvalg <- 0}
            NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil, 
                                      minald=minald, maxald=maxald, erMann=erMann, #aar=0, 
                                      InnMaate=InnMaate, dodInt=dodInt, enhetsUtvalg=enhetsUtvalg) #, grType=grType
            smltxt <- NIRUtvalg$smltxt
            medSml <- NIRUtvalg$medSml 
            utvalgTxt <- NIRUtvalg$utvalgTxt
            ind <- NIRUtvalg$ind
      }				
      if (offData == 1) {NIRUtvalg <- NIRUtvalgOff(RegData=RegData, aldGr=aldGr, aar=aar, erMann=erMann, 
                                                   InnMaate=InnMaate, grType=grType)
      
            utvalgTxt <- c(NIRUtvalg$utvalgsTxt, utvalgsInfo)
            ind <- list(Hoved = 1:dim(RegData)[1], Rest = NULL)
      }
      RegData <- NIRUtvalg$RegData
      
      #------------------------Klargjøre tidsenhet--------------
      
      #Brukes til sortering
      RegData$TidsEnhet <- switch(tidsenhet,
                                  Aar = RegData$Aar-min(RegData$Aar)+1,
                                  Mnd = RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
                                  +(RegData$Aar-min(RegData$Aar))*12,
                                  Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                        (RegData$Aar-min(RegData$Aar))*4,
                                  Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                        (RegData$Aar-min(RegData$Aar))*2
      )
      
      tidtxt <- switch(tidsenhet,
                       Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                   sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='.'),
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]))
      
      #RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=1:max(RegData$TidsEnhet))
      RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=1:max(RegData$TidsEnhet)) #evt. levels=tidtxt
      
      #tidtxt <- min(RegData$Aar):max(RegData$Aar)
      #RegData$Aar <- factor(RegData$Aar, levels=tidtxt)
      
      #--------------- Gjøre beregninger ------------------------------
      
      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))
      
      
      NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per år
      NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per år
      AggVerdier$Hoved <- NAarHendHoved/NAarHoved*100
      NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)	
      NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
      AggVerdier$Rest <- NAarHendRest/NAarRest*100
      Ngr <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)
      
      if (valgtVar %in% c('liggetidDod','respiratortidDod')) {
            #Kommentar: for liggetid og respiratortid vises antall pasienter og ikke antall liggedøgn for døde
            Ngr$Hoved<-tapply(RegData[ind$Hoved, 'DischargedIntensiveStatus'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T)    #         liggetid i døgn, navnene blir litt villedende men enklest å gjøre dette på denne måten 
            Ngr$Rest<- tapply(RegData$DischargedIntensiveStatus[ind$Rest], RegData$TidsEnhet[ind$Rest], sum)      
            SUMAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], sum,na.rm=T)
            SUMAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel2'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T)
            AggVerdier$Hoved <- SUMAarHendHoved/SUMAarHoved*100
            SUMAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], sum,na.rm=T)  
            SUMAarHendRest <- tapply(RegData$Variabel2[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
            AggVerdier$Rest <- SUMAarHendRest/SUMAarRest*100
      }  
      
      #grtxt <- paste0(rev(NIRVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Hoved)), '%)') 
      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      yAkseTxt <- 'Andel (%)'
      vektor <- c('Aar','Halvaar','Kvartal','Mnd')
      xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal', 'Innleggelsesmåned')
                         [which(tidsenhet==vektor)])
      
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
                           smltxt=NIRUtvalg$smltxt, Ngr = Ngr, KImaal = KImaal, N=N, retn='V', 
                           utvalgTxt=utvalgTxt, tidtxt=tidtxt, varTxt=varTxt, grtxt2=grtxt2, medSml=medSml, 
                           xAkseTxt=xAkseTxt, yAkseTxt=yAkseTxt,
                           outfile=outfile)	
      }
      
}	#end function



