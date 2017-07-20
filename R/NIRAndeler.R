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
#'     \item inklKrit: Andeler for de 5 inklusjonskriteriene
#'     \item InnMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'     \item liggetid: Liggetid 
#'     \item NEMS: Skår for ressursbruk. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score)
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'     \item PrimaryReasonAdmitted: Hovedårsak til intensivopphold
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
#' @param maxald Alder, til og med (Standardverdi: 130)
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

NIRAndeler  <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', aar=0, overfPas=0,
                        minald=0, maxald=130, erMann='',InnMaate='', dodInt='',outfile='', grType=99,  
                        preprosess=1, figurtype='andeler', hentData=0, reshID, enhetsUtvalg=1, lagFig=1)	{
      
      
      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil) #minald=0, maxald=130, erMann='',InnMaate='', dodInt=''
      }
      
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøres dette i samledokumentet)
      if (preprosess){
            RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      
      
 #     "%i%" <- intersect
      #--------------- Definere variable ------------------------------
      
      NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype=figurtype)
      RegData <- NIRVarSpes$RegData
      flerevar <- NIRVarSpes$flerevar
      
      
      NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, 
                                minald=minald, maxald=maxald, 
                                erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, 
                                reshID=reshID, enhetsUtvalg=enhetsUtvalg) #overfPas = overfPas,
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

      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      xAkseTxt <- NIRVarSpes$xAkseTxt
      yAkseTxt <- 'Andel pasienter (%)'
      retn <- NIRVarSpes$retn
      tittel <- NIRVarSpes$tittel
      hovedgrTxt <- NIRUtvalg$hovedgrTxt
      medSml <- NIRUtvalg$medSml
      grtxt <- NIRVarSpes$grtxt
      cexgr <- NIRVarSpes$cexgr
      grTypeTxt <- NIRUtvalg$grTypeTxt
      smltxt <- NIRUtvalg$smltxt
      KImaal <- NIRVarSpes$KImaal
      
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
            NIRFigSoyler(RegData, AggVerdier, Ngr, tittel=tittel, hovedgrTxt=hovedgrTxt, 
                         smltxt=smltxt, grTypeTxt=grTypeTxt, Ngr = Ngr, KImaal=KImaal,
                         N=Nfig, retn=retn, utvalgTxt=utvalgTxt, grtxt=grtxt, grtxt2=grtxt2, 
                         medSml=medSml, cexgr=cexgr, xAkseTxt=xAkseTxt, yAkseTxt=yAkseTxt, 
                         outfile=outfile, figurtype=figurtype)	
      }
      
      return(invisible(FigDataParam))
      
}