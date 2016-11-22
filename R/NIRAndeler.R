#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen beregner andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort. Kan trenge funksjonerne:
#' NIRUtvalgEnh (skal endre navn til NIRUtvalg når ferdig)
#' NIRFigSoyler
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling, 10-årige grupper 
#'     \item InnMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'     \item liggetid: Liggetid 
#'     \item NEMS: Skår for ressursbruk. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score)
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
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
#'	   \item 8: Egen region mot resten [NB: Mangler pt. variabel for region]
#'    	}							
#'    				
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
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
#'				alle, standard (alle andre verdier)
#' @param overfPas Overført under pågående intensivbehandling? 
#'				1 = Pasienten er ikke overført
#'				2 = Pasienten er overført
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'

NIRAndeler  <- function(RegData, valgtVar, datoFra='2011-01-01', datoTil='3000-12-31', 
		minald=0, maxald=130, erMann='',InnMaate='', dodInt='',outfile='', 
		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)	
{


if (hentData == 1) {		
  RegData <- NIRRegDataSQL(datoFra, datoTil) #minald=0, maxald=130, erMann='',InnMaate='', dodInt=''
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)
     }

"%i%" <- intersect

#Kan denne tas bort ?:
#NIRUtvalg <- NIRUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
#                       erMann=erMann,InnMaate=InnMaate,dodInt=dodInt)
#RegData <- NIRUtvalg$RegData
#----------

#--------------- Definere variable ------------------------------

NIRVarSpesifik <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)

RegData <- NIRVarSpesifik$RegData


NIRUtvalg <- NIRUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
                       overfPas=overfPas, erMann=erMann, InnMaate=InnMaate, dodInt=dodInt)
RegData <- NIRUtvalg$RegData
utvalgTxt <- NIRUtvalg$utvalgTxt




#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
N <- list(Hoved = 0, Rest =0)
Ant <- list(Hoved = 0, Rest =0)
Ant$Hoved <- switch(as.character(flerevar), 
				'0' = table(RegData$VariabelGr[indHoved]),
				'1' = colSums(sapply(RegData[indHoved ,variable], as.numeric), na.rm=T))
N$Hoved <- switch(as.character(flerevar), 
				'0' = sum(Ant$Hoved),	#length(indHoved)- Kan inneholde NA
				'1' = length(indHoved))
Andeler$Hoved <- 100*Ant$Hoved/N$Hoved

if (medSml==1) {
	Ant$Rest <- switch(as.character(flerevar), 
					'0' = table(RegData$VariabelGr[indRest]),
					'1' = colSums(sapply(RegData[indRest ,variable], as.numeric), na.rm=T))
	N$Rest <- switch(as.character(flerevar), 
					'0' = sum(Ant$Rest),	#length(indRest)- Kan inneholde NA
					'1' = length(indRest))
	Andeler$Rest <- 100*Ant$Rest/N$Rest
}

grtxt <- paste0(rev(NIRVarSpesifik$grtxt), ' (', rev(sprintf('%.1f',Andeler$Hoved)), '%)') 


FigDataParam <- list(Andeler=Andeler, tittel, smltxt, N=N, grtxt)

if (lagFig == 1) {NIRFigSoyler(RegData, FigDataParam='', outfile)
	} else {
            DataUt <- list(RegData=RegData, Andeler=Andeler, N=N)
            return(invisible(DataUt))}

}

