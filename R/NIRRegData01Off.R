#' Lager "anonymt" minimumsdatasett basert på rådata for å beregne kvalitetsindikatorer. 
#'
#' Datasettet som lagres inneholder utvalgs-/filtreringsvariable, samt teller (og nevner) for 
#' kvalitetsindikatoren som skal beregnes. Både teller og nevner er definert som en indikatorvariabel
#' hvor 1 angir om rada/hendelsen skal inkluderes i beregning av kvalitetsindikatoren eller ikke.
#' 
# Alternativt kan man lage et 01-datasett, men vi vil da trenge egen beregning for dette datasettet.
# Enhet, RHF, sykehustype, kjønn, aldersgruppe, år, samt variable som inngår i kvalitetsindikatorene
#' Funksjonen benytter funksjonene: NIRRegDataSQL, NIRPreprosess, NIRVarTilrettelegg, NIRUtvalgEnh
#' og NIRFigSoyler
#'
#' Argumentet \emph{valgtVar} angir hvilken kvalitetsindikator har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år 
#'     \item alder_over80: Pasienter over 80 år (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga. 
#'     \item innMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'		Dette valget viser en annen figurtype.
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item reinn: Andel reinnlagte (fjerner ukjente) Kvalitetsindikator
#'    }
#' @param tilleggsVar Variable som benyttes til filtrering eller gruppering av valgt variabel (valgtVar)
#'  	Aktuelle valg: Aar, erMann, ShNavn, Mnd, 
#' @inheritParams NIRAndeler
#' @return Definisjon av valgt variabel.
#'
#' @export

RegData01Off <- function(RegData, valgtVar, datoFra='2016-01-01', datoTil='3000-01-01', tilleggsVar=0, hentData=0) {



      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil)
      }

RegData <- NIRPreprosess(RegData=RegData)	

#------- Tilrettelegge variable
NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='andelGrVar')
RegData <- NIRVarSpes$RegData

#--------- Gjøre utvalg
NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)
RegData <- NIRUtvalg$RegData[ ,c(tilleggsVar, 'Variabel')]
utvalgTxt <- NIRUtvalg$utvalgTxt


#---Aldersgrupper
if ('Alder' %in% tilleggsVar) {
      gr <- c(0, 18, 80,150)	#c(0, 18, 40,60,80,150) #	
      grtxt <- c('0-17','18-79','80+') #c('0-17','18-39','40-59','60-79','80+') #
      RegData$AldersGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
      levels(RegData$AldersGr) <- c('0-17','18-39','40-59','60-79','80+')
      RegData <- RegData[ ,-which(names(RegData) == 'Alder')]
      tilleggsVar <- replace(tilleggsVar, which(tilleggsVar == 'Alder'), 'AldersGr')
}


#---Fjerne registreringer hvor gruppering gir <5 observasjoner
Ngrense <- 5
#test <- ftable(RegData[,tilleggsVar])
test2 <- aggregate(RegData$ShNavn, by=RegData[ ,tilleggsVar], FUN=length)
#Fjerne registreringer hvor gruppering gir <Ngrense: 
ind_faa <- which(test2$x < Ngrense) #indekser i test2, ikke i datasettet
AndelBortDum <- round(sum(test2$x[ind_faa])/dim(RegData)[1]*100,1) 
AndelBort <- sprintf("%.1f", AndelBortDum)
ident_ut <-test2[ind_faa, tilleggsVar]

verdiGML <- ident_ut #matrise
verdiNY <- 'sensurert'
mapping <- data.frame(verdiGML,verdiNY)
RegData$VarSensur <- mapping$verdiNY[prodlim::row.match(RegData[, tilleggsVar], ident_ut)]
RegData <- RegData[-which(RegData$VarSensur == 'sensurert'),-which(names(RegData) == 'VarSensur')]


#Lagre beregnede data
#filnavn <- paste0('data/RegData01', valgtVar, '.RData')
filnavn <- paste0('C:/Registre/NIR/data/NIRdata01', valgtVar, '.RData')

tittel <- NIRVarSpes$tittel
KImaal <- NIRVarSpes$KImaal #Mål for kvalitetsindikator
utvalgsInfo <- utvalgTxt
andelFjernet <- AndelBort 
metaInfo <- c('andelFjernet angir andelen data som sensureres pga. grupper med N<5',
             'utvalgsInfo angir utvalget for grunnlagsdataene',
             'KImaal angir målnivået for kvalitetsindikatoren') 
NIRdata01 <- list(NIRRegData01Off=RegData, andelFjernet=andelFjernet, KImaal=KImaal, tittel=tittel, 
                  utvalgsInfo=utvalgsInfo, metaInfo=metaInfo)
#assign(paste0(valgtVar, 'Data'),alleData)
#assign(paste0(valgtVar, 'Data'),list(RegData=RegData, andelFjernet=andelFjernet, KImaal=KImaal, tittel=tittel, 
#                                     utvalgsInfo=utvalgsInfo, metaInfo=metaInfo))

#save(assign(paste0(valgtVar, 'Data'), NIRKIdata), file=filnavn)
save(NIRdata01, file=filnavn)
return(invisible(NIRdata01))
}
