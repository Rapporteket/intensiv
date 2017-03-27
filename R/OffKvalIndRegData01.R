#' Lager "anonymt" minimumsdatasett basert på rådata for å beregne kvalitetsindikatorer. 
#'
#' Datasettet som lagres inneholder utvalgs-/filtreringsvariable, samt teller (og nevner) for 
#' kvalitetsindikatoren som skal beregnes. Både teller og nevner er definert som en indikatorvariabel
#' hvor 1 angir om rada/hendelsen skal inkluderes i beregning av kvalitetsindikatoren eller ikke.
#' 
# Alternativt kan man lage et 01-datasett, men vi vil da trenge egen beregning for dette datasettet.
# Enhet, RHF, sykehustype, kjønn, aldersgruppe, år, samt variable som inngår i kvalitetsindikatorene
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

#' Funksjonen benytter funksjonene: NIRRegDataSQL, NIRPreprosess, NIRVarTilrettelegg, NIRUtvalgEnh
#' og NIRFigSoyler

RegData01Off <- function(RegData, valgtVar, tilleggsVar=0, hentData=0) 

tilleggsVar <- c('Aar', 'erMann', 'ShNavn', 'ShType') #, 'Alder')
datoFra <- '2015-01-01'
datoTil='3000-01-01'

      if (hentData == 1) {		
            RegData <- NIRRegDataSQL(datoFra, datoTil)
      }

RegData <- NIRPreprosess(RegData=RegData)	

#------- Tilrettelegge variable
NIRVarSpes <- NIRVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
RegData <- NIRVarSpes$RegData

#--------- Gjøre utvalg
NIRUtvalg <- NIRUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil)
RegData <- NIRUtvalg$RegData[ ,c(tilleggsVar, 'Variabel')]
#utvalgTxt <- NIRUtvalg$utvalgTxt


#---Aldersgrupper
if ('Alder' %in% tilleggsVar) {
      gr <- c(0, 18, 40,60,80,150)		
      RegData$AldersGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
      levels(RegData$AldersGr) <- c('0-17','18-39','40-59','60-79','80+')
}
#RegData$lopenr <- 1:dim(RegData)[1]


#---Fjerne registreringer hvor gruppering gir <5 observasjoner
Ngrense <- 5
#test <- ftable(RegData[,tilleggsVar])
test2 <- aggregate(RegData$ShNavn, by=RegData[ ,tilleggsVar], FUN=length)
#Fjerne registreringer hvor gruppering gir <Ngrense: 
ind_faa <- which(test2$x<Ngrense) #indekser i test2, ikke i datasettet
AndelBortDum <- round(length(ind_faa)/dim(RegData)[1]*100,1)
AndelBort <- sprintf("%.1f", AndelBortDum)
ident_ut <-test2[ind_faa, tilleggsVar]

#MULIG Å GJØRE PÅ DENNE MÅTEN...????
#verdi <- c(-1,1:6)
#txt <- c('None', 'SvaertBra', 'MegetBra', 'Bra', 'GanskeBra', 'Daarlig', 'IkkeRelevant')
#mapping <- data.frame(verdi,txt)

#PaarorData$Ny <- mapping$verdi[match(PaarorData$Gml, mapping$txt)]
#PaarorData$Ny <- mapping$verdi[match(RegData[,tilleggsVar], ident_ut)]


#Må finne hvilke rader i RegData som skal ut.
ind_NA <- 0
for (k in 1:length(ind_faa)) {      #Tar ut de med mindre enn Ngrense obs.
      ind_NA <- c(which(RegData$ShNavn == ident_ut$ShNavn[k] &
                              RegData$Aar == ident_ut$Aar[k] &
                              RegData$erMann == ident_ut$erMann[k]),
                  ind_NA)
}
RegData[ind_NA,'Variabel'] <- NA
#Lagre beregnede data
filnavn <- paste0('data/RegData01', valgtVar, '.RData')
      save(RegData, file=filnavn)
