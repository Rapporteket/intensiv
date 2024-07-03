###------------- Lager "anonymt" minimumsdatasett basert på rådata for å beregne kvalitetsindikatorer. 
# Alternativt kan man lage et 01-datasett, men vi vil da trenge egen beregning for dette datasettet.
# Enhet, RHF, sykehustype, kjønn, aldersgruppe, år, samt variable som inngår i kvalitetsindikatorene
# DATA TIL BRUK I "EGEN" KvalInd-VISNING.
load("C:/Registre/NIR/data/NIRdata10000.Rdata") #RegData
RegData <- NIRPreprosess(RegData=RegData)	#, reshID=reshID)


KvalInd <- c('ReAdmitted', 'Overf', 'SAPSII', 'SMR', 'respiratortid')
OffDataKvalInd <- RegData[ ,c('Aar', 'erMann', 'ShNavn', 'ShType', KvalInd)]
#gr <- c(0, 18, 40,60,80,150)		
#OffDataKvalInd$AldersGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
#levels(OffDataKvalInd$AldersGr) <- c('0-17','18-39','40-59','60-79','80+')
OffDataKvalInd <- OffDataKvalInd[which(RegData$Aar >= 2012),]
#OffDataKvalInd$lopenr <- 1:dim(OffDataKvalInd)[1]

test <- ftable(OffDataKvalInd[,c('erMann', 'ShNavn', 'Aar')])
test2 <- aggregate(OffDataKvalInd$ShNavn, by=OffDataKvalInd[ ,c('ShNavn', 'Aar', 'erMann')], FUN=length)
#test <- ftable(OffDataKvalInd[,c('AldersGr','erMann', 'ShNavn', 'Aar')])
#Andel som mistes hvis tar bort <5: 
ind_faa <- which(test2$x<5) #which(test<5 & test>0)
length(ind_faa)/dim(OffDataKvalInd)[1]*100
ident_ut <-test2[ind_faa,c('ShNavn', 'Aar', 'erMann')]
ind_NA <- 0
for (k in 1:length(ind_faa)) {      #Tar ut de med mindre enn 5 obs.
      ind_NA <- c(which(OffDataKvalInd$ShNavn == ident_ut$ShNavn[k] &
                              OffDataKvalInd$Aar == ident_ut$Aar[k] &
                              OffDataKvalInd$erMann == ident_ut$erMann[k]),
                  ind_NA)
}
OffDataKvalInd[ind_NA,KvalInd] <- NA
#Lagre beregnede data
if (lagreKvalIndData==1) {
      save(OffDataKvalInd, file='data/OffDataKvalInd.RData')
}
#	 NIRUtvalg <- NIRUtvalgEnh(RegData=OffDataKvalInd, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
#	                           overfPas=overfPas, erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, grType=grType)
