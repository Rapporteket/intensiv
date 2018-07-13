#Hjelpefil for å utvikle nye funksjoner.

library(intensiv)
library(lubridate)
library(zoo)
library(xtable)

load(paste0("A:/Intensiv/NIRdata10000.Rdata")) #RegDataTEST, 2018-06-05
load(paste0("A:/Intensiv/MainFormDataContract2018-06-19.Rdata")) #RegData 2018-06-18
NIRdata <- RegData
RegData <- NIRdata


RegData <- NIRPreprosess(RegData = RegData)
Utvalg <- NIRUtvalgEnh(RegData = RegData, datoFra = '2016-05-07', datoTil = Sys.Date())
RegData <- Utvalg$RegData
datoTil <- as.POSIXlt(Sys.Date(), tz='UTC')
Ferdig:
      tabAntOpphSh12mnd(RegData, datoTil=datoTil)
tabAntOpphSh5Aar(RegData, datoTil = datoTil)
#AarNaa <- as.numeric(format(Sys.Date(), "%Y", tz='UTC'))
#aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
#reshIDdummy <- 109773 #Tromsø med.int



# Nye variable:
# RegData$Mnd <- RegData$InnDato$mon +1
# RegData$Kvartal <- ceiling(RegData$Mnd/3)
# RegData$Halvaar <- ceiling(RegData$Mnd/6)
# aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
# reshIDdummy <- 601161
tabAntPasSh5Aar(RegData, personIDvar='PasientID' , datoTil=datoTil)
      
tabBelegg(RegData, personIDvar='PasientID' , tidsenhet='Aar')

