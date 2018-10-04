# Lage en tabell med aggregerte tall for influensaovervåkning
#Info:
#«ICD-10-kode: J10» (bekreftet influensa), 
#«ICD-11-kode: J10» (mistenkt influensa), 

#Antall innleggelser med bekreftet influensa (ICD10_1 eller _2 == 10) pr.uke og  pr. helseregion, 
#evt. aldersgrupper. Foreløpig for få observasjoner. Kan angi median, min og maks?
#Variable:  RHF , PatientInRegistryGuid, DateAdmittedIntensive, ICD10_1-ICD10_5, 


InfluData <- read.table('A:/Intensiv/InfluensaFormDataContract2018-10-02.csv', sep=';', 
                        stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
library(synthpop)
library(dplyr)
RegDataSyn <- synthpop::syn(InfluData, method = "sample", k=50, seed = 500) #Trekker med tilbakelegging
InfluData <- RegDataSyn$syn
variableTilTab <- c('RHF', 'PatientInRegistryGuid', 'DateAdmittedIntensive', 'ICD10_1', 'ICD10_2')

#Identifiser J10 og J11 i ICD10-variablene.
indBekreftet <- which(InfluData$ICD10_1==10 | InfluData$ICD10_2==10)
indMistenkt <- which(InfluData$ICD10_1==11 | InfluData$ICD10_2==11)
#Aggregere uker
InfluData$InnDato <- strptime(InfluData$DateAdmittedIntensive, format = 'YY-mm-dd')

InfluData$InnDato <- as.Date(InfluData$DateAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d") 
InfluData$Innleggelsestidspunkt <- as.POSIXlt(InfluData$DateAdmittedIntensive, tz= 'UTC', format="%Y-%m-%d %H:%M:%S" )


table(InfluData[ ,variableTilTab])
help(tapply)

