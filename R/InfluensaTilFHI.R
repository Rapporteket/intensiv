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
#library(dplyr)
variableTilTab <- c('RHF', 'PatientInRegistryGuid', 'DateAdmittedIntensive', 'ICD10_1', 'ICD10_2')
RegData <- InfluData[ ,variableTilTab]
RegDataSyn <- synthpop::syn(InfluData, method = "sample", k=50, seed = 500) #Trekker med tilbakelegging
InfluData <- RegDataSyn$syn

#Identifiser J10 og J11 i ICD10-variablene.
indBekreftet <- which(InfluData$ICD10_1==10 | InfluData$ICD10_2==10)
indMistenkt <- which(InfluData$ICD10_1==11 | InfluData$ICD10_2==11)
InfluData$Bekreftet <- (InfluData$ICD10_1==10 | InfluData$ICD10_2==10)
InfluData$Mistenkt <- (InfluData$ICD10_1==11 | InfluData$ICD10_2==11)

#Legge på tidsenheter
InfluData$InnDato <- as.Date(InfluData$DateAdmittedIntensive) #, tz='UTC', format = '%Y-%m-%d"')
InfluData$Aar <- format(InfluData$InnDato, '%Y')
InfluData$UkeNr <- format(InfluData$InnDato, '%W')
InfluData$UkeNr <- factor(InfluData$UkeNr, levels=c(min(InfluData$UkeNr):max(InfluData$UkeNr)))

#Aggregere på RHF, UkeNr, Ant. bekreftet, Ant. mistenkt
# tapply(InfluData[ ,c('Bekreftet', 'Mistenkt')], InfluData[ ,c('RHF', 'UkeNr')], sum, na.rm=T)#InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
# table(InfluData[ ,c('RHF', 'Mistenkt', 'UkeNr')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
# table(InfluData[ ,c('RHF', 'Bekreftet', 'UkeNr')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))

Bekreftet = aggregate(Bekreftet ~ UkeNr+RHF, data = InfluData, sum, na.rm=T)
Mistenkt = aggregate(Mistenkt ~ UkeNr+RHF, data = InfluData, sum, na.rm=T)
TabellInflu <- cbind(Bekreftet, 
                     Mistenkt = Mistenkt$Mistenkt)
#library(plyr)
#plyr::count(InfluData, c('RHF', 'UkeNr', 'Bekreftet', 'Mistenkt'))

#----Figur med utvikling over tid for bekreftet/mistenkt influensa per RHF------





      # 
# table(InfluData[ ,variableTilTab])
help(tapply)

