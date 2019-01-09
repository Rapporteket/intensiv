# Lage en tabell med aggregerte tall for influensaovervåkning
#Info:
#«ICD-10-kode: J10» (bekreftet influensa), 
#«ICD-11-kode: J10» (mistenkt influensa), 

#Antall innleggelser med bekreftet influensa (ICD10_1 eller _2 == 10) pr.uke og  pr. helseregion, 
#evt. aldersgrupper. Foreløpig for få observasjoner. Kan angi median, min og maks?
#Variable:  RHF , PatientInRegistryGuid, DateAdmittedIntensive, ICD10_1-ICD10_5, 

# Datafelt som er ønskte i aggregert rapport, på dags/vekebasis:
# -	Veke
# -	Landsdel
# -	Diagnosekode
# -	Aldersgruppe ([0-5> [5-10> [10-15>) …. )
# -	Skjemastatus ? (tal på skjema i kladd vs ferdigstilte)


#' Henter data registrere influensadata fra influensatabellen i Intensivregisteret
#'
#' @inheritParams NIRFigAndeler
#'
#' @return Henter dataramma RegData for Intensivregisteret
#' @export
#'
#'
NIRInfluDataSQL <- function(datoFra = '2011-01-01', datoTil = '2099-01-01') {
      
      registryName <- "nir"
      dbType <- "mysql"
      
      query <- paste0('SELECT *
            FROM
            	InfluensaFormDataContract
            WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
      #WHERE cast(DateAdmittedIntensive as date) >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')  
      
      RegData <- rapbase::LoadRegData(registryName, query, dbType)
}      

                     
InfluData <- read.table('A:/Intensiv/InfluensaFormDataContract2019-01-07.csv', sep=';', 
                        stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#library(synthpop)
#library(dplyr)
variableTilTab <- c('RHF', 'PatientInRegistryGuid', 'FormDate', 'ICD10_1', 'ICD10_2') #'DateAdmittedIntensive', 
RegData <- InfluData[ ,variableTilTab]
#RegDataSyn <- synthpop::syn(InfluData, method = "sample", k=50, seed = 500) #Trekker med tilbakelegging
#InfluData <- RegDataSyn$syn

InfluData <- RegData
InfluData$Influensa <- NA
# InfluData$Bekreftet <- 0
# InfluData$Mistenkt <- 0
#--Identifiser J10 og J11 i ICD10-variablene.
# InfluData$Bekreftet[(InfluData$ICD10_1==10 | InfluData$ICD10_2==10)] <- 1
# InfluData$Mistenkt[(InfluData$ICD10_1==11 | InfluData$ICD10_2==11)] <- 1
 indBekreftet <- which(InfluData$ICD10_1==10 | InfluData$ICD10_2==10)
 indMistenkt <- which(InfluData$ICD10_1==11 | InfluData$ICD10_2==11)
# InfluData$Bekreftet <- (InfluData$ICD10_1==10 | InfluData$ICD10_2==10)
# InfluData$Mistenkt <- (InfluData$ICD10_1==11 | InfluData$ICD10_2==11)
 InfluData$Influensa[indMistenkt] <- 'Mistenkt'
 InfluData$Influensa[indBekreftet] <- 'Bekreftet'

 
#Legge på tidsenheter
# R starter på uke 0 i 2019 og har følgelig gjennomgående feil ukenummer
#InfluData$InnDato <- as.Date(InfluData$DateAdmittedIntensive) #, tz='UTC', format = '%Y-%m-%d"')
InfluData$InnDato <- as.Date(InfluData$FormDate) #, tz='UTC', format = '%Y-%m-%d"')
InfluData$Aar <- format(InfluData$InnDato, '%Y')
InfluData$UkeNr <- format(InfluData$InnDato, '%V')
InfluData$UkeAar <- format(InfluData$InnDato, '%Y.%V')
#InfluData$UkeAar <- sub('19.00', '18.52', InfluData$UkeAar)


#InfluData$UkeNr <- factor(InfluData$UkeNr, levels=c(min(InfluData$UkeNr):max(InfluData$UkeNr)))
InfluData$RHF <- factor(InfluData$RHF)

# dato <- '2018-12-31'
# format.Date(dato, '%Y.%V')
# lubridate::isoweek(dato)
# lubridate::week(dato)

#Aggregere på RHF, UkeNr, Ant. bekreftet, Ant. mistenkt
# tapply(InfluData[ ,c('Bekreftet', 'Mistenkt')], InfluData[ ,c('RHF', 'UkeAar')], sum, na.rm=T)#InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
# tapply(InfluData[ ,c('RHF', 'UkeAar', "ICD10_1")], length)
# table(InfluData[ ,c('RHF', 'Mistenkt', 'UkeNr')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
TabUkeRHF <- ftable(InfluData[ ,c('UkeAar', 'RHF', 'Influensa')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
TabUkeTot <- ftable(InfluData[ ,c('UkeAar', 'Influensa')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
 
 
# Bekreftet = aggregate(Bekreftet ~ RHF+UkeNr, data = InfluData, sum, na.rm=T, na.action = na.pass)
# Mistenkt = aggregate(Mistenkt ~ RHF+UkeNr, data = InfluData, sum, na.rm=T, na.action = na.pass)
# TabellInflu <- cbind(Bekreftet, 
#                      Mistenkt = Mistenkt$Mistenkt)
#library(plyr)
#plyr::count(InfluData, c('RHF', 'UkeNr', 'Bekreftet', 'Mistenkt'))

#----Figur med utvikling over tid for bekreftet/mistenkt influensa per RHF------





      # 
# table(InfluData[ ,variableTilTab])
help(tapply)

