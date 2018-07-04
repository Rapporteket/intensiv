

# Vi har to kvalitetsindikatorer som kan beregnes fra registeret: 
#Reinnleggelse og Respiratortid
library(intensiv)
valgtVar <- 'respiratortidInv'  #reinn, respiratortidInv-> respiratortidInvMoverf
datoFra <- '2016-01-01'
datoTil <- '2016-12-31'
tilleggsVar <- c('Aar', 'Kvartal', 'ShNavn', 'ShType', 'Alder')
rand <- 1
#RegData01Off(RegData, valgtVar=valgtVar, datoFra = datoFra, datoTil, tilleggsVar=tilleggsVar, 
#             hentData=0, rand=rand)
"%i%" <- intersect



#####################Fra skrætsj------------------------
# Vi har to kvalitetsindikatorer som kan beregnes fra registeret: 
#Reinnleggelse og Respiratortid

#Laste inn RegData, dvs. data fra intensivregisterets hovedskjema. 
load('A:/Intensiv/MainFormDataContract2017-11-07.Rdata')

# Riktig format
RegData$InnDato <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d") 
RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) #Fjerner mellomrom (før) og etter navn
#Her må du heller koble på standard sykehusnavn...

#---------------RESPIRATORTID--------------
#--- InvasivVentilation (pusterør/åpnet lufterør), inklusive overførte pasienter
      #Gyldige registreringer:
      ind <- which((RegData$InvasivVentilation>0) & (RegData$InnDato>=as.POSIXlt('2015-01-01')))
      RegData <- RegData[ind,]
    
        
#Hvis figuren skal vise median respiratortid, kan vi ikke kode om variabelen til 01-format:
      #Mål: median <2.5
      RegData$Variabel  <- as.numeric(RegData$InvasivVentilation)
      tittel <- 'Invasiv ventilasjon (inkl. overførte pasienter)'
      
#Hvis figuren skal vise andel som har respiratortid <2.5 døgn. 
#Lage indikatorvariabel (01-variabel):
      RegData$Variabel <- 0
      RegData$Variabel[which(RegData$InvasivVentilation < 2.5)] <- 1
      tittel <- 'Invasiv ventilasjon < 2,5 døgn (inkl. overførte pasienter)'   
  
      
      
      
#-------'Reinnleggelser på intensivavd. (innen 72t)'
     #Andel reinnlagte kun hvor dette er registrert. #Ja=1, nei=2, ukjent=9
     #Kvalitetsindikatormål: < 4% (Reinnleggelser <4%)
 #Hjelpefunksjon:
      FinnReinnleggelser <- function(RegData){
            #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive og PasientGUID
            RegDataSort <- RegData[order(RegData$PasientGUID, RegData$DateAdmittedIntensive,     #Denne tar mest tid
                                         RegData$DateDischargedIntensive), ]
            RegDataSort$OpphNr <- ave(RegDataSort$PasientGUID, RegDataSort$PasientGUID, FUN=seq_along)
            indPasFlereOpph <- which(RegDataSort$OpphNr>1) #intersect(which(RegDataSort$AntOpph>1), which(RegDataSort$OpphNr>1))
            RegDataSort$TidUtInn <- NA
            RegDataSort$TidUtInn[indPasFlereOpph] <- 
                   difftime(as.POSIXlt(RegDataSort$DateAdmittedIntensive[indPasFlereOpph], format="%Y-%m-%d %H:%M:%S"),
                            as.POSIXlt(RegDataSort$DateDischargedIntensive[indPasFlereOpph-1], format="%Y-%m-%d %H:%M:%S"),
                            units = 'hour')
            RegDataSort$Reinn <- 2 #Ikke reinnleggelse
                  RegDataSort$Reinn[RegDataSort$TidUtInn<72 & RegDataSort$TidUtInn >= 0] <- 1 #Reinnleggelse
            return(RegDataSort)
}
 
      #Last inn data på nytt
      #Gyldige registreringer:
      RegData <- RegData[which(RegData$InnDato >= as.POSIXlt('2016-01-01')), ]	#
      #Beregner variabelen Reinn (feil i ReAdmitted)
	  RegData <- FinnReinnleggelser(RegData=RegData)
	  #Lager indikatorvariabel: 
      RegData$Variabel <- 0
	  RegData$Variabel[which(RegData$Reinn==1)] <- 1
      
      
#-------------SMR--------------------
            #Tar ut reinnlagte på intensiv og overflyttede, samt de med SAPSII=0 (ikke scorede) 
            #De under 16år tas ut i NIRutvalg
            #(TransferredStatus: 1= ikke overført, 2= overført), 
            #ReAdmitted: #1:Ja, 2:Nei, 3:Ukjent, -1:Ikke utfylt
            
            indMed <- which(RegData$Reinn==2) %i% which(RegData$TransferredStatus==1) %i% 
            which(as.numeric(RegData$SAPSII)>0) %i% which(RegData$Alder>=16)
      RegData <- RegData[indMed,]
      xAkseTxt <- 'Observert / estimert dødelighet'
      KImaal <- 0.7  #Kvalitetsindikatormål: SMR <0.7 
      #SMR for hvert sykehus          
      grVar <- 'ShNavn'		
      ObsGr <- tapply(RegData$Dod30, RegData[ ,grVar], mean, na.rm=T)
      EstGr <- tapply(RegData$Saps2Score, RegData[ ,grVar], mean, na.rm=T)
      ind0 <- which(EstGr == 0)
      Gjsn <- 100*ObsGr/EstGr  
      if (length(ind0)>0) {Gjsn[ind0] <- 0}#Unngå å dele på 0
      
      Obs <-  mean(RegData$Dod30)	#Kun 0 og 1
      Est <- mean(RegData$Saps2Score, na.rm=T)
      #SMR for hele landet:		
      MidtHele <- ifelse(Est ==0, 0, 100*Obs/Est)
      