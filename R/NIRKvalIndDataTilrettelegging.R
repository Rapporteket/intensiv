

# Vi har to kvalitetsindikatorer som kan beregnes fra registeret: 
#Reinnleggelse og Respiratortid
library(intensiv)
valgtVar <- 'respiratortidInv'  #reinn, respiratortidInv-> respiratortidInvMoverf
datoFra <- '2016-01-01'
datoTil <- '2016-12-31'
tilleggsVar <- c('Aar', 'Kvartal', 'ShNavn', 'ShType', 'Alder')
rand <- 1
RegData01Off(RegData, valgtVar=valgtVar, datoFra = datoFra, datoTil, tilleggsVar=tilleggsVar, 
             hentData=0, rand=rand)




#####################Fra skrætsj------------------------
load('A:/Intensiv/MainFormDataContract2017-11-07.Rdata')
RegData <- 

# Riktig format
RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) #Fjerner mellomrom (før) og etter navn
RegData$InnDato <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d") 


#--- InvasivVentilation (pusterør/åpnet lufterør), inklusive overførte pasienter
      #Gyldige registreringer:
      ind <- which((RegData$InvasivVentilation>0) & (RegData$InnDato>=as.POSIXlt('2015-01-01')))
      RegData <- RegData[ind,]
      
#Hvis figuren skal vise median respiratortid, benyttes InvasivVentilation 
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
            
      #Gyldige registreringer:
      RegData <- RegData[which((RegData$ReAdmitted %in% 1:2) & (RegData$InnDato >= as.POSIXlt('2016-01-01'))), ]	#Tar bort ukjente
      #Lager indikatorvariabel: 
      RegData$Variabel[which(RegData$ReAdmitted==1)] <- 1}  
      