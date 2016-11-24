#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av andeler eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
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
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.
#' @param preprosess Preprosesser data
#'                 FALSE: Nei
#'                 TRUE: Ja (Standard)
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NIRVarTilrettelegg  <- function(RegData, valgtVar){
      #, datoFra='2011-01-01', datoTil='3000-12-31', 
      #		minald=0, maxald=130, erMann='',InnMaate='', dodInt='',outfile='', 
      #		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)	
      
      
      "%i%" <- intersect
      
      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      subtxt <- ''	#Benevning
      flerevar <- 0
      
      grNavn <- ''
      ben <- ''   #benevning
      xAkseTxt <- ''
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#andeler, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      
      
      RegData$Variabel <- 0
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller 
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???
      
      
      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      #Eksempel Numeriske variable
      if (valgtVar=='alder') {	#Fordeling, sentralmål
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel<-RegData$Alder  	#GjsnTid, GjsnGrVar
            gr <- c(seq(0, 100, 10),150)	#c(0,16,31,46,61,76,200)	
            RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
            grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
            subtxt <- 'Aldersgrupper'
            ben <- 'alder (år)'
            tittel <- 'Alder'
      }
      if (valgtVar=='alder_u18') {	#AndelTid, AndelerGrVar
            #retn <- 'H' #retn vil avhenge av figurtype, dvs. må avhenge av grupperingsvariabel. 
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder<18)] <- 1 
            VarTxt <- 'under 18 år'
            tittel <- 'Pasienter under 18 år'
      }
      
      #Eksempel, indikator - bare i andeler, typisk ikke fordeling og gjennomsnitt, dvs. .
      if (valgtVar=='dodeIntensiv') { #AndelGrVar
            #Andel som dør på intensiv
            RegData$Variabel <- RegData$DischargedIntensiveStatus	#0: I live, 1: Død intensiv
            RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
            tittel <- 'Andel opphold der pasienten døde på intensiv'
      }
      if (valgtVar=='dodeIntensiv') { #AndelTid
            RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]		#Tar bort ukjente
            RegData$Variabel[which(RegData$DischargedIntensiveStatus==1)] <- 1 
            VarTxt <- 'pasienter som døde på intensiv'
            Tittel <- 'Andel opphold der pasienten døde på intensiv'
      }
      
      
      #Eksempel, kategorisk
      if (valgtVar=='InnMaate') {
            tittel <- 'Fordeling av Innkomstmåte'   
            indMed <- which((RegData$InnMaate %in% c(0,6,8)))  #Maybe not neccesary just want to make sure that no other values than 0,6,8 
            RegData <- RegData[indMed, ]             
            gr <- c(0,6,8)
            RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
            grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
            subtxt <- 'Innkomstmåte'
      }
      
      if (valgtVar=='liggetid') {
            tittel <- 'Fordeling av liggetid'
            RegData <- RegData[which(RegData$liggetid > 0), ] #Liggetid og respiratortid bare for tid>0
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
            RegData$VariabelGr <- cut(RegData$liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)	
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            subtxt <- 'Liggetid (døgn)'
      }
      
      if (valgtVar=='Nas') {
            tittel <- 'Fordeling av Nas'   
            RegData$Variabel <- RegData$Nas/RegData$liggetid
            indMed <- which(RegData$Variabel <= 177) %i% which( (RegData$liggetid > 8/24) & (RegData$Nas>0))
            RegData <- RegData[indMed, ]  
            gr <- c(seq(0, 160, 20),500)
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-20)','[20-40)','[40-60)','[60-80)','[80-100)','[100-120)','[120-140)','[140-160)',  '160+')  
            subtxt <- 'Nas per døgn'
      }
      
      if (valgtVar=='NEMS') {
            tittel <- 'Fordeling av NEMS per døgn'  
            indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
            RegData <- RegData[indMed, ]
            gr <- c(seq(0, 60,10), 500) 
            RegData$Variabel <- RegData$NEMS/RegData$liggetid  
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','60+')  
            subtxt <- 'NEMS per døgn'
      }
      
      if (valgtVar=='respiratortid') {
            tittel <- 'Fordeling av respiratortid'
            RegData <- RegData[which(RegData$respiratortid > 0), ] #Bare for tid>0
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
            RegData$VariabelGr <- cut(RegData$respiratortid, breaks=gr, include.lowest=TRUE, right=FALSE)  
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            subtxt <- 'Respiratortid (døgn)'
      }
      
      if (valgtVar=='SAPSII') {
            tittel <- 'Fordeling av SAPSII'
            minald <- max(18, minald)     #Bare voksne skal skåres
            RegData <- RegData[which(as.numeric(RegData$SAPSII) > 0), ]
            gr <- c(seq(0, 100,10), 500) 
            RegData$VariabelGr <- cut(RegData$SAPSII, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')  
            subtxt <- 'SAPSII'
      }
      
      
      UtData <- list(RegData=RegData, grtxt=grtxt, subtxt=subtxt, ben=ben, retn=retn,
                     tittel=tittel, flerevar=flerevar)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData)) 
      
}