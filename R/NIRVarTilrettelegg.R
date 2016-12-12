#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen. 
#' Her kan mye hentes til analysebok
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
      xAkseTxt <- ''	#Benevning
      flerevar <- 0
      
      grNavn <- ''
      ben <- ''   #benevning
      xAkseTxt <- ''
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- TRUE  #Sortering av resultater
      
      
      RegData$Variabel <- 0
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller 
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???
      
      
      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      #Eksempel Numeriske variable
      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel<-RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'	#ben
            tittel <- 'Alder'
 			if (grVar == '') {	#Fordelingsfigur
				gr <- c(seq(0, 100, 10),150)		
				RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
				grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
				xAkseTxt <- 'Aldersgrupper'}
		}

	if (valgtVar=='alder_u18') {	#AndelTid, AndelerGrVar
            #retn <- 'H' #retn vil avhenge av figurtype, dvs. må avhenge av grupperingsvariabel. 
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder<18)] <- 1 
            VarTxt <- 'under 18 år'
            tittel <- 'Pasienter under 18 år'
      }

            
      #Eksempel, indikator - bare i AggVerdier, typisk ikke fordeling og gjennomsnitt, dvs. .
	  if (valgtVar=='reinn') {
		#Andel reinnlagte kun hvor dette er registrert. #Ja=1, nei=2, ukjent=9
		RegData <- RegData[which(RegData$ReAdmitted %in% 1:2), ]	#Tar bort ukjente
		RegData$Variabel[which(RegData$ReAdmitted==1)] <- 1  
		tittel <-'Reinnleggelser på intensivavdelingen (innen 72t)'
		sortAvtagende <- FALSE      #Rekkefølge
	}
 
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
            tittel <- 'Andel opphold der pasienten døde på intensiv'
      }
      

#Fra AndelerGrVar
if (valgtVar=='dod30d') {
#Tar bort ukjente og de som ikke er utskrevet, dvs. tar ut 3:reinnlagt
  #RegData <- RegData[which(RegData$DischargedHospitalStatus %in% 0:2), ]    
  #RegData$Variabel[which(RegData$DischargedHospitalStatus!=0)] <- 1 
  RegData$Variabel <- RegData$Dod30
  tittel <-  'Andel opphold der pasienten døde innen 30 dager etter innleggelse'
}

if (valgtVar=='dodeIntensiv') {
	#Andel som dør på intensiv
	RegData$Variabel <- RegData$DischargedIntensiveStatus	#0: I live, 1: Død intensiv
	RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
	tittel <- 'Andel opphold der pasienten døde på intensiv'
}

if (valgtVar=='innMaate') {
	#Innleggelsesmåte. Genererer annen figurtype
      #0:Planlagt operasjon, 6:Akutt nonoperativ, 8:Akutt operasjon
      RegData$Variabel <- RegData$InnMaate	#Gir ikke mening i andelsberegning, men trenger å være tilgengelig.
      RegData <- RegData[which(RegData$InnMaate %in% c(0,6,8)), ]
	tittel <-'Innkomstmåte'
}

if (valgtVar=='respStotte') {
	#Fått respiratorstøtte. Ja=1, nei=2,
  RegData <- RegData[which(RegData$MechanicalRespirator %in% 1:2), ]
  RegData$Variabel[which(RegData$MechanicalRespirator==1)] <- 1  
  tittel <-'Andel med respiratorstøtte'
}

 #----------Fra gjsnGrVar---------------
      
      if (valgtVar == 'SMR') {
            #Tar ut reinnlagte på intensiv og overflyttede, samt de med SAPSII=0 (ikke scorede) 
            #og de 	under 18år (tas ut i NIRutvalg)
            minald <- max(18, minald) 
            #datoTil <- '2015-12-31'
            #RegData <- RegData[RegData$DischargedHospitalStatus %in% 0:2, ]#0:i live, 1:død int., 2:død post
            #      #Variabelen tatt bort desember 2015
            indMed <- which(RegData$ReAdmitted==2) %i% which(RegData$Overf==1) %i% which(as.numeric(RegData$SAPSII)>0)
            #RegData <- RegData[RegData$ReAdmitted==2,] #1:Ja, 2:Nei, 3:Ukjent, -1:Ikke utfylt
            #RegData <- RegData[RegData$Overf==1, ] 
            #RegData <- RegData[as.numeric(RegData$SAPSII) > 0, ]
            RegData <- RegData[indMed,]
      }
      
      if (valgtVar %in% c('respiratortid', 'liggetid')) {
            #Liggetid og respiratortid bare >0
            #RegData <- RegData[RegData$Overf==1, ] #for ikke overflyttede. Tar ikke lenger ut overflyttede
            RegData <- RegData[which(RegData[ ,valgtVar]>0), ] 
            ben <- ' (dager)'
      } 
      
      if (valgtVar == 'SAPSII') {
            #Tar ut SAPSII=0 (ikke scorede)
            #og de under 18år (tas ut i NIRutvalg)
            minald <- max(18, minald)
            RegData <- RegData[as.numeric(RegData$SAPSII) > 0, ]
      }
      
      
      if (valgtVar %in% c('liggetid', 'NEMS', 'respiratortid', 'SAPSII', 'SMR')){
            RegData$Variabel  <- as.numeric(RegData[ ,valgtVar])
      }
      
      if (valgtVar == 'Nas') {
            #valgtVar <- 'NAS24'
            RegData$NAS24 <- RegData$Nas/RegData$liggetid	#floor(RegData$liggetid)
            indMed <- intersect(which(RegData$NAS24 <= 177), 
                                which( (RegData$liggetid > 8/24) & (RegData$Nas>0)))
            RegData <- RegData[indMed, ]
            RegData$Variabel <- RegData$NAS24
            varTittel <- 'Nas/døgn'
      }
      
      if (valgtVar=='NEMS') {
            #Inkluderer: opphald lenger enn 24 timar og det faktisk er skåra NEMS-poeng.
            #Dvs. NEMS-poeng totalt, altså NEMS per opphold
            indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
            RegData <- RegData[indMed, ]
            varTittel <- 'NEMS/opphold'
      }
      if (valgtVar=='NEMS24') {
            #Inkluderer: opphald lenger enn 24 timar og det faktisk er skåra NEMS-poeng.
            #Dvs. NEMS-poeng totalt/liggjedøger, altså NEMS/24 timar
            indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
            RegData <- RegData[indMed, ]
            RegData$Variabel <- RegData$NEMS/RegData$liggetid	#floor(RegData$liggetid)
            varTittel <- 'NEMS/døgn'
      }
      
#---------------------      
      
      #Eksempel, kategorisk
      if (valgtVar=='InnMaate') {
            tittel <- 'Fordeling av Innkomstmåte'   
            indMed <- which((RegData$InnMaate %in% c(0,6,8)))  #Maybe not neccesary just want to make sure that no other values than 0,6,8 
            RegData <- RegData[indMed, ]             
            gr <- c(0,6,8)
            RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
            grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
            xAkseTxt <- 'Innkomstmåte'
      }
      
      if (valgtVar=='liggetid') {
            tittel <- 'Fordeling av liggetid'
            RegData <- RegData[which(RegData$liggetid > 0), ] #Liggetid og respiratortid bare for tid>0
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
            RegData$VariabelGr <- cut(RegData$liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)	
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'Liggetid (døgn)'
      }
      
      if (valgtVar=='Nas') {
            tittel <- 'Fordeling av Nas'   
            RegData$Variabel <- RegData$Nas/RegData$liggetid
            indMed <- which(RegData$Variabel <= 177) %i% which( (RegData$liggetid > 8/24) & (RegData$Nas>0))
            RegData <- RegData[indMed, ]  
            gr <- c(seq(0, 160, 20),500)
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-20)','[20-40)','[40-60)','[60-80)','[80-100)','[100-120)','[120-140)','[140-160)',  '160+')  
            xAkseTxt <- 'Nas per døgn'
      }
      
      if (valgtVar=='NEMS') {
            tittel <- 'Fordeling av NEMS per døgn'  
            indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
            RegData <- RegData[indMed, ]
            gr <- c(seq(0, 60,10), 500) 
            RegData$Variabel <- RegData$NEMS/RegData$liggetid  
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','60+')  
            xAkseTxt <- 'NEMS per døgn'
      }
      
      if (valgtVar=='respiratortid') {
            tittel <- 'Fordeling av respiratortid'
            RegData <- RegData[which(RegData$respiratortid > 0), ] #Bare for tid>0
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
            RegData$VariabelGr <- cut(RegData$respiratortid, breaks=gr, include.lowest=TRUE, right=FALSE)  
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'Respiratortid (døgn)'
      }
      
      if (valgtVar=='SAPSII') {
            tittel <- 'Fordeling av SAPSII'
            minald <- max(18, minald)     #Bare voksne skal skåres
            RegData <- RegData[which(as.numeric(RegData$SAPSII) > 0), ]
            gr <- c(seq(0, 100,10), 500) 
            RegData$VariabelGr <- cut(RegData$SAPSII, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')  
            xAkseTxt <- 'SAPSII'
      }
      
      
      UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, ben=ben, retn=retn,
                     tittel=tittel, flerevar=flerevar, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData)) 
      
}