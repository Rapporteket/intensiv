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
#' @inheritParams NIRAndeler
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NIRVarTilrettelegg  <- function(RegData, valgtVar, grVar='', figurtype='andeler'){
      #, datoFra='2011-01-01', datoTil='3000-12-31', 
      #		minald=0, maxald=130, erMann='',InnMaate='', dodInt='',outfile='', 
      #		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)	
      
      
      "%i%" <- intersect
      
      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      grNavn <- ''
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- TRUE  #Sortering av resultater
      KImaal <- NA
      
      
      RegData$Variabel <- 0
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller 
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???
      
      
      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      # For hver valgtVar:
      # Definer og gjør utvalg for variabelen
      # tittel, xAkseTxt, sortAvtagende (standard: TRUE)
      
 tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen
#MANGER 'innMaate' !! Ikke tilrettelagt

 if (valgtVar=='InnMaate') {
       tittel <- 'Fordeling av Innkomstmåte'   
       indMed <- which((RegData$InnMaate %in% c(0,6,8)))  
       RegData <- RegData[indMed, ]             
       gr <- c(0,6,8)
       RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
       grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
       subtxt <- 'Innkomstmåte'
 }
 
 
#------------------------------------- 
 if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'	
            tittel <- 'Alder ved innleggelse'
 		if (grVar == '') {	#Fordelingsfigur
			gr <- c(seq(0, 100, 10),150)		
			RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
			grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
			xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
		}

	if (valgtVar=='alder_u18') {	#AndelTid, AndelerGrVar
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder<18)] <- 1 
            varTxt <- 'under 18 år'
            tittel <- 'Pasienter under 18 år'
      }
 
       if (valgtVar=='alder_over80') {	#AndelTid, AndelerGrVar
             RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
             RegData$Variabel[which(RegData$Alder>=80)] <- 1 
             varTxt <- 'over 80 år'
             tittel <- 'Pasienter over 80 år'
       }
 if (valgtVar=='dod30d') { #AndelTid,AndelerGrVar
       RegData$Variabel <- RegData$Dod30
         varTxt <- 'pasienter som døde'
		tittel <- 'Opphold der pasienten døde innen 30 dager etter innleggelse'
       sortAvtagende <- FALSE
 }
 
 if (valgtVar=='dodeIntensiv') { #AndelTid,AndelerGrVar
       #Andel som dør på intensiv
       RegData$Variabel <- RegData$DischargedIntensiveStatus	#0: I live, 1: Død intensiv
       RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
       varTxt <- 'pasienter som døde på intensiv'
       tittel <- 'Opphold der pasienten døde på intensiv'
       sortAvtagende <- FALSE
 }
 if (valgtVar=='InnMaate') { #Andeler
       #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
       tittel <- 'Fordeling av Innkomstmåte'   
       gr <- c(0,6,8)
       RegData <- RegData[which((RegData$InnMaate %in% gr)), ]  #Kun gyldige verdier: 0,6,8          
       RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
       grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') 
       xAkseTxt <- 'Innkomstmåte'
 }
 

 if (valgtVar == 'liggetid') { #Andeler #GjsnGrVar
		#Liggetid og respiratortid bare >0
		RegData$Variabel  <- as.numeric(RegData$liggetid)
		RegData <- RegData[which(RegData$Variabel>0), ] 
		tittel <- 'Liggetid'
		gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
		RegData$VariabelGr <- cut(RegData$liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)	
		grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
		xAkseTxt <- 'Liggetid (døgn)'
 }

 if (valgtVar=='liggetidDod') { #AndelTid
	  RegData <- RegData[which(RegData$liggetid>=0), ]    #Tar bort liggetid<0 samt NA
	  RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]  	#Tar bort ukjente  
	  RegData$Variabel<-RegData$liggetid	#Liggetid for alle (total liggetid)
	  RegData$Variabel2<- as.numeric(RegData$DischargedIntensiveStatus)*RegData$liggetid #Liggetid for døde
	  varTxt <- 'pasienter som døde'
	  tittel <- 'Andel av total liggetid brukt på dem som dør på intensiv'
}

      if (valgtVar=='Nas24') { #Fordeling, GjsnGrVar
            tittel <- 'Nas per døgn'   #GjsnGrVar henter tittel fra NIRGjsnVar
            RegData$Variabel <- RegData$Nas/RegData$liggetid
            indMed <- which(RegData$Variabel <= 177) %i% which( (RegData$liggetid > 8/24) & (RegData$Nas>0))
            RegData <- RegData[indMed, ]  
            gr <- c(seq(0, 160, 20),500)
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-20)','[20-40)','[40-60)','[60-80)','[80-100)','[100-120)','[120-140)','[140-160)',  '160+')  
            xAkseTxt <- 'Nas-score/døgn'
      }
      if (valgtVar=='NEMS') { #GjsnGrVar
            #Inkluderer: opphald lenger enn 24 timar og det faktisk er skåra NEMS-poeng.
            #Dvs. NEMS-poeng totalt, altså NEMS per opphold
            tittel <- 'NEMS per opphold'
            RegData$Variabel <- RegData$NEMS
            indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
            RegData <- RegData[indMed, ]
            xAkseTxt <- 'NEMS/opphold'
      }
      if (valgtVar=='NEMS24') { #Andeler, GjsnGrVar
            #Inkluderer: opphald lenger enn 24 timar og det faktisk er skåra NEMS-poeng.
            #Dvs. NEMS-poeng totalt/liggjedøger, altså NEMS/24 timar
            indMed <- which( (RegData$liggetid>=1) & (RegData$NEMS>1))	#NEMS=0 el 1 - ikke registrert.
            RegData <- RegData[indMed, ]
            RegData$Variabel <- RegData$NEMS/RegData$liggetid
            tittel <- 'Fordeling av NEMS per døgn'  #Benyttes bare i Andeler
            gr <- c(seq(0, 60,10), 500) 
            RegData$Variabel <- RegData$NEMS/RegData$liggetid  
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','60+')  
            xAkseTxt <- 'NEMS per døgn'
      }
       if (valgtVar=='reinn') { #AndelGrVar, AndelTid, GjsnGrVar
             #Andel reinnlagte kun hvor dette er registrert. #Ja=1, nei=2, ukjent=9
             RegData <- RegData[which((RegData$ReAdmitted %in% 1:2) & (RegData$InnDato >= as.POSIXlt('2016-01-01'))), ]	#Tar bort ukjente
             if (figurtype %in% c('andelGrVar', 'andelTid')) {
                   RegData$Variabel[which(RegData$ReAdmitted==1)] <- 1}  
             if (figurtype == 'gjsnGrVar') {RegData$Variabel <- RegData$ReAdmitted}  
             tittel <-'Reinnleggelser på intensivavd. (innen 72t)'
             sortAvtagende <- FALSE      #Rekkefølge
             KImaal <- 4  #Reinnleggelser <4% 
       }
	   
      if (valgtVar == 'respiratortid') { #Andeler #GjsnGrVar #AndelGrVar, GjsnTid
            RegData <- RegData[which((RegData$respiratortid>0) & (RegData$InnDato>=as.POSIXlt('2016-01-01'))), ] 
            if (figurtype %in% c('andeler', 'gjsnGrVar')) {
                  RegData$Variabel  <- as.numeric(RegData$respiratortid)
                  tittel <- 'Respiratortid'}      #Andeler, GjsnGrVar
            if (figurtype %in% c('andelTid', 'andelGrVar')) {
                  RegData$Variabel[which(RegData$respiratortid < 2.5)] <- 1
                  tittel <- 'Respiratortid mindre enn 2,5 døgn'}     #AndelGrVar, AndelTid
            
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
            RegData$VariabelGr <- cut(RegData$respiratortid, breaks=gr, include.lowest=TRUE, right=FALSE)  
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'Respiratortid (døgn)'
            varTxt <- 'med resp.tid < 2,5 døgn'
            #KImaal <- 2.5 #Median respiratortid <2,5døgn 
            KImaal <- 50 #Over 50% med respiratortid <2,5døgn
            sortAvtagende <- TRUE      #Rekkefølge
      } 


if (valgtVar=='respiratortidDod') {
  RegData <- RegData[which(RegData$respiratortid>=0), ]    #Tar bort respiratortid<0 samt NA
  RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]    #Tar bort ukjente. 0:levende, 1:døde  
  RegData$Variabel<-RegData$respiratortid
  RegData$Variabel2<-as.numeric(RegData$DischargedIntensiveStatus)*RegData$respiratortid
  varTxt <- 'pasienter som døde'
  tittel <- 'Andel av total respiratortid brukt på dem som dør på intensiv'
}

       if (valgtVar=='respStotte') { #AndelGrVar, AndelTid
             #Fått respiratorstøtte. Ja=1, nei=2,
             RegData <- RegData[which(RegData$MechanicalRespirator %in% 1:2), ]
             RegData$Variabel[which(RegData$MechanicalRespirator==1)] <- 1  
             varTxt <- 'pasienter med respiratorstøtte'
			 tittel <-'Andel med respiratorstøtte'
             sortAvtagende <- FALSE      #Rekkefølge
       }
 if (valgtVar=='SAPSII') { #Andeler #GjsnGrVar
             #Tar ut SAPSII=0 (ikke scorede)
             #og de under 18år (tas ut i NIRutvalg)
             tittel <- 'Fordeling av SAPSII'
            minald <- max(18, minald)     #Bare voksne skal skåres
            RegData <- RegData[which(as.numeric(RegData$SAPSII) > 0), ]
            RegData$Variabel <- RegData$SAPSII
            gr <- c(seq(0, 100,10), 500) 
            RegData$VariabelGr <- cut(RegData$SAPSII, breaks=gr, include.lowest=TRUE, right=FALSE) 
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')  
            xAkseTxt <- 'SAPSII-skår'
       }

      if (valgtVar == 'SMR') { #GjsnGrVar
            #Tar ut reinnlagte på intensiv og overflyttede, samt de med SAPSII=0 (ikke scorede) 
            #De under 18år tas ut i NIRutvalg
            #(TransferredStatus: 1= ikke overført, 2= overført), 
            #ReAdmitted: #1:Ja, 2:Nei, 3:Ukjent, -1:Ikke utfylt
            minald <- max(18, minald) 
            indMed <- which(RegData$ReAdmitted==2) %i% which(RegData$Overf==1) %i% 
                  which(as.numeric(RegData$SAPSII)>0)
            RegData <- RegData[indMed,]
            RegData$Variabel <- RegData$SMR
            xAkseTxt <- 'Observert / estimert dødelighet'
            KImaal <- 0.7  #SMR <0.7 
      }
      
#---------------KATEGORISKE
      if (valgtVar=='InnMaate') {
            tittel <- 'Fordeling av Innkomstmåte'   
            indMed <- which((RegData$InnMaate %in% c(0,6,8)))  #Maybe not neccesary just want to make sure that no other values than 0,6,8 
            RegData <- RegData[indMed, ]             
            gr <- c(0,6,8)
            RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
            grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
            xAkseTxt <- 'Innkomstmåte'
      }
 #if (valgtVar=='innMaate') {
 #	#Innleggelsesmåte. Genererer annen figurtype
 #      #0:Planlagt operasjon, 6:Akutt nonoperativ, 8:Akutt operasjon
 #      RegData$Variabel <- RegData$InnMaate	#Gir ikke mening i andelsberegning, men trenger å være tilgengelig.
 #      RegData <- RegData[which(RegData$InnMaate %in% c(0,6,8)), ]
 #	tittel <-'Innkomstmåte'
 #}
 #------------Alle over er ok
 
 
      
      
      UtData <- list(RegData=RegData, grtxt=grtxt, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData)) 
      
}
