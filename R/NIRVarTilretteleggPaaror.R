#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen. 
#' Her kan mye hentes til analysebok
#'
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
#' @inheritParams NIRFigAndeler
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for: 
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NIRVarTilretteleggPaaror  <- function(RegData, valgtVar, grVar='ShNavn', figurtype='andeler'){
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
      KImaaltxt=''
      varTxt <- 'hendelser'

      minald <- 0
      tittel <- 'Mangler tittel' 
      variable <- 'Ingen'
      #deltittel <- ''
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
      
      
      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'	
            tittel <- 'Alder ved innleggelse'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            sortAvtagende <- FALSE
      }
      
#      '', 'BeslutningTot','FSICUtot'
      if (valgtVar == 'OmsorgTot') {  #gjsnGrVar
            RegData$Variabel  <- RegData$OmsorgTot
            tittel <- c('Totalskår m.h.t. omsorg')
      } 
      
      if (valgtVar == 'BehandlingHoeflighetRespektMedfoelelseSkaar') {  #gjsnGrVar
        RegData$Variabel  <- RegData$BehandlingHoeflighetRespektMedfoelelseSkaar
        tittel <- c('Hvor godt ble pasienten ivaretatt', 
                    'med hensyn til høflighet, respekt og medfølelse?')
      } 
      # ""         "SymptomSmerteSkaar"                                 
      # [3] "SymptomPustebesvaerSkaar"                            "SymptomUroSkaar"                                    
      # [5] "BehandlingBesvarerBehovSkaar"                        "BehandlingBesvarerStoetteSkaar"                     
      # [7] "BehandlingSamarbeidSkaar"                            "BehandlingBesvarerHoeflighetRespektMedfoelelseSkaar"
      # [9] "SykepleierOmsorgSkaar"                               "SykepleierKommunikasjonSkaar"                       
      # [11] "LegeBehandlingSkaar"                                 "AtmosfaerenIntensivAvdSkaar"                        
      # [13] "AtmosfaerenPaaroerenderomSkaar"                      "OmfangetAvBehandlingenSkaar"
      # 
      # LegeInformasjonFrekvensSkaar      "SvarPaaSpoersmaalSkaar"            "ForklaringForstaaelseSkaar"       
      # [4] "InformasjonsAerlighetSkaar"        "InformasjonOmForloepSkaar"         "InformasjonsOverensstemmelseSkaar"
      # [7] "BeslutningsInvolveringSkaar"       "BeslutningsStoetteSkaar"           "BeslutningsKontrollSkaar"         
      # [10] "BeslutningsTidSkaar"               "LivsLengdeSkaar"                   "LivssluttKomforSkaar"      
      
      # tittel <- switch(valgtVar,
      #                  
      #                  BehandlingHoeflighetRespektMedfoelelse = ,
      #                  SymptomSmerte = c('Hvor godt vurderte og behandlet intensivpersonalet ', 
      #                                    'symptomene til pasienten med hensyn til smerte'),
      #                  SymptomPustebesvaer = c('Hvor godt vurderte og behandlet intensivpersonalet ', 
      #                                          'symptomene til pasienten med hensyn til pustebesvær'),
      #                  SymptomUro = c('Hvor godt vurderte og behandlet intensivpersonalet ', 
      #                                 'symptomene til pasienten med hensyn til uro'),
      #                  BehandlingBesvarerBehov	= 'Hvor godt viste intensivpersonalet interesse for dine behov?',
      #                  BehandlingBesvarerStoette = c('Hvor god var den følelsesmessige støtten', 
      #                                                'som du fikk av intensivpersonalet?'),
      #                  BehandlingSamarbeid = c('Hvordan samarbeidet intensivpersonalet som ivaretok', 
      #                                          'og behandlet pasienten?'),
      #                  BehandlingBesvarerHoeflighetRespektMedfoelelse = c('Hvordan ble du møtt av intensivpersonalet', 
      #                                                                     'med hensyn til høflighet, respekt og medfølelse?'),
      #                  SykepleierOmsorg = 'Hvor godt synes du sykepleierne ivaretok pasienten?',
      #                  SykepleierKommunikasjon = 'Hvor ofte snakket sykepleierne med deg om pasientens tilstand?',
      #                  LegeBehandling = 'Hvor godt synes du legene ivaretok pasienten?',
      #                  AtmosfaerenIntensivAvd = 'Atmosfæren i intensivavdelingen var:',
      #                  AtmosfaerenPaaroerenderom = 'Atmosfæren på pårørenderommet/venterommet var:',
      #                  OmfangetAvBehandlingen =	c('Hvor tilfreds var du med nivå eller omfang av', 
      #                                             'pleie og behandling som pasienten fikk på intensivavdelingen?'),
      #                  LegeInformasjonFrekvens	= 'Hvor ofte snakket legene med deg om pasientens tilstand?',
      #                  SvarPaaSpoersmaal	= 'Hvor villig var intensivpersonalet til å svare på dine spørsmål?',
      #                  ForklaringForstaaelse = 'Hvor godt klarte intensivpersonalet å gi deg forklaringer som du forsto?',
      #                  InformasjonsAerlighet = c('Hvor ærlig synes du informasjonen du fikk', 
      #                                            'om tilstanden til pasienten var?'),
      #                  InformasjonOmForloep = c('Hvor godt ble du informert om hva som skjedde med pasienten', 
      #                                           'og hvorfor ting ble gjort?'),
      #                  InformasjonsOverensstemmelse = c('Hvor stor overensstemmelse var det i informasjonen', 
      #                                                   'du fikk om tilstanden til pasienten?')
      # )
      # 
      # 
      
      if (valgtVar == 'BeslutningsInvolvering') { #Del2[7]
        #-1:5
        tittel <-  'Følte du deg involvert i beslutningsprosessen?'	
        grtxt <- c('veldig utelatt', 'noe utelatt', 'verken eller', 'noe involvert', 
                   'veldig involvert', '','Ikke svart') #paste0('Jeg følte meg ', )
      }
      if (valgtVar == 'BeslutningsStoette') { #Del2[8]
        #-1:5
        tittel <-  'Følte du at du fikk støtte når beslutningene ble tatt?'	
        grtxt <- c('ikke fikk støtte', 'fikk liten støtte', 'fikk en viss støtte', 
                   'fikk støtte', 'fikk mye støtte', '','Ikke svart') #paste0('Jeg følte at jeg ', )
      }
      
      if (valgtVar == 'BeslutningsKontroll') { #Del2[9]
        #-1:5
        tittel <- 'Følte du at du hadde innflytelse på den behandlingen som ditt familiemedlem fikk?'	
        grtxt <- c('helt uten innflytelse', 'liten innflytelse', 'verken eller', 
                   'en viss innflytelse', 'god innflytelse', '','Ikke svart')
      }
      if (valgtVar == 'BeslutningsTid') { #Del2[10]
        #-1:2
        tittel <- c('Når beslutninger skulle tas, hadde du tilstrekkelig med tid til ', 
                    'å uttrykke dine bekymringer og få besvart dine spørsmål?')
        grtxt <- c('trengte mer tid', 'tilstrekkelig med tid', '','Ikke svart')
        koder <- c(1,2,8,9)
      }
      if (valgtVar == 'LivsLengde') { #Del2[11], #-1:5
        tittel <- 'Hvilket utsagn beskriver best din oppfatning \nang. livet til pasienten:'
        grtxt <- c('unødvendig forlenget', 'forlenget litt lenger enn nødvendig', 
                   'passe', 'forkortet litt mer enn nødvendig', 'unødvendig forkortet')
      }
      if (valgtVar == 'LivssluttKomfor') { #Del2[12] #-1:5
        tittel <- 'Under de siste timene av livet til pasienten, 
                  \nhvilket utsagn beskriver best din oppfatning om hvordan han/hun hadde det:'
        grtxt <- c('ukomfortabelt', 'noe ukomfortabelt', 'stort sett komfortabelt',
                   'svært komfortabelt', 'fullstendig komfortabelt')
      }
      if (valgtVar == 'LivssluttStoette') { #Del2[13] #-1:5
        tittel <- 'Under de siste timene før pasienten døde, 
                  \nhvordan følte at du ble involvert beslutningsprosessen?'
        grtxt <- c('veldig utelatt', 'noe utelatt', 'verken eller', 'noe involvert', 'veldig involvert')
      }
      
      
      
      UtData <- list(RegData=RegData, minald=minald,
                     grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, KImaaltxt=KImaaltxt, 
                     retn=retn,tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData)) 
      
}
