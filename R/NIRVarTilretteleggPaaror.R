#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen. 
#' Her kan mye hentes til analysebok
#'
#'    				
#' @inheritParams NIRFigAndeler
#' @inheritParams NIRUtvalgEnh
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for: 
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NIRVarTilretteleggPaaror  <- function(RegData, valgtVar, grVar='ShNavn', figurtype='andeler'){

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

      
      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      # For hver valgtVar:
      # Definer og gjør utvalg for variabelen
      # tittel, xAkseTxt, sortAvtagende (standard: TRUE)
      
#DEFINERTE VARIABLE:
            Del1 <- c('BehandlingHoeflighetRespektMedfoelelse',
                'SymptomSmerte',
                'SymptomPustebesvaer',
                'SymptomUro',
                'BehandlingBesvarerBehov',
                'BehandlingBesvarerStoette',
                'BehandlingSamarbeid',
                'BehandlingBesvarerHoeflighetRespektMedfoelelse',
                'SykepleierOmsorg',
                'SykepleierKommunikasjon',
                'LegeBehandling',
                'AtmosfaerenIntensivAvd',
                'AtmosfaerenPaaroerenderom',
                'OmfangetAvBehandlingen')
      Del2 <- c('LegeInformasjonFrekvens',
                'SvarPaaSpoersmaal',
                'ForklaringForstaaelse',
                'InformasjonsAerlighet',
                'InformasjonOmForloep',
                'InformasjonsOverensstemmelse',
                'BeslutningsInvolvering',
                'BeslutningsStoette',
                'BeslutningsKontroll',
                'BeslutningsTid',
                'LivsLengde',
                'LivssluttKomfor',
                'LivssluttStoette')
 
      tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen
      verdiTom <- -1
      
      library(magrittr)
      #KodebokPaarorende <- read.table(file='C:/ResultattjenesteGIT/intensiv/doc/KodebokPaarorende.csv', 
      #                                header=T, sep=';', stringsAsFactors = F) #encoding = 'UTF-8',
      #VarNavnKodebok <- KodebokPaarorende$Variabelnavn[which(KodebokPaarorende$Variabelnavn != "")]
      

 #AUTOMATISK TILRETTELEGGING AV VARIABLE VHA KODEBOK     
       if (valgtVar %in% c(Del1, Del2)) {
      #       indKodebok <- which(KodebokPaarorende$Variabelnavn == valgtVar):(
      #             which(KodebokPaarorende$Variabelnavn == VarNavnKodebok[which(VarNavnKodebok==valgtVar)+1])-1)
      #   #grtxtDum <- KodebokPaarorende$Mulige.verdier[indKodebok] 
      #   #splitt <- stringr::str_split(grtxtDum, pattern = ' = ')
      #   #matr <- rlist::list.rbind(splitt)
      #   grInfo <- KodebokPaarorende$Mulige.verdier[indKodebok] %>% stringr::str_split(pattern=' = ') %>% rlist::list.rbind()
      #  #    if(grInfo[1,1] == verdiTom) {grInfo <- grInfo[-1,]}
      #   gr <- grInfo[,1]
      #   grtxt <- grInfo[,2]
      #   #dum <- stringi::stri_locate_all(pattern=' = ', grtxtDum, fixed = T) #Finner start og sluttindex for tekstmønster
      #   #posTxt <- rlist::list.rbind(dum) #Konverterer liste til matrise
      #   tittel <- KodebokPaarorende$Feltnavn[indKodebok[1]]
      #   RegData$VariabelGr <- factor(RegData[,valgtVar], levels=gr, labels=grtxt)
      # } 
      
      tittel <- switch(valgtVar,
                       BehandlingHoeflighetRespektMedfoelelse = c('Hvordan ble pasienten møtt av intensivpersonalet', 
                                                                  ' med hensyn til høflighet, respekt og medfølelse?'),
                       SymptomSmerte = c('Hvor godt vurderte og behandlet intensivpersonalet ',
                                         'symptomene til pasienten med hensyn til smerte?'),
                       SymptomPustebesvaer = c('Hvor godt vurderte og behandlet intensivpersonalet ',
                                               'symptomene til pasienten med hensyn til pustebesvær?'),
                       SymptomUro = c('Hvor godt vurderte og behandlet intensivpersonalet ',
                                      'symptomene til pasienten med hensyn til uro?'),
                       BehandlingBesvarerBehov	= 'Hvor godt viste intensivpersonalet interesse for dine behov?',
                       BehandlingBesvarerStoette = c('Hvor god var den følelsesmessige støtten',
                                                     'som du fikk av intensivpersonalet?'),
                       BehandlingSamarbeid = c('Hvordan samarbeidet intensivpersonalet som ivaretok',
                                               'og behandlet pasienten?'),
                       BehandlingBesvarerHoeflighetRespektMedfoelelse = c('Hvordan ble du møtt av intensivpersonalet',
                                                                          'med hensyn til høflighet, respekt og medfølelse?'),
                       SykepleierOmsorg = 'Hvor godt synes du sykepleierne ivaretok pasienten?',
                       SykepleierKommunikasjon = 'Hvor ofte snakket sykepleierne med deg om pasientens tilstand?',
                       LegeBehandling = 'Hvor godt synes du legene ivaretok pasienten?',
                       AtmosfaerenIntensivAvd = 'Atmosfæren i intensivavdelingen var:',
                       AtmosfaerenPaaroerenderom = 'Atmosfæren på pårørenderommet/venterommet var:',
                       OmfangetAvBehandlingen =	c('Hvor tilfreds var du med nivå eller omfang av',
                                                  'pleie og behandling som pasienten fikk på intensivavdelingen?'),
                       LegeInformasjonFrekvens	= 'Hvor ofte snakket legene med deg om pasientens tilstand?',
                       SvarPaaSpoersmaal	= 'Hvor villig var intensivpersonalet til å svare på dine spørsmål?',
                       ForklaringForstaaelse = 'Hvor godt klarte intensivpersonalet å gi deg forklaringer som du forsto?',
                       InformasjonsAerlighet = c('Hvor ærlig synes du informasjonen du fikk',
                                                 'om tilstanden til pasienten var?'),
                       InformasjonOmForloep = c('Hvor godt ble du informert om hva som skjedde med pasienten',
                                                'og hvorfor ting ble gjort?'),
                       InformasjonsOverensstemmelse = c('Hvor stor overensstemmelse var det i informasjonen',
                                                        'du fikk om tilstanden til pasienten?')
      )


      

      if (valgtVar %in% c(Del1[c(1:9,11:13)],Del2[c(2:6)])) {
            # -1 = Velg verdi	1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	6 = Ikke aktuelt
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Fremragende', 'Meget godt', 'Godt', 'Noenlunde', 'Dårlig') #, '','Ikke svart')
      }
      if (valgtVar %in% c(Del1[10],Del2[1])) {
            #      -1:Velg verdi, 1:Svært ofte, 2: Ofte, 3: Av og til, 4: Sjelden, 5: Aldri, 6: Ikke relevant
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Svært ofte', 'Ofte', 'Av og til', 'Sjelden', 'Aldri')#, '','Ikke svart')
      }
      if (valgtVar %in% Del1[14]) {
            #-1 = Velg verdi	1 = Svært fornøyd	2 = Meget fornøyd	3 = Middels fornøyd	4 = Ganske misfornøyd	
            #5 = Svært misfornøyd	6 = Ikke relevant
            # 1 = Fremragende	2 = Meget godt	3 = Godt	4 = Noenlunde	5 = Dårlig	8:'', 9: = Ikke aktuelt
            grtxt <- c('Svært fornøyd', 'Meget fornøyd', 'Middels fornøyd',
                       'Ganske misfornøyd', 'Svært misfornøyd')#, '','Ikke svart')
      }
      
}
       
       if (valgtVar == 'BeslutningsInvolvering') { #Del2[7]
             #-1:5
             tittel <-  'Følte du deg involvert i beslutningsprosessen?'	
             grtxt <- c('veldig utelatt', 'noe utelatt', 'verken eller', 'noe involvert', 
                        'veldig involvert')#, '','Ikke svart') #paste0('Jeg følte meg ', )
       }
       if (valgtVar == 'BeslutningsStoette') { #Del2[8]
             #-1:5
             tittel <-  'Følte du at du fikk støtte når beslutningene ble tatt?'	
             grtxt <- c('ikke støtte', 'liten støtte', 'en viss støtte', 
                        'støtte', 'mye støtte')#, '','Ikke svart') #paste0('Jeg følte at jeg ', )
       }
       if (valgtVar == 'BeslutningsKontroll') { #Del2[9]
             #-1:5
             tittel <- 'Følte du at du hadde innflytelse på den behandlingen \nsom ditt familiemedlem fikk?'	
             grtxt <- c('helt uten innflytelse', 'liten innflytelse', 'verken eller', 
                        'en viss innflytelse', 'god innflytelse')#, '','Ikke svart')
       }
       if (valgtVar == 'BeslutningsTid') { #Del2[10]
             #-1:2
             tittel <- c('Når beslutninger skulle tas, hadde du tilstrekkelig med tid til ', 
                         'å uttrykke dine bekymringer og få besvart dine spørsmål?')
             grtxt <- c('trengte mer tid', 'tilstrekkelig med tid')
             #koder <- c(1,2,8,9)
       }
       if (valgtVar == 'LivsLengde') { #Del2[11], #-1:5
             tittel <- 'Hvilket utsagn beskriver best din oppfatning \nang. livet til pasienten:'
             grtxt <- c('unødvendig forlenget', 'forlenget litt mer enn nødvendig', 
                        'passe', 'forkortet litt mer enn nødvendig', 'unødvendig forkortet')
       }
       if (valgtVar == 'LivssluttKomfor') { #Del2[12] #-1:5
             tittel <- 'Under de siste timene av livet til pasienten, hvilket utsagn 
             beskriver best din oppfatning om hvordan han/hun hadde det:'
             grtxt <- c('ukomfortabelt', 'noe ukomfortabelt', 'stort sett \nkomfortabelt',
                        'svært komfortabelt', 'fullstendig komfortabelt')
       }
       if (valgtVar == 'LivssluttStoette') { #Del2[13] #-1:5
             tittel <- 'Under de siste timene før pasienten døde, hvordan \nfølte at du ble involvert beslutningsprosessen?'
             grtxt <- c('veldig utelatt', 'noe utelatt', 'verken eller', 'noe involvert', 'veldig involvert')
       }
      grtxt <- c(grtxt, '','Ikke svart')
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr[RegData$VariabelGr %in% c(-1,6)] <- 9
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels=c(1:(length(grtxt)-2),8:9))
      
      
       if (valgtVar %in% c('SumScoreSatisfactionCare', 'SumScoreSatisfactionDecision', 'SumScoreAllQuestions')) {  #gjsnGrVar
             RegData <- RegData[which(RegData[,valgtVar] >= 0), ]    #Tar bort alder<0
             if (figurtype == 'andeler') {	#Fordelingsfigur
                   RegData$Variabel  <- RegData[,valgtVar]
                   gr <- c(seq(0, 90, 10),100)		
                   RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)	
                   grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-100')
                   xAkseTxt <- 'sumskår'}
             tittel <- switch (valgtVar,
                               SumScoreSatisfactionCare  = 'Totalskår, omsorg',
                               SumScoreSatisfactionDecision = 'Totalskår, beslutningsmedvirkning',
                               SumScoreAllQuestions = 'Totalskår')
       }
       
       
          
      UtData <- list(RegData=RegData, minald=minald,
                     grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, KImaaltxt=KImaaltxt, 
                     retn=retn,tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData)) 
      
}
