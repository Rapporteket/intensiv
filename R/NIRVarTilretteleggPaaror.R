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
#Nytt skjema tatt i bruk 7.nov 2023

  "%i%" <- intersect

  #----------- Figurparametre ------------------------------
  cexgr <- 1	#Kan endres for enkeltvariable
  retn <- 'H'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
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
  RegData$Variabel <- 0


  #--------------- Definere variable ------------------------------

  tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen
  verdiTom <- -1

  library(magrittr)

  #AUTOMATISK TILRETTELEGGING AV VARIABLE VHA KODEBOK
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

  varSmSvar <- c(
    'AtmosfaerenIntensivAvd_2',
    'AtmosfaerenPaaroerenderom_2',
    'BehandlingBesvarerBeho_2v',
    'BehandlingBesvarerHoeflighetRespektMedfoelelse_2',
    'BehandlingBesvarerStoette_2',
    'BehandlingHoeflighetRespektMedfoelelse_2',
    'BehandlingSamarbeid_2',
    'DeltagelseIOmsorg',
    'ForklaringForstaaelse_2',
    'InformasjonOmForloep_2',
    'InformasjonsAerlighet_2',
    'InformasjonsOverensstemmelse_2',
    'LegeBehandling_2',
    'LegeInformasjonFrekvens_2',
    'MengdenAvHelsetjenester',
    'OmfangetAvBehandlingen_2',
    'SvarPaaSpoersmaal_2',
    'SykepleierKommunikasjon_2',
    'SykepleierOmsorg_2',
    'SymptomPustebesvaer_2',
    'SymptomSmerte_2',
    'SymptomUro_2')


  tittel <- switch(valgtVar,
                   AtmosfaerenIntensivAvd_2 = 'Hvor fornøyd er du med stemningen i intensivavd.?',
                   AtmosfaerenPaaroerenderom_2	= 'Hvor fornøyd er du med stemningen på venterommet?',
                   BehandlingBesvarerBeho_2v	=  'Hvor godt viste intensivpersonalet interesse for dine behov?',
                   BehandlingBesvarerHoeflighetRespektMedfoelelse_2	= 'Hvilken høflighet, respekt og omtanke som ble vist deg?',
                   BehandlingBesvarerStoette_2	= 'Hvor god følelsesmessig støtte ga personalet deg?',
                   BehandlingHoeflighetRespektMedfoelelse_2	= 'Høflighet, respekt og medfølelse som ble vist pasienten',
                   BehandlingSamarbeid_2	= 'Samarbeidet mellom alt intensivpersonale som tok hand om pasienten',
                   DeltagelseIOmsorg	= 'Din deltakelse i omsorgen for ditt kritisk syke familiemedlem?',
                   ForklaringForstaaelse_2	= 'Hvor godt intensivpersonalet ga deg forklaringer som du forsto?',
                   InformasjonOmForloep_2 = 'Informasjon fra personalet om hva som hendte med pasienten?',
                   InformasjonsAerlighet_2 = 'Ærligheten i informasjon som ble gitt om pasientens tilstanden?',
                   InformasjonsOverensstemmelse_2 = 'Konsistensen i informasjonen som ble gitt om tilstanden til pasienten',
                   LegeBehandling_2 = 'Hvor godt ivaretok legene pasienten (færdighet og kompetanse)',
                   LegeInformasjonFrekvens_2 = 'Hyppigheten av kommunikasjon med legene i intensivavdelingen?',
                   MengdenAvHelsetjenester = 'Fornøydhet med NIVÅET/mengden av helsetjenester pasienten mottok på avd.?',
                   OmfangetAvBehandlingen_2 = 'Fornøydhet med din deltakelse i daglige legevisitter?',
                   SvarPaaSpoersmaal_2 = 'Personalets villighet til å svare på spørsmål?',
                   SykepleierKommunikasjon_2 = 'Hyppighet av intensivsykepleierne kommunikasjon om pasientens tilstanden?',
                   SykepleierOmsorg_2 = 'Hvor godt ivaretok intensivsykepleiere pasienten',
                   SymptomPustebesvaer_2 = 'Intensivpersonalet vurdering/behandling av pasientens pustevanskeligheter',
                   SymptomSmerte_2 = 'Intensivpersonalet vurdering/behandling av pasientens smerte',
                   SymptomUro_2 = 'Intensivpersonalets vurdering og behandling av pasientens uro/agitasjon'
  )

  if (valgtVar %in% varSmSvar){
    gr <- c(-1,1:6)
    grtxt <- c('Ikke besvart', 'Totalt misfornøyd', 'Litt misfornøyd', 'For det meste fornøyd',
               'Svært fornøyd', 'Totalt fornøyd', 'Ikke aktuelt')
  } else {

    if (valgtVar == 'LivssluttKomfor_2') {
      tittel <- 'De siste timene ditt familiemedlem levde, \n
             hvordan  tenker du at hen hadde det:'
      grtxt <- c('Svært ukomfortabelt', 'Noe ukomfortabelt',
                 'For det meste komfortabelt', 'Svært komfortabelt',
                 'Fullkomment komfortabelt')
    }


    if (valgtVar == 'BeslutningsStoette_2') {
      tittel <- 'Hvor mye støtte følte du at du fikk i prosessen med beslutningstaking?'
      grtxt <- c('Ingen  støtte', 'Liten støtte',
                 'Hverken eller', 'Fikk støtte', 'Mye støtte')
    }

    if (valgtVar == 'BeslutningsKontroll_2') {
      tittel <- 'Følte du at du hadde over kontroll ivaretakelsen av ditt familiemedlem?'
      grtxt <- c('Ingen kontroll', 'Ikke så mye kontroll',
                 'Hverken eller', 'Noe kontroll', 'God kontroll')
    }

    if (valgtVar == 'LivsLengde_2') {
      tittel <- 'Jeg føler at livet til mitt familiemedlem ble:'
      grtxt <- c('Unødvendig forlenget', 'Noe unødvendig forlenget',
                 'Hverken eller', 'Noe unødvendig forkortet', 'Unødvendig forkortet')
    }
    if (valgtVar == 'LivssluttStoette_2') {
      tittel <- 'I hvilken grad følte du deg støttet av helseteamet \n de siste timene av pasientens liv?'
      grtxt <- c('Svært forlatt', 'Forlatt',
                 'Hverken eller', 'Støttet', 'Svært støttet')
    }
    if (valgtVar == 'BeslutningsInvolvering_2') {
      tittel <- 'Følte du deg inkludert i beslutningstaking?'
      grtxt <- c('Veldig ekskludert', 'Noe ekskludert',
                 'Hverken eller', 'Noe inkludert', 'Veldig inkludert')
    }

    if (valgtVar == 'BeslutningsTid_2') {
      tittel <- 'Fikk du nok tid til å få svar på dine spørsmål ifm beslutninger som ble tatt'
      grtxt <- c('Behøvde mer tid', 'Noe utilstrekkelig','Tilstrekkelig',
                 'Mer enn nok', 'Mye mer enn nok')
    }

     gr <- 0:5
     grtxt <- c('Ikke besvart', grtxt)



    if (valgtVar == 'HoeyesteFullfoerteUtdannelse') {
      #-1:5         -1 = Velg verdi,
      tittel <- 'Høyeste nivå av fullført utdannelse'
      gr <- -1:4
      grtxt <- c('Ikke besvart',
                 'Ikke fullført grunnskole/vgs',
                 'Fullført grunnskole og vgs',
                 'Fullført yrkesutdanning',
                 'Universitetsgrad tilsv. bachelor',
                 'Master eller Doktorgrad')
    }
  }




  RegData$VariabelGr <- RegData[ ,valgtVar]
  #RegData$VariabelGr[RegData$VariabelGr %in% c(-1,6)] <- 9
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = gr) #c(1:(length(grtxt)-2),8:9))


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
  return(invisible(UtData))

}
