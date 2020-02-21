# Shiny-app for Norsk Intensivregister
#NB: For å få lagt ut app'en på Shinyapps, må Github-pakkene (intensiv og rapbase) være installert fra Github.
#devtools::install_github(ref = 'rel', repo = 'Rapporteket/intensiv')
library(intensiv)
library(shiny)
library(lubridate)
library(zoo)
library(kableExtra)
library(knitr)

addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

options(knitr.table.format = "html")
idag <- Sys.Date() #as.Date('2018-11-30') #
datoTil <- as.POSIXlt(idag)
aarFra <- paste0(1900+as.POSIXlt(idag)$year-5, '-01-01')
startDato <- paste0(as.numeric(format(idag-90, "%Y")), '-01-01') #paste0(1900+as.POSIXlt(idag)$year, '-01-01')
AarNaa <- as.numeric(format(idag, "%Y"))

regTitle <- ifelse(paaServer, 
                   'NORSK INTENSIVREGISTER',
                   'Norsk Intensivregister med FIKTIVE data')

#Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
#ibrary(shinyBS) # Additional Bootstrap Controls

#---------Hente data------------
if (paaServer) {
  RegData <- NIRRegDataSQL(datoFra='2018-08-01') #, session = session) #datoFra = datoFra, datoTil = datoTil)
  PaarorData <- NIRpaarorDataSQL() 
  PaarorDataH <- KobleMedHoved(RegData, PaarorData, alleHovedskjema=F, alleSkjema2=F)
  qInfluensa <- 'SELECT ShNavn, RHF, PatientInRegistryGuid, FormDate,FormStatus, ICD10_1
                  from InfluensaFormDataContract'
  InfluData <- rapbase::LoadRegData(registryName= "nir", query=qInfluensa, dbType="mysql")
  
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
} #hente data på server

if (!exists('PaarorDataH')){
  data('NIRRegDataSyn', package = 'intensiv')
  #try(data(package = "intensiv"))
}
RegData <- NIRPreprosess(RegData = RegData)
PaarorData <- NIRPreprosess(RegData = PaarorDataH) #Må først koble på hoveddata for å få ShType++


#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------

#Definere utvalgsinnhold
#sykehusNavn <- sort(c('',unique(RegData$ShNavn)), index.return=T)
#sykehusValg <- c(0,unique(RegData$ReshId))[sykehusNavn$ix]
sykehusNavn <- sort(unique(RegData$ShNavn), index.return=T)
sykehusValg <- unique(RegData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Alle',sykehusNavn$x)

enhetsUtvalg <- c("Egen mot resten av landet"=1, 
                  "Hele landet"=0, 
                  "Egen enhet"=2,
                  "Egen enhet mot egen sykehustype" = 3,
                  "Egen sykehustype" = 4,
                  "Egen sykehustype mot resten av landet" = 5,
                  "Egen enhet mot egen region" = 6, 
                  "Egen region" = 7,
                  "Egen region mot resten" = 8)

valgFordeling<- c('Alder' = 'alder', 
                  'Bukleie' = 'bukleie',
                  'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
                  'Frailty index' = 'frailtyIndex',
                  'Inklusjonskriterier' = 'inklKrit',
                  'Isolasjon, type' = 'isolering',
                  'Isolasjon, varighet' = 'isoleringDogn',
                  'Liggetid' = 'liggetid',
                  'Nas-skår (sykepleierakt.)' = 'Nas24',
                  'NEMS-skår (ressursbruk)' = 'NEMS24',
                  'Nyreerstattende beh., type' = 'nyreBeh',
                  'Nyreerstattende beh., varighet' = 'nyreBehTid',
                  'Potensielle donorer, årsak ikke påvist opph. sirkulasjon' = 'CerebralCirculationAbolishedReasonForNo',
                  'Primærårsak' = 'PrimaryReasonAdmitted',
                  'Respiratortid' = 'respiratortid',
                  'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                  'Respiratortid, invasiv m/overf.' = 'respiratortidInvMoverf',
                  'Respiratortid, invasiv u/overf.' = 'respiratortidInvUoverf',
                  'SAPSII-skår (alvorlighet av sykd.)' = 'SAPSII',
                  'SAPSII-skår (uten alderspoeng)' = 'SAPSIIuAlder',
                  'Spesielle tiltak' = 'spesTiltak',
                  'Type opphold' = 'InnMaate',
                  'Årsak, ikke donasjon ved opphevet intrakraniell sirk.' = 'OrganDonationCompletedReasonForNoStatus'
)
valgAndeler <- c('Alder minst 80 år' = 'alder_over80',
                 'Alder under 18år' = 'alder_u18',
                 'Bukleie' = 'bukleie',
                 'Døde innen 30 dager' = 'dod30d',
                 'Døde innen 90 dager' = 'dod90d',
                 'Døde innen ett år' = 'dod365d',
                 'Døde på intensiv' = 'dodeIntensiv',
                 'Invasiv respiratortid < 2,5 døgn, m/overførte' = 'respiratortidInvMoverf',
                 'Invasiv respiratortid < 2,5 døgn, u/overførte' = 'respiratortidInvUoverf',
                 'Isolasjon av pasient' = 'isolering',
                 'Liggetid, døde' = 'liggetidDod',
                 'Menn' = 'erMann',
                 'Nyreerstattende behandling' = 'nyreBeh',
                 'Organdonorer, av døde' = 'OrganDonationCompletedStatus',
                 'Organdonorer, av alle med opphevet intrakran. sirk.' = 'OrganDonationCompletedCirc',
                 'Reinnleggelse' = 'reinn',
                 'Respiratorstøtte' = 'respStotte',
                 'Respiratortid, døde' = 'respiratortidDod',
                 'Utenfor vakttid, innlagt' = 'utenforVakttidInn',
                 'Utenfor vakttid, utskrevet' = 'utenforVakttidUt',
                 'Utvidet hemodyn. overvåkning' = 'ExtendedHemodynamicMonitoring',
                 'Trakeostomi' = 'trakeostomi',
                 'Trakeostomi, åpen' = 'trakAapen')


# Define UI for application that draws figures
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  #span("Tab1", title="Short description  for the tab") ,
  #title = regTitle,
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",
  id = 'hovedark',

  
  
  
  #--------------Startside------------------------------  
  tabPanel(p("Oversiktsside", 
             title= 'Nøkkeltall og samlerapporter'),
           #fluidRow(
           #column(width=5,
           h2('Velkommen til Rapporteket-Intensiv!', align='center'),
           br(),
           sidebarPanel(
             width = 3,
             h3('Dokumenter med samling av resultater'),
             h5('Disse kan man få regelmessig tilsendt på e-post. 
                Gå til fanen "Abonnement" for å bestille dette.'),
             br(),
             h3("Månedsrapport"), #),
             downloadButton(outputId = 'mndRapp.pdf', label='Last ned MÅNEDSRAPPORT', class = "butt"),
             tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
             br(),
             h3('Samlede resultater, egen enhet'),
             downloadButton(outputId = 'samleRapp.pdf', label='Last ned samlerapport', class = "butt"),
             br(),
             br(),
             br(),
             h3('Resultater fra influensaregistrering'),
             downloadButton(outputId = 'influensaRapp.pdf', label='Last ned influensarapport', class = "butt")
           ),
           mainPanel(
             shinyalert::useShinyalert(),
             appNavbarUserWidget(user = uiOutput("appUserName"),
                                 organization = uiOutput("appOrgName"),
                                 addUserInfo = TRUE),
             tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
             
             tabsetPanel(
               tabPanel('Startside',
             h3(ifelse(paaServer, "","Merk at noen resultater kan se rare ut siden dette er syntetiske data!"), align='center' ),
             #h2("Nøkkeltall på intensiv"),
             h3(uiOutput('NokkeltallUtvalgTxt')),
             selectInput(inputId = 'enhetsNivaaStart', label='Enhetsnivå',
                           choices = c("Egen enhet"=2, "Hele landet"=0, 
                                       "Egen sykehustype"=4, "Egen region"=7)
               ),
             tableOutput('tabNokkeltallStart'), 
             br('Se neste fane "Aktivitet" for å se nærmere på nøkkeltall')
           ),
           
           tabPanel('Brukerveiledning',
                    h4('På Rapporteket kan du finne visualiseringer og oppsummeringer av de fleste 
                       variabler som registreres i registeret. Hold musepekeren over en fane for 
                       å se hvilke variable/tema som er visualisert i fanen.'),
                    br(),
                    h4(tags$b('Abonnement på rapporter'), 'Dersom du går i fanen «Abonnement» helt 
                       til høyre kan du bestille leveranse av utvalgte samlerapporter til e-postkassen 
                       din til faste tider. Du kan laste ned de samme rapportene direkte ved å klikke 
                       på knappene i venstre marg på denne startsiden.'),
                    br(),
                    h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                    h4('Generelt finner du i hver fane et felt til venstre hvor du kan velge hvilken 
                       variabel du ønsker å se resultater for. Der kan du også gjøre ulike 
                       filtreringer/utvalg av data. Du kan også velge om du vil se resultater 
                       kun fra egen enhet, fra hele landet, eller egen enhet sammenlignet mot for 
                       eksempel liknende enheter. I underfanene kan du som regel velge om du vil 
                       se resultatene som figur eller tabell.'),
                    h4(tags$b('Aktivitet '), 'viser informasjon om antall registreringer og aktivitet 
                       i avdelingen.'),
                    h4(tags$b('Fordelinger '), 'viser hvordan verdiene på valgt variabel fordeler seg. 
                       For eksempel kan du se en fordeling av alder som viser antall pasienter 
                       i ulike alderskategorier.'),
                    h4(tags$b('Andeler'), ' viser andeler(prosent) av valgt parameter per sykehus, 
                       og utvikling av andelen over tid. Du kan velge hvilken tidsskala du vi se på. 
                       Her finner du resultater av typen "andel under 80 år" eller "andel opphold 
                       hvor pasienten døde".'),
                    h4(tags$b('Gjennomsnitt'), ' viser gjennomsnittsverdier for valgt variabel 
                       per sykehus og utvikling over tid for den samme variabelen. Du kan velge 
                       om du vil se gjennomsnitt eller median. Her finner du resultater som 
                       "gjennomsnittsalder" eller "median respiratortid".'),
                    h4(tags$b('SMR '), 'viser SMR per sykehus. Dette er faktisk dødelighet 
                       delt på estimert dødelighet ut fra SAPS-skår.'),
                    h4(tags$b('Type opphold'), 'viser en figur med fordeling av oppholdstyper.'),
                    h4(tags$b('Pasientskjema'), 'viser resultater fra pårørendeundersøkelser 
                       registrert i skjemaet FS-ICU.'),
                    br(),
                    h4('Gi gjerne innspill til registerledelsen om det er resultater/tabeller/figurer du savner
                            på Rapporteket-Intensiv.')
                    )
             )#tabset
           )#main
  ), #tab
  
  #-----Registreringsoversikter------------
  tabPanel(p("Aktivitet", title='Tabeller med registreringsoversikter, samt nøkkeltall'),
           #evt: span("Tab1",title="Short description  for the tab" )
           sidebarPanel(width=3,
                        br(),
                        br(),
                        br(),
                        
                        conditionalPanel(condition = "input.ark == 'Nøkkeltall' || input.ark == 'Ant. opphold' 
                                             || input.ark == 'Pasientar per år og avd.' ", 
                                         # || input.ark == 'Inklusjonskriterier' ",
                                         dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                   value = Sys.Date(), max = Sys.Date())
                        ),
                        # selectInput(inputId = "tidsenhet", label="Velg tidsenhet",
                        #             choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                        #                             'Kvartal'='Kvartal', 'Måned'='Mnd'))),
                        conditionalPanel(
                          condition = "input.ark == 'Nøkkeltall' || input.ark == 'Ant. opphold'",
                          selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                      choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                        conditionalPanel(
                          condition = "input.ark == 'Nøkkeltall'", #  || input.ark == 'Inklusjonskriterier' ",
                          selectInput(inputId = 'enhetsNivaa', label='Enhetsnivå',
                                      choices = c("Hele landet"=0, "Egen enhet"=2, 
                                                  "Egen sykehustype"=4, "Egen region"=7)
                          )),
                        conditionalPanel(
                          condition = "input.ark == 'Dobbeltregistreringar' || 'input.ark == 'Overføringer'",
                          dateRangeInput(inputId = 'datovalgReg', start = startDato, end = idag,
                                         label = "Tidsperiode", separator="t.o.m.", language="nb")
                        )
                        
           ),
           mainPanel(
             tabsetPanel(id='ark',
                         tabPanel('Ant. opphold',
                                  h2("Antall opphold per avdeling"),
                                  h3('ferdigstilte inntil forrige døgn'),
                                  p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                  tableOutput("tabAntOpphSh")
                         ),
                         tabPanel('Pasienter per år og avd.',
                                  h2("Antall pasienter ved avdelingene siste 5 år"),
                                  tableOutput("tabAntPasSh5Aar")
                         ),
                         tabPanel('Nøkkeltall',
                                  h2('Nøkkeltall på intensiv'),
                                  #h2(uiOutput('NokkeltallTxt'), align='center'), 
                                  br(),
                                  tableOutput('tabNokkeltall')
                         ),
                         tabPanel('Overføringer',
                                  p(h2('Overføring av intensivpasienter til/fra ', uiOutput('egetShNavn'),
                                     align='center') ), 
                                  #h2(uiOutput('egetShNavn')),
                                  br(),
                                  column(6,
                                         h4('Overføringer TIL '),
                                  tableOutput('tabOverfFra')),
                                  column(6,
                                         h4('Overføringer FRA'),
                                         tableOutput('tabOverfTil'))
                         ),
                         # tabPanel('Inklusjonskriterier',
                         #   tabsetPanel(
                         #     tabPanel('Figur',
                         #              plotOutput('inklKrit')) #,
                         #     # tabPanel(
                         #     #   'Tabell',
                         #     #   uiOutput("tittelFord"),
                         #     #   tableOutput('fordelingTab'))
                         #   ),
                         tabPanel('Dobbeltregistreringer',
                                  h2("Mulige dobbeltregistreringer"),
                                  tableOutput("tabDblReg")
                         )
             )
           )
  ), #tab
 
  #   #-------Utvalg, figurer (felles sidebarpanel)----------    
  
  #-----sidebarPanel(-----
  sidebarPanel(
    width = 3,
    h4('FELLES: Her kan man velge hvilken variabel man ønsker å se resultater for og gjøre ulike filtreringer.'),
    br(),
    
    print(interactive()),
    
    selectInput("plotType", "Plot Type",
                c(Scatter = "scatter", Histogram = "hist")
    ),
    # Only show this panel if the plot type is a histogram
    conditionalPanel(
      condition = "input.plotType == 'hist'",
      selectInput(
        "breaks", "Breaks",
        c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
      ),
    ),
    
     conditionalPanel(condition = "input.hovedark == 'andeler'",
                     selectInput(inputId = "valgtVar", label="Velg variabel",
                                 choices = valgFordeling)),
       conditionalPanel(condition = "input.hovedark == 'fordelinger'",               
         selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                                 choices = valgAndeler)),
     dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                     label = "Tidsperiode", separator="t.o.m.", language="nb"),
      selectInput(inputId = "erMann", label="Kjønn",
                  choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
      ),
    sliderInput(inputId="alder", label = "Alder", min = 0,
                  max = 110, value = c(0, 110)
      ),
      selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                    choices = enhetsUtvalg
        ),
    
    
  #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
      #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
    #),
    br(),
    # conditionalPanel(
    #   condition = "input.ark == 'Andeler' || 'input.ark == 'Gjennomsnitt'",
      p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
    selectInput(inputId = "tidsenhet", label="Velg tidsenhet",
                choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                'Kvartal'='Kvartal', 'Måned'='Mnd')))
  ), #felles sidebarPanel
  
  
  
  
  #-------Fordelinger----------      
  
  
  tabPanel(p("Fordelinger", 
             title='Alder, Type opphold, Hemodynamisk overvåkning, Isolasjon, Liggetid, Nas, NEMS, Nyrebehandling,
              Primærårsak, Respiratortid, SAPSII, Spesielle tiltak, Donorer'),
           value="fordelinger",
           h2("Fordelingsfigurer", align='center'),
           # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())

           mainPanel(
             tabsetPanel(
               tabPanel(
                 'Figur',
                 plotOutput('fordelinger')),
               tabPanel(
                 'Tabell',
                 uiOutput("tittelFord"),
                 tableOutput('fordelingTab'),
                 downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell') #, class = "butt")
               )
             )
           )
  ), #tab Fordelinger
  
  # mainPanel(tabsetPanel(id = "tabs_andeler",
  #                       tabPanel("Figur, tidssvisning",
  #                                plotOutput("fig_andel_tid", height="auto"),
  #                                downloadButton("lastNedBilde_tid", "Last ned figur")),
  #                       tabPanel("Tabell, tidssvisning",
  #                                uiOutput("utvalg_tid"),
  #                                tableOutput("Tabell_tid"), downloadButton("lastNed_tid", "Last ned tabell")),
  #                       tabPanel("Figur, sykehusvisning",
  #                                plotOutput("fig_andel_grvar", height="auto"),
  #                                downloadButton("lastNedBilde_sykehus_andel", "Last ned figur")),
  #                       tabPanel("Tabell, sykehusvisning",
  #                                uiOutput("utvalg_sykehus_andel"),
  #                                tableOutput("Tabell_sykehus_andel"), downloadButton("lastNed_sykehus_andel", "Last ned tabell"))
  # )),
  
  #-------Andeler----------      
  tabPanel(p("Andeler", title='Alder, Overlevelse, Isolasjon, Nyrebehandling, Reinnleggelse, Respiratorstøtte, Respiratortid,
                Utenfor vakttid, Hemodyn., Takeostomi, Donorer'),
           value = "andeler",
           h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
           h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer.", align='center'),
           br(),
           br(),
           mainPanel(
             tabsetPanel(
               tabPanel(
                 "Figurer",
                 #column(10,
                 h3(em("Utvikling over tid")),
                 plotOutput("andelTid", height = 'auto'),
                 br(),
                 h3(em("Sykehusvise resultater")),
                 plotOutput("andelerGrVar", height='auto')
               ),
               tabPanel("Tabeller",
                        uiOutput("tittelAndelGrVar"),
                        br(),
                        #fluidRow(
                        column(width = 3, 
                               h3("Sykehusvise resultater"),
                               tableOutput("andelerGrVarTab"),
                               downloadButton(outputId = 'lastNed_tabAndelGrVar', label='Last ned tabell')),
                        column(width = 1),
                        column(width = 5, 
                               h3("Utvikling over tid"),
                               tableOutput("andelTidTab"),
                               downloadButton(outputId = 'lastNed_tabAndelTid', label='Last ned tabell'))
                        #DT::DTOutput("andelerGrVarTab")
               ))
           ) #mainPanel
           
  ) #tab
  
  

)  #navbarPage


#----------------- Define server logic ----------
server <- function(input, output, session) { #
  
#-----------Div serveroppstart------------------  
  raplog::appLogger(session = session, msg = "Starter intensiv-app")
      
  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 109773)
  rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
  brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavn')})
  # reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 109773)})
  # rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
  # brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavn')})
  #userRole <- reactive({ifelse(onServer, rapbase::getUserRole(session), 'SC')})
  #output$reshID <- renderText({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)}) #evt renderUI
  #enhetsNavn <- reactive({ifelse(paaServer, rapbase::(shinySession=session), 'egen enhet')})
  indReshEgen <- match(reshID, RegData$ReshId)
  egetShNavn <- as.character(RegData$ShNavn[indReshEgen])
  egetRHF <- as.character(RegData$RHF[indReshEgen])
  egetHF <- as.character(RegData$HF[indReshEgen])
  egenShType <- c('lokal-/sentralsykehus', '', 
                       'universitetssykehus')[RegData$ShType[indReshEgen]]
  egenLokalitet <- c(0, 2, 4, 7)
  names(egenLokalitet) <- c('hele landet', egetShNavn, egenShType , egetRHF)
  output$egetShNavn <- renderText(egetShNavn)

  # observe({if (rolle() != 'SC') { #
  #   shinyjs::hide(id = 'velgResh')
  #   shinyjs::hide(id = 'velgReshKval')
  #   #hideTab(inputId = "tabs_andeler", target = "Figur, sykehusvisning")
  # }
  # })
  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle(), 
                                           '<br> ReshID: ', reshID,
                                           '<br> Enhet: ', egetShNavn) )}
  
  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })
  
  
      #--------startside--------------      
#      output$tekstDash <- c('Figurer med kvalitetsindikatorer',
#                           'hente ned månedsrapport'),

  output$mndRapp.pdf <- downloadHandler(
    filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRmndRapp.Rnw", 
                          reshID = reshID, datoFra = startDato)
    }
  )
  
  output$samleRapp.pdf <- downloadHandler(
    filename = function(){ paste0('NIRsamleRapp', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRSamleRapp.Rnw",
                  reshID = reshID, datoFra = startDato)
    }
  )
  
  output$influensaRapp.pdf <- downloadHandler(
    filename = function(){ paste0('NIRinfluensa', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRinfluensa.Rnw")
    }
  )
  
#------------ Aktivitet (/Tabeller) --------
 # observe({
  #TESTING
  # tab <- t(tabNokkeltall(RegData=RegData, tidsenhet='Mnd',
  #                        enhetsUtvalg=0, reshID=109773))#
  output$NokkeltallUtvalgTxt <- renderText({
    paste0('Nøkkeltall på intensiv, ', 
              as.character(names(egenLokalitet[which(egenLokalitet==as.numeric(input$enhetsNivaaStart))])))
    #paste0('Nøkkeltall på intensiv, ', as.character(names(egenLokalitet[which(egenLokalitet==4)])))
  })    
   output$tabNokkeltallStart <- function() {
    tab <- t(tabNokkeltall(RegData=RegData, tidsenhet='Mnd',
                           enhetsUtvalg=as.numeric(input$enhetsNivaaStart), reshID=reshID))
    kableExtra::kable(tab,
                      full_width=F,
                      digits = c(0,0,0,1,0,1,1,0,0,0,1,1,2,1)
    ) %>%
      column_spec(column = 1, width_min = '4em', width_max = 10) %>%
      column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
      row_spec(0, bold = T, align = 'c') %>%
      kable_styling(full_width = FALSE, position = 'left') #"hover",
  }
  
   #output$NokkeltallTxt <- renderText({paste0('Nøkkeltall på intensiv, ', egetShNavn)})
   output$tabNokkeltall <- function() {#renderTable({
            tab <- t(tabNokkeltall(RegData=RegData, tidsenhet=input$tidsenhetReg, 
                                   datoTil=input$sluttDatoReg, 
                      enhetsUtvalg=as.numeric(input$enhetsNivaa), reshID=reshID))
            #tab <- tabNokkeltall(RegData, tidsenhet='Mnd', datoTil, enhetsUtvalg=0, reshID=0)
            kableExtra::kable(tab, 
                              full_width=F, 
                              digits = c(0,0,0,1,0,1,1,0,0,0,1,1,2,1)
                             ) %>%
                  column_spec(column = 1, width_min = '4em', width_max = 10) %>%
                  #column_spec(column = 1, width = '4em') %>%
                  column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
                  #column_spec(column = 2:(ncol(tab)), width_min = '7em', width_max = '7em') %>%
                  row_spec(0, bold = T, align = 'c') %>%
                  kable_styling(full_width = FALSE, position = 'left') #"hover", 
                  
            
      }#,rownames=T, digits=0 )
      
      output$tabAntOpphSh <- renderTable({
            tab <- switch(input$tidsenhetReg,
                   Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])  
                   Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))
           
      }, rownames = T, digits=0, spacing="xs" 
      )
      
      output$tabAntPasSh5Aar <- renderTable({
            tabAntOpphPasSh5Aar(RegData=RegData, gr='pas', datoTil=input$sluttDatoReg)
      }, rownames = T, digits=0, spacing="xs")
      
      output$tabOverfFra <- renderTable({
        #tab <- tabOverforinger(RegData=RegData, reshID=reshID) 
        tab <- tabOverforinger(RegData=RegData, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2], 
                                    reshID=reshID, overfFraSh=1)
         xtable::xtable(tab) #, colnames=F)
      }, rownames = T)
      output$tabOverfTil <- renderTable({
        tab <- tabOverforinger(RegData=RegData, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2], 
                        reshID=reshID, overfFraSh=0)
        xtable::xtable(tab)
      }, rownames=T)
      
      output$tabDblReg <- renderTable({
        tabDBL <- finnDblReg(RegData, reshID=reshID) #tabDBL <- 
        finnDblReg(RegData, reshID=reshID)
        #tabDBL <- knitr::kable(tabDBL, format='html', row.names = NA)
      }, spacing="xs") #rownames = T, 

      
      output$inklKrit <- renderPlot({
        NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar='inklKrit',
                      reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                      datoFra=input$datovalg[1], datoTil=input$datovalg[2], session=session)
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )
  #})
#------------Fordelinger---------------------  
      output$fordelinger <- renderPlot({
            NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                                      reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                                      datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                                      minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                                      erMann=as.numeric(input$erMann), session = session)
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )
      
      observe({      
            UtDataFord <- NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                        reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                        datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                        erMann=as.numeric(input$erMann), lagFig = 0, session = session)
            #RegData <- NIRRegDataSQL(datoFra = '2018-01-01')
            #UtDataFord <- NIRFigAndeler(RegData=RegData, valgtVar='bukleie', reshID=109773, enhetsUtvalg=0 ) 
            tab <- lagTabavFig(UtDataFraFig = UtDataFord)

            output$tittelFord <- renderUI({
                  tagList(
                        h3(HTML(paste(UtDataFord$tittel, sep='<br />'))),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
                  
                  #       kable_styling("hover", full_width = F)
                  antKol <- ncol(tab)
                  kableExtra::kable(tab, format = 'html'
                                    , full_width=F
                                    , digits = c(0,1,0,1)[1:antKol]
                                    ) %>%
                        add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                        column_spec(column = 1, width_min = '7em') %>%
                        column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
                        row_spec(0, bold = T)
            }
            
            output$lastNed_tabFord <- downloadHandler(
              filename = function(){
                paste0(input$valgtVar, '_fordeling.csv')
              },
              content = function(file, filename){
                write.csv2(tab, file, row.names = F, na = '')
              })
            }) #observe
      
#---------Andeler-------------------------
      output$andelerGrVar <- renderPlot({
            NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                               erMann=as.numeric(input$erMann), session=session)
      }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
      )
      
            output$andelTid <- renderPlot({
                  
                  NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                 reshID=reshID,
                                 datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                 minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                 erMann=as.numeric(input$erMann),
                                 tidsenhet = input$tidsenhet,
                                 enhetsUtvalg = input$enhetsUtvalg,
                                 session=session)
            }, height = 300, width = 1000
            )
            
            observe({
                  #AndelTid
                  AndelerTid <- NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                               reshID=reshID,
                                               datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                               erMann=as.numeric(input$erMann),
                                               tidsenhet = input$tidsenhet,
                                               enhetsUtvalg = input$enhetsUtvalg, 
                                               lagFig=0, session=session)
                  tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid)


                  output$andelTidTab <- function() {
                        antKol <- ncol(tabAndelTid)
                        kableExtra::kable(tabAndelTid, format = 'html'
                                          , full_width=F
                                          , digits = c(0,1,0,1)[1:antKol]
                        ) %>%
                              add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                              column_spec(column = 1, width_min = '7em') %>%
                              column_spec(column = 2:(antKol+1), width = '7em') %>%
                              row_spec(0, bold = T)
                  }
                  output$lastNed_tabAndelTid <- downloadHandler(
                    filename = function(){
                      paste0(input$valgtVar, '_andelTid.csv')
                    },
                    content = function(file, filename){
                      write.csv2(tabAndelTid, file, row.names = T, na = '')
                    })
                  
                  
                  #AndelGrVar
                  AndelerShus <- NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                                    datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                                    minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                                    erMann=as.numeric(input$erMann), 
                                                    lagFig = 0, session=session)
                  tabAndelerShus <- cbind(Antall=AndelerShus$Ngr$Hoved,
                                          Andeler = AndelerShus$AggVerdier$Hoved)
                  
                  # output$andelerGrVarTab <- renderTable({ 
                  #       tabAndelerShus}, rownames=T, spacing="xs" #,height='60%' #width='60%', 
                  # )
                  output$andelerGrVarTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
                        antKol <- ncol(tabAndelerShus)
                        kableExtra::kable(tabAndelerShus, format = 'html'
                                          #, full_width=T
                                          , digits = c(0,1) #,0,1)[1:antKol]
                        ) %>%
                              column_spec(column = 1, width_min = '5em') %>%
                              column_spec(column = 2:(antKol+1), width = '4em') %>%
                              row_spec(0, bold = T)
                  }
                  output$lastNed_tabAndelGrVar <- downloadHandler(
                    filename = function(){
                      paste0(input$valgtVar, '_andelGrVar.csv')
                    },
                    content = function(file, filename){
                      write.csv2(tabAndelerShus, file, row.names = T, na = '')
                    })
                  
                  output$tittelAndelGrVar <- renderUI({
                              tagList(
                                    h3(HTML(paste(AndelerShus$tittel, sep = '<br />'))),
                                    h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
                              )}) #, align='center'
            }) #observe
            
             
} #serverdel

# Run the application
shinyApp(ui = ui, server = server)

