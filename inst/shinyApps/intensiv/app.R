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


# Define UI for application that draws figures
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  #span("Tab1", title="Short description  for the tab") ,
  #title = regTitle,
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",
  
  tabPanel(p("Oversiktsside", 
             title= 'Nøkkeltall og samlerapporter'),
           #fluidRow(
           #column(width=5,
           h2('Velkommen til Rapporteket-Intensiv!', align='center'),
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
             h2("Nøkkeltall på intensiv"),
             selectInput(inputId = 'enhetsNivaaStart', label='Enhetsnivå',
                           choices = c("Egen enhet"=2, "Hele landet"=0, 
                                       "Egen sykehustype"=4, "Egen region"=7)
               ),
             tableOutput('tabNokkeltallStart')
           ),
           
           tabPanel('Brukerveiledning',
                    h4('På Rapporteket kan du finne visualiseringer og oppsummeringer av de fleste variable 
                    som registreres i registeret. Hold musepekeren over en fane for å se hvilke variable/tema 
                    som er visualisert i fanen. 
                  Fanene er i hovedsak organisert ut fra hvordan resultatene er visualisert.'),
                    br(),
                    h4(tags$b('Månedsrapport ol.'), 'Vi må huske å skrive noe om hvordan man bestiller 
                       månedsrapport...!'),
                    br(),
                    h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                    h4('I feltet til venstre på hver side kan du velge hvilken variabel du ønsker å se
                            resultater for. Der kan du også gjøre ulike filtreringer/utvalg av data.'),
                    h4(tags$b('Aktivitet '), 'viser oversikt over registreringer og aktivitet'),
                    h4(tags$b('Fordelinger '), 'viser på fordelinger (figur/tabell) av ulike variable.'),
                    h4(tags$b('Andeler'), ' viser andeler(prosent) en per sykehus og utvikling over tid.
                            du kan velge hvilken tidsskala du vi se på. Her finner du resultater av typen 
                            "andel under 80 år" eller "andel opphold hvor pasienten døde".'),
                    h4(tags$b('Gjennomsnitt'), ' viser gjennomsnittsverdier per sykehus og utvikling over tid.
                            Du kan velge om du vil se gjennomsnitt eller median. Her finner du resultater som 
                       "gjennomsnittsalder" eller median respiratortid.'),
                    h4(tags$b('SMR '), 'viser SMR per sykehus.'),
                    h4(tags$b('Type opphold'), 'viser en figur med fordeling av oppholdstyper'),
                    h4(tags$b('Pasientskjema'), 'viser resultater fra pårørendeundersøkelser 
                       registrert i skjemaet FS-ICU'),
                    br(),
                    h4('Gi gjerne innspill til registerledelsen om det er resultater/tabeller/figurer du savner
                            på Rapporteket-Intensiv')
                    )
             )#tabset
           )#main
  ), #tab
  
  #-----Registreringsoversikter------------
  tabPanel(p("Aktivitet", title='Tabeller med registreringsoversikter, samt nøkkeltall'),
           #evt: span("Tab1",title="Short description  for the tab" )
           sidebarPanel(width=3,
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
                          condition = "input.ark == 'Dobbeltregistreringar'",
                          dateRangeInput(inputId = 'datovalgReg', start = startDato, end = idag,
                                         label = "Tidsperiode", separator="t.o.m.", language="nb")
                        )
                        
           ),
           mainPanel(
             tabsetPanel(id='ark',
                         tabPanel('Ant. opphold',
                                  h2("Antall opphold per avdeling"),
                                  p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                  tableOutput("tabAntOpphSh")
                         ),
                         tabPanel('Pasientar per år og avd.',
                                  h2("Antall pasienter ved avdelingene siste 5 år"),
                                  tableOutput("tabAntPasSh5Aar")
                         ),
                         tabPanel('Nøkkeltall',
                                  h2("Nøkkeltall på intensiv"),
                                  br(),
                                  tableOutput('tabNokkeltall')
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
  
  
  #-------Fordelinger----------      
  
  tabPanel(p("Fordelinger", 
             title='Alder, Innkomstmåte, Hemodynamisk overvåkning, Isolasjon, Liggetid, Nas, NEMS, Nyrebehandling,
                 Primærårsak, Respiratortid, SAPSII, Spesielle tiltak, Donorer'),
           #"Fordelinger",
           # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
           sidebarPanel(
             width = 3,
             h4('Her kan man velge hvilken variabel man ønsker å se resultater for og gjøre ulike filtreringer.'),
             br(),
             selectInput(
               inputId = "valgtVar", label="Velg variabel",
               choices = c('Alder' = 'alder', 
                           'Bukleie' = 'bukleie',
                           'Innkomstmåte' = 'InnMaate',
                           'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
                           'Inklusjonskriterier' = 'inklKrit',
                           'Isolasjon, type' = 'isolering',
                           'Isolasjon, varighet' = 'isoleringDogn',
                           'Liggetid' = 'liggetid',
                           'Nas-skår (sykepleierakt.)' = 'Nas24',
                           'NEMS-skår (ressursbruk)' = 'NEMS24',
                           'Nyrebeh., type' = 'nyreBeh',
                           'Nyrebeh., varighet' = 'nyreBehTid',
                           'Primærårsak' = 'PrimaryReasonAdmitted',
                           'Respiratortid' = 'respiratortid',
                           'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                           'Respiratortid, invasiv m/overf.' = 'respiratortidInvMoverf',
                           'Respiratortid, invasiv u/overf.' = 'respiratortidInvUoverf',
                           'SAPSII-skår (alvorlighet av sykd.)' = 'SAPSII',
                           'Spesielle tiltak' = 'spesTiltak',
                           'Potensielle donorer, årsak ikke påvist opph. sirkulasjon' = 'CerebralCirculationAbolishedReasonForNo',
                           'Årsak, ikke donasjon ved opphevet intrakraniell sirk.' = 'OrganDonationCompletedReasonForNoStatus'
               )
             ),
             dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMann", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
             ),
             sliderInput(inputId="alder", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             ),
             selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                         choices = c("Egen mot resten av landet"=1, 
                                     "Hele landet"=0, 
                                     "Egen enhet"=2,
                                     "Egen enhet mot egen sykehustype" = 3,
                                     "Egen sykehustype" = 4,
                                     "Egen sykehustype mot resten av landet" = 5,
                                     "Egen enhet mot egen region" = 6, 
                                     "Egen region" = 7,
                                     "Egen region mot resten" = 8)
             )
             #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
             #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
           ),
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
           h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
           h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer.", align='center'),
           br(),
           br(),
           sidebarPanel(
             width=3,
             selectInput(
               inputId = "valgtVarAndelGrVar", label="Velg variabel",
               choices = c('Alder minst 80 år' = 'alder_over80',
                           'Alder under 18år' = 'alder_u18',
                           'Bukleie' = 'bukleie',
                           'Død innen 30 dager' = 'dod30d',
                           'Død innen ett år' = 'dod365d',
                           'Døde på intensiv' = 'dodeIntensiv',
                           'Isolasjon av pasient' = 'isolering',
                           'Liggetid, døde' = 'liggetidDod',
                           'Menn' = 'erMann',
                           'Nyrebehandling' = 'nyreBeh',
                           'Reinnleggelse' = 'reinn',
                           'Respiratorstøtte' = 'respStotte',
                           'Respiratortid, inv. < 2,5d m/overf.' = 'respiratortidInvMoverf',
                           'Respiratortid, inv. < 2,5d u/overf.' = 'respiratortidInvUoverf',
                           'Respiratortid, døde' = 'respiratortidDod',
                           'Utenfor vakttid, innlagt' = 'utenforVakttidInn',
                           'Utenfor vakttid, utskrevet' = 'utenforVakttidUt',
                           'Utvidet hemodyn. overvåkning' = 'ExtendedHemodynamicMonitoring',
                           'Trakeostomi' = 'trakeostomi',
                           'Trakeostomi, åpen' = 'trakAapen',
                           'Døde som ble donorer' = 'OrganDonationCompletedStatus',
                           'Donorer, opphevet intrakran. sirkulajon' = 'OrganDonationCompletedCirc')
             ), 
             dateRangeInput(inputId = 'datovalgAndelGrVar', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannAndelGrVar", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)),
             sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)),
             br(),
             p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
             selectInput(inputId = 'enhetsUtvalgAndelTid', label='Egen enhet og/eller landet',
                         choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
             selectInput(inputId = "tidsenhetAndelTid", label="Velg tidsenhet",
                         choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                         'Kvartal'='Kvartal', 'Måned'='Mnd')))
           ),
           mainPanel(
             # fluidRow(column(6, plotOutput("andelTid"))),
             # br(),
             # br(),
             # fluidRow(
             #       column(6, plotOutput("andelerGrVar") ) #, div(style = "height:100px")) #height='1000px') # '400px'
             # )   
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
           
  ), #tab
  
  
  #------- Gjennomsnitt ----------      
  tabPanel(p("Gjennomsnitt", title = 'Alder, Liggetid, Nas, NEMS, Respiratortid, SAPSII'),
           h2("Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel", align='center'),
           h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                  til venstre. (Man kan også gjøre ulike filtreringer.)", align='center'),
           sidebarPanel( 
             width = 3,
             selectInput(inputId = "valgtVarGjsn", label="Velg variabel",
                         choices = c('Alder' = 'alder',
                                     'Liggetid' = 'liggetid',
                                     'Nas-skår (sykepleierakt.)' = 'Nas24',
                                     'NEMS-skår (ressursbruk)' = 'NEMS24',
                                     'NEMS-skår per opphold' = 'NEMS',
                                     'Respiratortid' = 'respiratortid',
                                     'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                                     'Respiratortid, invasiv m/overf.' = 'respiratortidInvMoverf',
                                     'Respiratortid, invasiv u/overf.' = 'respiratortidInvUoverf',
                                     'SAPSII-skår (alvorlighetsgrad)' = 'SAPSII'
                         )
             ),
             dateRangeInput(inputId = 'datovalgGjsn', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannGjsn", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
             ),
             sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             ),
             selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                         choices = c("Gjennomsnitt"='Gjsn', "Median"='Med')),
             br(),
             p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
             selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                         choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
             ),
             selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet",
                         choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                         'Kvartal'='Kvartal', 'Måned'='Mnd'))
             )
           ), #sidebarPanel/kolonna til venstre
           mainPanel(
             br(),
             tabsetPanel(
               tabPanel("Figurer",
                        plotOutput("gjsnTid"),
                        plotOutput("gjsnGrVar")),
               tabPanel("Tabeller",
                        uiOutput("tittelGjsn"),
                        br(),
                        column(width = 3, 
                               h3("Sykehusvise resultater"),
                               tableOutput("tabGjsnGrVar"),
                               downloadButton(outputId = 'lastNed_tabGjsnGrVar', label='Last ned tabell')),
                        column(width = 1),
                        column(width = 5,
                               h3("Utvikling over tid"),
                               tableOutput("tabGjsnTid"),
                               downloadButton(outputId = 'lastNed_tabGjsnTid', label='Last ned tabell')) )
             )
           )
  ),
  
  #--------SMR--------------
  tabPanel('SMR',
           h3('SMR: Standardisert mortalitetsratio', align='center'),
           br(),
           sidebarPanel(
             width = 3,
             dateRangeInput(inputId = 'datovalgSMR', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannSMR", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
             ),
             sliderInput(inputId="alderSMR", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             )
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Figur",
                        plotOutput("SMRfig")),
               tabPanel("Tabell",
                        uiOutput("tittelSMR"),
                        br(),
                        tableOutput("SMRtab"))
             )
           )
  ), #tab
  
  #--------Type opphold--------------
  tabPanel('Type opphold',
           h3('Type opphold'),
           sidebarPanel(
             width = 3,
             dateRangeInput(inputId = 'datovalgInnMaate', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannInnMaate", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
             ),
             sliderInput(inputId="alderInnMaate", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             )
           ),
           mainPanel(
             plotOutput('innMaate')
           )
  ), #tab
  
  #-------Pårørendeskjema----------      
  tabPanel(p("Pasientskjema", title='Enkeltspørsmål fra FS-ICU, samt totalskårer'),
           h2('Resultater fra Pårørendeskjema (FS-ICU)', align = 'center'),
           # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
           sidebarPanel(
             width = 3,
             selectInput(
               inputId = "valgtVarPaarorFord", label="Velg variabel",
               choices = c('S1.1 Pasient, høflighet og medfølelse' = 'BehandlingHoeflighetRespektMedfoelelse',
                           'S1.2 Smerte' = 'SymptomSmerte',
                           'S1.3 Pustebesvær' = 'SymptomPustebesvaer',
                           'S1.4 Uro' = 'SymptomUro',
                           'S1.5 Interesse for behov' = 'BehandlingBesvarerBehov',
                           'S1.6 Følelsesmessig støtte' = 'BehandlingBesvarerStoette',
                           'S1.7 Samarbeid' = 'BehandlingSamarbeid',
                           'S1.8 Pårørende, høflighet og medfølelse' = 'BehandlingBesvarerHoeflighetRespektMedfoelelse',
                           'S1.9 Omsorg, sykepleier' = 'SykepleierOmsorg',
                           'S1.10 Kommunikasjon, sykepleier' = 'SykepleierKommunikasjon',
                           'S1.11 Omsorg, lege' = 'LegeBehandling',
                           'S1.12 Atmosfære på avd.' = 'AtmosfaerenIntensivAvd',
                           'S1.13 Atmosfære, venterom' = 'AtmosfaerenPaaroerenderom',
                           'S1.14 Omfang av behandling' = 'OmfangetAvBehandlingen',
                           'S2.1 Legens informasjonsfrekvens' = 'LegeInformasjonFrekvens',
                           'S2.2 Svarvillighet, personale' = 'SvarPaaSpoersmaal',
                           'S2.3 Forståelige forklaringer' = 'ForklaringForstaaelse',
                           'S2.4 Informasjon, ærlighet' = 'InformasjonsAerlighet',
                           'S2.5 Informasjon' = 'InformasjonOmForloep',
                           'S2.6 Informasjon, overensstemmelse' = 'InformasjonsOverensstemmelse',
                           'S2.7 Beslutningsprosess, involvering' = 'BeslutningsInvolvering',
                           'S2.8 Beslutningsprosess, støtte' = 'BeslutningsStoette',
                           'S2.9 Beslutningsprosess, innflytelse' = 'BeslutningsKontroll',
                           'S2.10 Beslutningsprosess, tid' = 'BeslutningsTid',
                           'S2.11 Livslengde' = 'LivsLengde',
                           'S2.12 Komfort ved livsslutt, pasient' = 'LivssluttKomfor',
                           'S2.13 Involvering ved livsslutt' = 'LivssluttStoette',
                           'Totalskår, omsorg (skjema 1)' = 'SumScoreSatisfactionCare', 
                           'Totalskår, beslutning (skjema 2)' = 'SumScoreSatisfactionDecision', 
                           'Totalskår, alle spørsmål' = 'SumScoreAllQuestions')
             ),
             dateInput(inputId = 'startDatoIntervensjon', label = 'Startdato, intervensjon', language="nb",
                       value = '2016-10-01', max = Sys.Date()),
             dateRangeInput(inputId = 'datovalgPaarorFord', start = "2015-01-01", end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannPaarorFord", label="Kjønn, pasient",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)),
             h3('Utvalg vedrørende den pårørende (alder, kjønn, relasjon,...)?')
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel(
                 'Figur',
                 plotOutput('paarorFord')),
               tabPanel(
                 'Tabell',
                 h3('Her kommer en tabell')
                 #uiOutput("tittelFord"),
                 #tableOutput('fordelingTabPaaror')
               )
             )
           )
  ), #tab Pårørende
  
  
  tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("subscriptionRep", "Rapport:",
                                      c("Månedsrapport", "Samlerapport", "Influensaresultater")),
                          selectInput("subscriptionFreq", "Frekvens:",
                                      list(Årlig="Årlig-year",
                                            Kvartalsvis="Kvartalsvis-quarter",
                                            Månedlig="Månedlig-month",
                                            Ukentlig="Ukentlig-week",
                                            Daglig="Daglig-DSTday"),
                                      selected = "Månedlig-month"),
                          #selectInput("subscriptionFileFormat", "Format:",
                          #            c("html", "pdf")),
                          actionButton("subscribe", "Bestill!")
             ),
             mainPanel(
               uiOutput("subscriptionContent")
             )
           )
  )

  
  
  
  
  
  
  
  


#------------Influensa-----------------------------
# tabPanel(p("Inluensa", title='Resultater fra influensaregistrering'),
#          h2('Resultater fra influensaregistrering', align = 'center'),    
#  mainPanel(
#  )        
# )

)  #navbarPage


#----------------- Define server logic ----------
server <- function(input, output, session) { #
  
#-----------Div serveroppstart------------------  
  raplog::appLogger(session = session, msg = "Starter intensiv-app")
      
  reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 109773)})
  rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
  brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(shinySession=session), 'tullebukk')})
  #userRole <- reactive({ifelse(onServer, rapbase::getUserRole(session), 'SC')})
  #output$reshID <- renderText({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)}) #evt renderUI
  
  # observe({if (rolle() != 'SC') { #
  #   shinyjs::hide(id = 'velgResh')
  #   shinyjs::hide(id = 'velgReshKval')
  #   #hideTab(inputId = "tabs_andeler", target = "Figur, sykehusvisning")
  # }
  # })
  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle(), '<br> ReshID: ', reshID()) )}
  
  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })
  
#---------Hente data  
  if (paaServer) {
    RegData <- NIRRegDataSQL(datoFra='2015-01-01', session = session) #datoFra = datoFra, datoTil = datoTil)
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
  
  
      #--------startside--------------      
#      output$tekstDash <- c('Figurer med kvalitetsindikatorer',
#                           'hente ned månedsrapport'),
#  funksjon for å kjøre Rnw-filer (render file funksjon)
  # contentFile <- function(file, srcFil, tmpFile,
  #                         reshID=0, datoFra=startDato, datoTil=Sys.Date()) {
  #   src <- normalizePath(system.file(srcFil, package="intensiv"))
  #  #dev.off()
  #   # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #   owd <- setwd(tempdir())
  #   on.exit(setwd(owd))
  #   file.copy(src, tmpFile, overwrite = TRUE)
  # 
  #   knitr::knit2pdf(tmpFile)
  # 
  #   gc() #Opprydning gc-"garbage collection"
  #   file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  #   # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  # }
  #Erstattes av henteSamlerapporter()

  output$mndRapp.pdf <- downloadHandler(
    filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRmndRapp.Rnw", 
                          reshID = reshID(), datoFra = startDato)
    }
  )
  
    # output$mndRapp.pdf <- downloadHandler(
  #   filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')}, #'MndRapp.pdf',
  #   content = function(file){
  #     contentFile(file, srcFil="NIRmndRapp.Rnw", tmpFile="tmpNIRmndRapp.Rnw",
  #                 reshID = reshID(), datoFra = startDato)
  #   }
  # )

  output$samleRapp.pdf <- downloadHandler(
    filename = function(){ paste0('NIRsamleRapp', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRSamleRapp.Rnw",
                  reshID = reshID(), datoFra = startDato)
    }
  )
  
  output$influensaRapp.pdf <- downloadHandler(
    filename = function(){ paste0('NIRinfluensa', Sys.time(), '.pdf')}, 
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRinfluensa.Rnw")
    }
  )
  
  
#   out <- rmarkdown::render(tmpFile, output_format = pdf_document(),
#     params = list(tableFormat="latex",
#     hospitalName=hospitalName,
#     reshId=reshId,
#     year=input$yearSet,
#     session=session
#   ), output_dir = tempdir())
#   # active garbage collection to prevent memory hogging?
#   gc()
#   file.rename(out, file)
# }
      
      #------------ Aktivitet (/Tabeller) --------
 # observe({
 
   output$tabNokkeltallStart <- function() {
    tab <- t(tabNokkeltall(RegData=RegData, tidsenhet='Mnd',
                           enhetsUtvalg=as.numeric(input$enhetsNivaaStart), reshID=reshID()))
    kableExtra::kable(tab,
                      full_width=F,
                      digits = c(0,0,0,1,0,1,1,0,0,0,1,1,2,1)
    ) %>%
      column_spec(column = 1, width_min = '4em', width_max = 10) %>%
      column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
      row_spec(0, bold = T, align = 'c') %>%
      kable_styling(full_width = FALSE, position = 'left') #"hover",
  }
  
  output$tabNokkeltall <- function() {#renderTable({
            tab <- t(tabNokkeltall(RegData=RegData, tidsenhet=input$tidsenhetReg, datoTil=input$sluttDatoReg, 
                      enhetsUtvalg=as.numeric(input$enhetsNivaa), reshID=reshID()))
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
      
      output$tabDblReg <- renderTable({
        tabDBL <- finnDblReg(RegData, reshID=reshID()) #tabDBL <- 
        finnDblReg(RegData, reshID=reshID())
        #tabDBL <- knitr::kable(tabDBL, format='html', row.names = NA)
      }, spacing="xs") #rownames = T, 

      
      output$inklKrit <- renderPlot({
        NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar='inklKrit',
                      reshID=reshID(), enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                      datoFra=input$datovalg[1], datoTil=input$datovalg[2], session=session)
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )
  #})
    #------------Fordelinger---------------------  
      output$fordelinger <- renderPlot({
            NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                                      reshID=reshID(), enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                                      datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                                      minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                                      erMann=as.numeric(input$erMann), session = session)
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )
      
      observe({      
            UtDataFord <- NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                        reshID=reshID(), enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                        datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                        erMann=as.numeric(input$erMann), lagFig = 0, session = session)
            #NIRFigAndeler(RegData=RegData, preprosess = 0, reshID=109773, enhetsUtvalg=1 ) 
            tab <- lagTabavFig(UtDataFraFig = UtDataFord)

            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
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
                               datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                               minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                               erMann=as.numeric(input$erMannAndelGrVar))
      }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
      )
      
            output$andelTid <- renderPlot({
                  
                  NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                 reshID=reshID(),
                                 datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                                 minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                                 erMann=as.numeric(input$erMannAndelGrVar),
                                 tidsenhet = input$tidsenhetAndelTid,
                                 enhetsUtvalg = input$enhetsUtvalgAndelTid)
            }, height = 300, width = 1000
            )
            
            observe({
                  #AndelTid
                  AndelerTid <- NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                               reshID=reshID(),
                                               datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                                               minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                                               erMann=as.numeric(input$erMannAndelGrVar),
                                               tidsenhet = input$tidsenhetAndelTid,
                                               enhetsUtvalg = input$enhetsUtvalgAndelTid, 
                                               lagFig=0)
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
                  AndelerShus <- NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                                    datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                                                    minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                                                    erMann=as.numeric(input$erMannAndelGrVar, lagFig = 0))
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
                                    h3(AndelerShus$tittel),
                                    h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
                              )}) #, align='center'
            }) #observe
            
             
  #--------------Gjennomsnitt---------------          
       output$gjsnGrVar <- renderPlot({
            NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                            datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                            minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                            erMann=as.numeric(input$erMannGjsn),
                            valgtMaal = input$sentralmaal)
      }, height=900, width=700
      )
      
      output$gjsnTid <- renderPlot({
            NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                          reshID=reshID(),
                          datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                          minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                          erMann=as.numeric(input$erMannGjsn), 
                          valgtMaal = input$sentralmaal,
                          tidsenhet = input$tidsenhetGjsn,
                          enhetsUtvalg = input$enhetsUtvalgGjsn)
      }, height=400, width = 1200
      )
      
      observe({
        dataUtGjsnGrVar <- NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                           datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                           minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                           erMann=as.numeric(input$erMannGjsn),
                                           valgtMaal = input$sentralmaal, lagFig = 0)
        tabGjsnGrVar <- cbind(Antall = dataUtGjsnGrVar$Ngr$Hoved,
                              Sentralmål = dataUtGjsnGrVar$AggVerdier$Hoved)
        colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')
        
        output$tabGjsnGrVar <- function() {
          kableExtra::kable(tabGjsnGrVar, format = 'html'
                            , full_width=F
                            , digits = c(0,1) #,1,1)[1:antKol]
          ) %>%
            column_spec(column = 1, width_min = '7em') %>%
            column_spec(column = 2:3, width = '7em') %>%
            row_spec(0, bold = T)
        }
        
        output$lastNed_tabGjsnGrVar <- downloadHandler(
          filename = function(){
            paste0(input$valgtVar, '_gjsnGrVar.csv')
          },
          content = function(file, filename){
            write.csv2(tabGjsnGrVar, file, row.names = T, na = '')
          })
        
        output$tittelGjsn <- renderUI(
          tagList(
            h3(dataUtGjsnGrVar$tittel),
            br(),
            h5(HTML(paste0(dataUtGjsnGrVar$utvalgTxt, '<br />')))
          ))
        dataUtGjsnTid <- NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                       reshID=reshID(),
                                       datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                       minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                       erMann=as.numeric(input$erMannGjsn), 
                                       valgtMaal = input$sentralmaal,
                                       tidsenhet = input$tidsenhetGjsn,
                                       enhetsUtvalg = input$enhetsUtvalgGjsn) #, lagFig=0)
        #dataUtGjsnTid <- NIRFigGjsnTid(RegData=RegData, preprosess = 0, reshID=reshID, datoFra = '2019-01-01')
        tabGjsnTid <- t(dataUtGjsnTid$AggVerdier)
        grtxt <-dataUtGjsnTid$grtxt
        if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
          grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
        rownames(tabGjsnTid) <- grtxt
        antKol <- ncol(tabGjsnTid)
        navnKol <- colnames(tabGjsnTid) 
        if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}
        
        output$tabGjsnTid <- function() {
          kableExtra::kable(tabGjsnTid, format = 'html'
                            , full_width=F
                            , digits = 1 #c(0,1,1,1)[1:antKol]
          ) %>%
            add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
            #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
            column_spec(column = 1, width_min = '7em') %>%
            column_spec(column = 2:(antKol+1), width = '7em') %>%
            row_spec(0, bold = T)
        }
        output$lastNed_tabGjsnTid <- downloadHandler(
          filename = function(){
            paste0(input$valgtVar, '_gjsnTid.csv')
          },
          content = function(file, filename){
            write.csv2(tabGjsnTid, file, row.names = T, na = '')
          })
        
      })
      
      #--------------SMR----------------------------------
      output$SMRfig <- renderPlot({
        NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar='SMR',
                        datoFra=input$datovalgSMR[1], datoTil=input$datovalgSMR[2],
                        minald=as.numeric(input$alderSMR[1]), maxald=as.numeric(input$alderSMR[2]),
                        erMann=as.numeric(input$erMannSMR))
      }, #heigth = 8000, width=800
      height = function() {2.2*session$clientData$output_SMRfig_height}, #
      width = function() {0.8*session$clientData$output_SMRfig_width}
      )
      
      observe({
        dataUtSMR <- NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar='SMR',
                                     datoFra=input$datovalgSMR[1], datoTil=input$datovalgSMR[2],
                                     minald=as.numeric(input$alderSMR[1]), maxald=as.numeric(input$alderSMR[2]),
                                     erMann=as.numeric(input$erMannSMR), lagFig = 0)
        output$SMRtab <- function() {
          tabSMR <- cbind(Antall = dataUtSMR$Ngr$Hoved,
                          SMR = dataUtSMR$AggVerdier$Hoved)
          #colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')
          
          kableExtra::kable(tabSMR, format = 'html'
                            , full_width=F
                            , digits = c(0,2) #,1,1)[1:antKol]
          ) %>%
            column_spec(column = 1, width_min = '7em') %>%
            column_spec(column = 2:3, width = '7em') %>%
            row_spec(0, bold = T)
        }
        output$tittelSMR <- renderUI(
          tagList(
            h3(dataUtSMR$tittel),
            br(),
            h5(HTML(paste0(dataUtSMR$utvalgTxt, '<br />')))
          ))
        
      })
      
      output$innMaate <- renderPlot({
        NIRFigInnMaate(RegData=RegData, preprosess=0, valgtVar='InnMaate', 
                       datoFra=input$datovalgInnMaate[1], datoTil=input$datovalgInnMaate[2],
                       minald=as.numeric(input$alderInnMaate[1]), maxald=as.numeric(input$alderInnMaate[2]),
                       erMann=as.numeric(input$erMannInnMaate))
      }, height = function() {2.2*session$clientData$output_innMaate_height},
      width = function() {0.7*session$clientData$output_innMaate_width}) #, height=900, width=700)
      
      
      output$paarorFord <- renderPlot(
        NIRFigPrePostPaaror(RegData=PaarorData, preprosess = 0, valgtVar=input$valgtVarPaarorFord,
                            startDatoIntervensjon = input$startDatoIntervensjon,
                            datoFra=input$datovalgPaarorFord[1], datoTil=input$datovalgPaarorFord[2],
                            erMann=as.numeric(input$erMannPaarorFord)
        ), width=800, height = 800 #execOnResize=TRUE, 
      )
    
      
      
#------------------ Abonnement ----------------------------------------------
      ## reaktive verdier for å holde rede på endringer som skjer mens
      ## applikasjonen kjører
      rv <- reactiveValues(
        subscriptionTab = rapbase::makeUserSubscriptionTab(session))
      
      ## lag tabell over gjeldende status for abonnement
      output$activeSubscriptions <- DT::renderDataTable(
        rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
        rownames = FALSE, options = list(dom = 't')
      )
      
      ## lag side som viser status for abonnement, også når det ikke finnes noen
      output$subscriptionContent <- renderUI({
        fullName <- rapbase::getUserFullName(session)
        if (length(rv$subscriptionTab) == 0) {
          p(paste("Ingen aktive abonnement for", fullName))
        } else {
          tagList(
            p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                    rapbase::getUserEmail(session), ":")),
            DT::dataTableOutput("activeSubscriptions")
          )
        }
      })
      
      ## nye abonnement
      observeEvent (input$subscribe, { #MÅ HA
        package <- "intensiv"
        owner <- rapbase::getUserName(session)
        interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
        intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
        organization <- rapbase::getUserReshId(session)
        runDayOfYear <- rapbase::makeRunDayOfYearSequence(
          interval = interval
        )
        email <- rapbase::getUserEmail(session)
        if (input$subscriptionRep == "Månedsrapport") {
          synopsis <- "Intensiv/Rapporteket: månedsrapport"
          rnwFil <- "NIRmndRapp.Rnw" #Navn på fila
          print(rnwFil)
        }
        #"Månedsrapport", "Samlerapport", "Influensaresultater"
        #NIRSamleRapp.Rnw og NIRinfluensa.Rnw
        # if (input$subscriptionRep == "Stentbruk, månedlig") {
        #   synopsis <- "NORIC/Rapporteket: stentbruk, månedlig"
        #   baseName <- "NORIC_local_monthly_stent"
        # }
        
        
        fun <- "abonnement"  #"henteSamlerapporter"
        paramNames <- c('rnwFil', 'brukernavn', "reshID", "datoFra", 'datoTil')
        paramValues <- c(rnwFil, brukernavn(), reshID(), startDato, Sys.Date()) #input$subscriptionFileFormat)
        #abonnement('NIRmndRapp.Rnw')
        
        rapbase::createAutoReport(synopsis = synopsis, package = package,
                                  fun = fun, paramNames = paramNames,
                                  paramValues = paramValues, owner = owner,
                                  email = email, organization = organization,
                                  runDayOfYear = runDayOfYear, interval = interval,
                                  intervalName = intervalName)
        rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      })
      
      ## slett eksisterende abonnement
      observeEvent(input$del_button, {
        selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
        rapbase::deleteAutoReport(selectedRepId)
        rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      })

      
      
      
        
} #serverdel

# Run the application
shinyApp(ui = ui, server = server)

