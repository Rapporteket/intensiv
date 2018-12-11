# Shiny-app for Norsk Intensivregister

library(shiny)
library(knitr)
library(intensiv)
library(lubridate)
library(zoo)
library(kableExtra)
#ibrary(shinyBS) # Additional Bootstrap Controls

# ui <- shinyUI(basicPage(
#   downloadButton('report')
# ))
#
# server <- function(input, output) {
#   output$report = downloadHandler(
#     filename = 'MndRapp.pdf',
#     content = function(file) {
#       out = knit2pdf('C:/ResultattjenesteGIT/Intensiv/inst/IntensivMndRapp.Rnw', encoding = 'UTF-8', clean = TRUE)
#       file.rename(out, file) # move pdf to file for downloading
#     },
#     contentType = 'application/pdf'
#   )
#
# }


# Define UI for application that draws figures
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = 'NORSK INTENSIVREGISTER',
      tabPanel("Viktigste resultater/Oversiktsside",
               #fluidRow(
               #column(width=5,
               h2("Månedsrapport"), #),
               downloadButton(outputId = 'mndRapp', label='Månedsrapport-virker ikke på server', class = "butt"),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               br(),
               br(),
               br(),
               br(),
               br(),
               h2("Her kan man evt. vise de variable/resultater som er viktigst å overvåke", align='center' ),
               h2("Gi tilbakemelding på hva som skal være på sida", align='center' ),
               br(),
               h3("Merk at noen resultater kan se rare ut siden det er syntetiske data!", align='center' ),
               br(),
               tags$ul(tags$b('Andre ting å ta stilling til: '),
                       tags$li("Er innhold i tabeller i tilknytning til figurer ok? Dvs. er det disse kolonnene dere ønsker?"),
                       tags$li("Foretrukket tittellayout på side - som på andeler eller gjennomsnitt?"), 
                       tags$li("Ønskes annen organisering av innhold? - NB: Vi kan ikke gjøre store endringer nå, 
                               men evt. ha en plan på sikt"), 
                       tags$li("Kun en figur på hver side, eller er det fint å vise to samtidig som under 'Andeler'? "),
                       tags$li("Hvilke utvalgs/filtreringsmuligheter skal vi ha i de ulike fanene"), 
                       tags$li("Hvilke navn ønskes på fanene?")
               ),
               br(),
               tags$ul(tags$b('Kommer: '),
                       tags$li('Overflyttinger mellom sykehus/avd.'),
                       tags$li("Fordelinger alder og kjønn - hvordan vil dere ha denne framstilt og hvor?")
               )
      ), #tab
      
#-----Registreringsoversikter------------
            tabPanel("Registreringsoversikter",
               sidebarPanel(width=3,
                            conditionalPanel(condition = "input.ark == 'Nøkkeltall' || input.ark == 'Ant. opphold'
                                             || input.ark == 'Pasientar per år og avd.' ",
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
                                  condition = "input.ark == 'Nøkkeltall'",
                                  selectInput(inputId = 'enhetsNivaa', label='Enhetsnivå',
                                              choices = c("Hele landet"=0, "Egen sykehustype"=4, "Egen enhet"=2)
                                  )),
                            conditionalPanel(
                                  condition = "input.ark == 'Dobbeltregistreringar'",
                                  dateRangeInput(inputId = 'datovalgReg', start = "2017-07-01", end = Sys.Date(),
                                                 label = "Tidsperiode", separator="t.o.m.", language="nb")
                            )
               ),
               mainPanel(
                     tabsetPanel(id='ark',
                                 tabPanel('Nøkkeltall',
                                          h2("Nøkkeltall på intensiv"),
                                          br(),
                                          tableOutput('tabNokkeltall')
                                 ),
                                 tabPanel('Ant. opphold',
                                          h2("Antall opphold per avdeling"),
                                          p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                          tableOutput("tabAntOpphShMnd12")
                                 ),
                                 tabPanel('Pasientar per år og avd.',
                                          h2("Antall pasienter ved avdelingene siste 5 år"),
                                          tableOutput("tabAntPasSh5Aar")
                                 ),
                                 tabPanel('Dobbeltregistreringar',
                                          h2("Moglege dobbeltregistreringar"),
                                          tableOutput("tabDblReg")
                                 ))
               )
      ), #tab
      
      
      #-------Fordelinger----------      
      
      tabPanel("Fordelinger",
               # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
               sidebarPanel(
                     selectInput(
                           inputId = "valgtVar", label="Velg variabel",
                           choices = c('Alder' = 'alder', 
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
                     dateRangeInput(inputId = 'datovalg', start = "2017-07-01", end = Sys.Date(),
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
                                 tableOutput('fordelingTab'))
                     )
               )
      ), #tab Fordelinger
      
      
      #-------Andeler----------      
      tabPanel("Andeler",
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
                                       'Død innen 30 dager' = 'dod30d',
                                       'Døde på intensiv' = 'dodeIntensiv',
                                       'Isolasjon av pasient' = 'isolering',
                                       'Liggetid, døde' = 'liggetidDod',
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
                     dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-07-01", end = Sys.Date(),
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
                                                 tableOutput("andelerGrVarTab"))),
                                    column(width = 1),
                                    column(width = 5, 
                                           h3("Utvikling over tid"),
                                           tableOutput("andelTidTab")#)
                                    #DT::DTOutput("andelerGrVarTab")
                           ))
               ) #mainPanel
               
      ), #tab
      
      
      #------- Gjennomsnitt ----------      
      tabPanel("Gjennomsnitt",
               h2("Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel", align='center'),
               sidebarPanel( 
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
                     dateRangeInput(inputId = 'datovalgGjsn', start = "2017-07-01", end = Sys.Date(),
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
                     h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                  til venstre. Man kan også gjøre ulike filtreringer."),
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
                                           tableOutput("tabGjsnGrVar")),
                           column(width = 1),
                           column(width = 5,
                                  h3("Utvikling over tid"),
                                  tableOutput("tabGjsnTid"))
                           ))
               )
),

#--------SMR--------------
      tabPanel('SMR',
               h3('SMR: Standardisert mortalitetsratio', align='center'),
               br(),
               sidebarPanel(
                     dateRangeInput(inputId = 'datovalgSMR', start = "2017-07-01", end = Sys.Date(),
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
                     dateRangeInput(inputId = 'datovalgInnMaate', start = "2017-07-01", end = Sys.Date(),
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
tabPanel("Pårørendeskjema",
         h2('Resultater fra Pårørendeskjema', align = 'center'),
         # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
         sidebarPanel(
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
               dateRangeInput(inputId = 'datovalgPaarorFord', start = "2015-01-01", end = Sys.Date(),
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
                           #tableOutput('fordelingTab')
                           )
               )
         )
) #tab Pårørende
)  #navbarPage




#----------------- Define server logic ----------
server <- function(input, output, session) { #
      
      
      context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
      if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
            RegData <- NIRRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
            PaarorData <- NIRpaarorDataSQL() 
            
      } #hente data på server
      
      if (!exists('PaarorDataH')){
            #system.file('inst/IntensivMndRapp.Rnw', package='intensiv')
            # load("A:/Intensiv/NIRdataPaaror.RData")
            # PaarorData <- RegData
            # load('A:/Intensiv/NIRdata10000.Rdata')
            
            data('NIRRegDataSyn', package = 'intensiv')
            
            #RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
            #Funker:
            #  data('NIRRegDataSyn', package = 'intensiv')
            #try(data(package = "intensiv"))
      }
      
      options(knitr.table.format = "html")
      datoTil <- as.POSIXlt(Sys.Date())
      AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
      aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
      reshIDdummy <- 109773 #Tromsø med.int
      reshID = 109773 
      RegData <- NIRPreprosess(RegData = RegData)
      PaarorData <- NIRPreprosess(RegData = PaarorDataH) #Må først koble på hoveddata for å få ShType++
      
      #--------startside--------------      
      #output$tekstDash <- c('Figurer med kvalitetsindikatorer',
      #                      'hente ned månedsrapport'),
      # output$mndRapp = downloadHandler(
      #       filename = 'MndRapp.pdf',
      #       #content = function(file) file.copy(system.file('NIRMndRapp.pdf', package = 'Nakke'), file, overwrite = TRUE),
      #       content = function(file) {
      #             # permission to the current working directory
      #             src <- normalizePath(system.file('NIRmndRapp.Rnw', package='intensiv'))
      #             owd <- setwd(tempdir())
      #             on.exit(setwd(owd))
      #             file.copy(src, 'NIRmndRapp.Rnw', overwrite = TRUE)
      #             
      #             #knitr::knit2pdf(system.file('NIRmndRapp.Rnw', package='intensiv'), encoding = 'UTF-8')
      #             texfil <- knitr::knit(system.file('NIRmndRapp.Rnw', package='intensiv'), encoding = 'UTF-8')
      #             tools::texi2pdf(system.file(texfil, package='intensiv'),clean = TRUE) #"NakkeMndRapp.tex"
      #             file.copy('NIRmndRapp.pdf', file)
      #             #file.rename('NIRmndRapp.pdf', file)
      #       }, contentType = 'application/pdf'
      #       
      #       # content = function(file) {
      #       #       src <- normalizePath(system.file("NORIC_local_monthly_stent.Rmd", package="noric"))
      #       #       owd <- setwd(tempdir())
      #       #       on.exit(setwd(owd))
      #       #       file.copy(src, "tmpNoricStent.Rmd", overwrite = TRUE)
      #       #       out <- render("tmpNoricStent.Rmd", output_format = pdf_document(),
      #       #                     params = list(tableFormat="latex"),
      #       #                     output_dir = tempdir())
      #       #       # active garbage collection to prevent memory hogging?
      #       #       gc()
      #       #       file.rename(out, file)
      #       # }
      #       
      #       
      # )
      # #  If you already have made the PDF file, you can just copy it to file, i.e.
      #  content = function(file) file.copy('your_existing.pdf', file, overwrite = TRUE)
      
      
      
      #------------Tabeller --------
      output$tabNokkeltall <- function() {#renderTable({
            # print(paste('Enhetsnivå: ', as.numeric(input$enhetsNivaa)))
            # print(paste('sluttdato: ', input$sluttDatoReg))
            # print(paste('tidsenhet: ', input$tidsenhetReg))
            # print(paste('RegData: ', dim(RegData)))
            # print(paste('resh: ', reshID))
            tab <- tabNokkeltall(RegData=RegData, tidsenhet=input$tidsenhetReg, datoTil=input$sluttDatoReg, 
                      enhetsUtvalg=as.numeric(input$enhetsNivaa), reshID=reshID)
            #tab <- tabNokkeltall(RegData, tidsenhet='Mnd', datoTil, enhetsUtvalg=0, reshID=0)
            kableExtra::kable(t(tab), 
                              full_width=F, 
                              digits = c(0,0,0,1,1,1,0,0,1,2,1)
                             ) %>%
                  column_spec(column = 1, width_min = '4em', width_max = 10) %>%
                  #column_spec(column = 1, width = '4em') %>%
                  column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
                  #column_spec(column = 2:(ncol(tab)), width_min = '7em', width_max = '7em') %>%
                  row_spec(0, bold = T, align = 'c') %>%
                  kable_styling(full_width = FALSE, position = 'left') #"hover", 
                  
            
      }#,rownames=T, digits=0 )
      
      output$tabAntOpphShMnd12 <- renderTable({
            switch(input$tidsenhetReg,
                   Mnd=tabAntOpphShMnd(RegData=RegData, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])  
                   Aar=tabAntOpphSh5Aar(RegData=RegData, datoTil=input$sluttDatoReg))
            #sprintf('%1.3f'
            #xtable::xtable(tabAvd12MndNskjema,  align = c('l', rep('r', ncol(tabAvd12MndNskjema))),
            #              caption= paste0('Tidsperiode: ', as.POSIXlt(datoFra12), 'til', as.POSIXlt(input$datoTil)))
            #},
      }, rownames = T, digits=0, spacing="xs" 
      ) 
      
      # output$tabAvdNAar5 <- renderTable({
      #       tabAntOpphSh5Aar(RegData=RegData, datoTil='2018-10-20')
      # }, rownames = T, digits=0, spacing="xs")
      
      output$tabAntPasSh5Aar <- renderTable({
            tabAntOpphPasSh5Aar(RegData=RegData, gr='pas', datoTil=input$sluttDatoReg)
      }, rownames = T, digits=0, spacing="xs")
      
      output$tabDblReg <- renderTable({
            finnDblReg(RegData, reshID=reshID)
      }, spacing="xs") #rownames = T, 

      output$fordelinger <- renderPlot({
            NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                                      reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                                      datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                                      minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                                      erMann=as.numeric(input$erMann))
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )
      
      observe({      
            UtDataFord <- NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                        reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                        datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                        erMann=as.numeric(input$erMann), lagFig = 0)
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
            } )
      
      output$andelerGrVar <- renderPlot({
            NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                               datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                               minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                               erMann=as.numeric(input$erMannAndelGrVar))
      }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
      )
      
            output$andelTid <- renderPlot({
                  
                  NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                 reshID=reshIDdummy,
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
                                               reshID=reshIDdummy,
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
                  output$tittelAndelGrVar <- renderUI({
                              tagList(
                                    h3(AndelerShus$tittel),
                                    h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
                              )}) #, align='center'
            }) #observe
            
             
            
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
                          reshID=reshIDdummy,
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
            output$tabGjsnGrVar <- function() {
                  tabGjsnGrVar <- cbind(Antall = dataUtGjsnGrVar$Ngr$Hoved,
                                        Sentralmål = dataUtGjsnGrVar$AggVerdier$Hoved)
                  colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')

                  kableExtra::kable(tabGjsnGrVar, format = 'html'
                                    , full_width=F
                                    , digits = c(0,1) #,1,1)[1:antKol]
                  ) %>%
                        column_spec(column = 1, width_min = '7em') %>%
                        column_spec(column = 2:3, width = '7em') %>%
                        row_spec(0, bold = T)
            }
                  output$tittelGjsn <- renderUI(
                  tagList(
                        h3(dataUtGjsnGrVar$tittel),
                        br(),
                        h5(HTML(paste0(dataUtGjsnGrVar$utvalgTxt, '<br />')))
                  ))
            dataUtGjsnTid <- NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                           reshID=reshIDdummy,
                                           datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                           minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                           erMann=as.numeric(input$erMannGjsn), 
                                           valgtMaal = input$sentralmaal,
                                           tidsenhet = input$tidsenhetGjsn,
                                           enhetsUtvalg = input$enhetsUtvalgGjsn) #, lagFig=0)
            output$tabGjsnTid <- function() {
                  tabGjsnTid <- t(dataUtGjsnTid$AggVerdier)
                  grtxt <-dataUtGjsnTid$grtxt
                  if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
                        grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
                  rownames(tabGjsnTid) <- grtxt

                  antKol <- ncol(tabGjsnTid)
                  navnKol <- colnames(tabGjsnTid) 
                  if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}
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
      })

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

} #serverdel
# Run the application
shinyApp(ui = ui, server = server)

