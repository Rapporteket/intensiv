# Shiny-app for Norsk Intensivregister
library(intensiv)

addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- (context %in% c("DEV", "TEST", "QA","QAC", "PRODUCTION", "PRODUCTIONC"))


options(knitr.table.format = "html")
idag <- Sys.Date() #as.Date('2018-11-30') #
datoTil <- as.POSIXlt(idag)
aarFra <- paste0(1900+as.POSIXlt(idag)$year-5, '-01-01')
startDato <- paste0(as.numeric(format(idag-90, "%Y")), '-01-01') #paste0(1900+as.POSIXlt(idag)$year, '-01-01')
AarNaa <- as.numeric(format(idag, "%Y"))


#---------Hente data------------

  message("Getting IntData")
  IntDataRaa <- NIRRegDataSQL(datoFra = '2015-01-01')

#Covid-skjema:
  message("Getting covid data")
  qCovid <- paste0('SELECT UPPER(HovedskjemaGUID) AS HovedskjemaGUID, FormStatus, Diagnosis
                FROM beredskap')
CovidData <- rapbase::loadRegData(registryName= "data", query=qCovid, dbType="mysql")

CovidData$Bekreftet <- 0
CovidData$Bekreftet[which(CovidData$Diagnosis %in% 100:103)] <- 1

message("Merge IntData and covid data")
RegData <- merge(IntDataRaa, CovidData[ ,-which(names(CovidData) == 'Diagnosis')], suffixes = c('','Cov'),
                 by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)
message("Preposess data")
RegData <- NIRPreprosess(RegData = RegData)
message("Get paaror data")
PaarorData <- NIRpaarorDataSQL()
PaarorDataH <- KobleMedHoved(IntDataRaa, PaarorData, alleHovedskjema=F, alleSkjema2=F)
antPaaror <- dim(PaarorDataH)[1]
if (antPaaror>0) {
  PaarorData <- NIRPreprosess(RegData = PaarorDataH) #Må først koble på hoveddata for å få ShType++
}
message("Alle data hentet!")

#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------


#Definere utvalgsinnhold

sykehusNavnResh <- unique(RegData[,c("ShNavn", "ReshId")])
rekkeflg <- order(sykehusNavnResh$ShNavn)
sykehusValg <- c(0,sykehusNavnResh$ReshId[rekkeflg])
names(sykehusValg) <- c('Ikke valgt',sykehusNavnResh$ShNavn[rekkeflg])
message("Done!")

enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2,
                  "Egen enhet mot egen sykehustype" = 3,
                  "Egen sykehustype" = 4,
                  "Egen sykehustype mot resten av landet" = 5,
                  "Egen enhet mot egen region" = 6,
                  "Egen region" = 7,
                  "Egen region mot resten" = 8)

covidValg <- c('Alle pasienter' = 0,
                  'Covid-pasienter' = 1,
               'Alle unntatt Covid-pasienter' = 2)
velgCovidTxt <- 'Velg diagnose (covid-pasienter)'

# -----Define UI for application that draws figures-----
pdf(file = NULL)
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              'NORSK INTENSIVREGISTER'),
  windowTitle = 'NORSK INTENSIVREGISTER',
  theme = "rap/bootstrap.css",
  id = 'hovedark',


#--------------Startside------------------------------
  tabPanel(p("Oversiktsside",
             title= 'Nøkkeltall og samlerapporter'),
           shinyjs::useShinyjs(),

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
             h2('Hente datauttrekk'),
             dateRangeInput(inputId = 'datovalgData', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = 'velgReshData', label='Velg sykehus',
                         selected = 0,
                         choices = sykehusValg),
             downloadButton(outputId = 'lastNed_dataDump', label='Last ned datadump')

           ),
           mainPanel(
             tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
             if (paaServer) {
               rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
             },

             tabsetPanel(
               tabPanel('Startside',
             h3(ifelse(paaServer, "","Merk at noen resultater kan se rare ut siden dette er syntetiske data!"), align='center' ),
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
                    # h4(tags$b('PREM-skjema'), 'viser resultater fra pårørendeundersøkelser
                    #    registrert i skjemaet FS-ICU.'),
                    br(),
                    h4('Gi gjerne innspill til registerledelsen om det er resultater/tabeller/figurer du savner
                            på Rapporteket-Intensiv.')
                    )
             )#tabset
           )#main
  ), #tab

  #-----Registreringsoversikter------------
  tabPanel(p("Aktivitet", title='Tabeller med registreringsoversikter, samt nøkkeltall'),
           sidebarPanel(width=3,
                        br(),
                        br(),
                        br(),
                        conditionalPanel(condition = "input.ark == 'Nøkkeltall' || input.ark == 'Ant. opphold'
                                             || input.ark == 'Pasientar per år og avd.' ",
                                         dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                   value = Sys.Date(), max = Sys.Date()),
                                         selectInput(inputId = "covidvalgReg", label= velgCovidTxt,
                                                     choices = covidValg)
                        ),
                       conditionalPanel(
                          condition = "input.ark == 'Nøkkeltall' || input.ark == 'Ant. opphold'",
                          selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                      choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                        conditionalPanel(
                          condition = "input.ark == 'Nøkkeltall'",
                          selectInput(inputId = 'enhetsNivaaReg', label='Enhetsnivå',
                                      choices = c("Hele landet"=0, "Egen enhet"=2,
                                                  "Egen sykehustype"=4, "Egen region"=7)
                          ),
                          selectInput(inputId = 'respiratorReg', label = 'Respiratorstøtte?',
                                      choices = c(' '=4, 'Nei'=0, 'Ja'=1, 'Invasiv'=2, 'Non-invasiv'=3)
                                      )),
                        conditionalPanel(
                          condition = "input.ark == 'Overføringer'",
                          dateRangeInput(inputId = 'datovalgReg', start = startDato, end = idag,
                                         label = "Tidsperiode", separator="t.o.m.", language="nb"),
                          selectInput(inputId = 'velgReshOverf', label='Velg eget Sykehus',
                                      #selected = 0,
                                      choices = sykehusValg)
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
                                  #h2('Nøkkeltall på intensiv'),
                                  h2(uiOutput('NokkeltallTxtReg'), align='center'),
                                  tableOutput('tabNokkeltall')
                         ),
                         tabPanel('Overføringer',
                                  p(h2('Overføring av intensivpasienter',
                                     align='center') ),
                                  #h2(uiOutput('user$org()')),
                                  br(),
                                  column(6,
                                         tableOutput('tabOverfTil')
                                  ),
                                  column(6,
                                         tableOutput('tabOverfFra'))
                         ),
                          tabPanel('Dobbeltregistreringer',
                                  h2("Mulige dobbeltregistreringer"),
                                  tableOutput("tabDblReg")
                         )
             ) #tabset
           ) #main
  ), #tab


  #-------Fordelinger----------

  tabPanel(p("Fordelinger",
             title='Alder, Type opphold, Hemodynamisk overvåkning, Isolasjon, Liggetid, Nas, NEMS, Nyrebehandling,
                 Primærårsak, Respiratortid, SAPSII, Spesielle tiltak, Donorer'),
           value = 'Fordelinger',
           h2("Fordelingsfigurer", align='center'),
           # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())

           sidebarPanel(
             id = "brukervalg_fordeling",
             width = 3,
             h4('Her kan man velge hvilken variabel man ønsker å se resultater for og gjøre filtreringer.'),
             br(),
             selectInput(
               inputId = "valgtVar", label="Velg variabel",
               selected = c('Registreringsforsinkelse' = 'regForsinkelse'),
               choices = c('Alder' = 'alder',
                           'Bukleie' = 'bukleie',
                           'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
                           'Frailty index' = 'frailtyIndex',
                           'Inklusjonskriterier' = 'inklKrit',
                           'Isolasjon, type' = 'isolering',
                           'Isolasjon, varighet' = 'isoleringDogn',
                           'Komplikasjoner' = 'komplikasjoner',
                           'Liggetid' = 'liggetid',
                           'Nas-skår (sykepleierakt.)' = 'Nas24',
                           'NEMS-skår (ressursbruk)' = 'NEMS24',
                           'Nyreerstattende beh., type' = 'nyreBeh',
                           'Nyreerstattende beh., varighet' = 'nyreBehTid',
                           'Potensielle donorer, årsak ikke påvist opph. sirkulasjon' = 'CerebralCirculationAbolishedReasonForNo',
                           'Primærårsak' = 'PrimaryReasonAdmitted',
                           'Registreringsforsinkelse, innleggelse' = 'regForsinkelseInn',
                           'Registreringsforsinkelse, ferdigstillelse' = 'regForsinkelse',
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
             ),

               dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                              label = "Tidsperiode", separator="t.o.m.", language="nb" #)
               ),
               selectInput(inputId = "erMann", label="Kjønn",
                           choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
               ),
               sliderInput(inputId="alder", label = "Alder", min = 0,
                           max = 110, value = c(0, 110)
               ),
             selectInput(inputId = "covidvalg", label= velgCovidTxt,
                         choices = covidValg),

             selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                             choices = enhetsUtvalg
                 ),
             selectInput(inputId = 'velgResh', label='Velg eget Sykehus',
                         #selected = 0,
                         choices = sykehusValg),
             actionButton("reset_fordValg", label="Tilbakestill valg"),
             br(),
             selectInput(inputId = "bildeformatFord",
                         label = "Velg format for nedlasting av figur",
                         choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
           ),


           mainPanel(
             tabsetPanel(
               tabPanel(
                 'Figur',
                 plotOutput('fordelinger', height = 'auto'),
                 downloadButton('LastNedFigFord', label='Velg format og last ned figur')),
               tabPanel(
                 'Tabell',
                 uiOutput("tittelFord"),
                 tableOutput('fordelingTab'),
                 downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell') #, class = "butt")
               )
             )
           )
  ), #tab Fordelinger

  #-------Andeler----------
  tabPanel(p("Andeler", title='Alder, Overlevelse, Isolasjon, Nyrebehandling, Reinnleggelse, Respiratorstøtte, Respiratortid,
                 Utenfor vakttid, Hemodyn., Takeostomi, Donorer'),
           value = 'Andeler',
           h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
           h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                    til venstre. Man kan også gjøre ulike filtreringer.", align='center'),
           br(),
           br(),
           sidebarPanel(
             id = "brukervalg_andeler",
             width=3,
             h4('Her kan man velge hvilken variabel man ønsker å se resultater for og gjøre filtreringer.'),
             selectInput(
               inputId = "valgtVarAndel", label="Velg variabel",
               choices = c('Alder minst 80 år' = 'alder_over80',
                           'Alder under 18år' = 'alder_u18',
                           'Bukleie' = 'bukleie',
                           'Døde innen 30 dager' = 'dod30d',
                           'Døde innen 90 dager' = 'dod90d',
                           'Døde innen ett år' = 'dod365d',
                           'Døde på intensiv' = 'dodeIntensiv',
                           'Frailty index registrert' = 'frailtyIndex',
                           'Invasiv respiratortid < 2,5 døgn, m/overførte' = 'respiratortidInvMoverf',
                           'Invasiv respiratortid < 2,5 døgn, u/overførte' = 'respiratortidInvUoverf',
                           'Isolasjon av pasient' = 'isolering',
                           'Invasiv ventilasjon' = 'invasivVent',
                           'Komplikasjonsregistrering' = 'komplReg',
                           'Liggetid, døde' = 'liggetidDod',
                           'Menn' = 'erMann',
                           'Nyreerstattende behandling' = 'nyreBeh',
                           'Organdonorer, av døde' = 'OrganDonationCompletedStatus',
                           'Organdonorer, av alle med opphevet intrakran. sirk.' = 'OrganDonationCompletedCirc',
                           'Potensielle donorer' = 'potDonor',
                           'Registreringsforsinkelse, innleggelse' = 'regForsinkelseInn',
                           'Registreringsforsinkelse, ferdigstillelse' = 'regForsinkelse',
                           'Reinnleggelse' = 'reinn',
                           'Respiratorstøtte' = 'respStotte',
                           'Respiratortid, døde' = 'respiratortidDod',
                           'Utenfor vakttid, innlagt' = 'utenforVakttidInn',
                           'Utenfor vakttid, utskrevet' = 'utenforVakttidUt',
                           'Utvidet hemodyn. overvåkning' = 'ExtendedHemodynamicMonitoring',
                           'Trakeostomi' = 'trakeostomi',
                           'Trakeostomi, åpen' = 'trakAapen'
                           ),
               selected = 'regForsinkelseInn',
             ),
             dateRangeInput(inputId = 'datovalgAndel', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannAndel", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)),
             sliderInput(inputId="alderAndel", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)),
             selectInput(inputId = "covidvalgAndel", label= velgCovidTxt,
                         choices = covidValg),
             br(),
             p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
             selectInput(inputId = 'enhetsUtvalgAndelTid', label='Egen enhet og/eller landet',
                         choices = enhetsUtvalg), #c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
             selectInput(inputId = "tidsenhetAndelTid", label="Velg tidsenhet",
                         choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                         'Kvartal'='Kvartal', 'Måned'='Mnd'))),
             actionButton("reset_andelValg", label="Tilbakestill valg"),
             br(),
             selectInput(inputId = "bildeformatAndel",
                         label = "Velg format for nedlasting av figur",
                         choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))

           ),
           mainPanel(
             tabsetPanel(
               tabPanel(
                 "Figurer",
                 h3(em("Utvikling over tid")),
                 plotOutput("andelTid", height = 'auto'),
                 downloadButton('LastNedFigAndelTid', label='Velg format og last ned figur'),
                 br(),
                 h3(em("Sykehusvise resultater")),
                 plotOutput("andelerGrVar", height='auto'),
                 downloadButton('LastNedFigAndelGrVar', label='Velg format og last ned figur'),
                 h5('Velg format til venstre')
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
             id = "brukervalg_gjsn",
             width = 3,
             h4('Her kan man velge hvilken variabel man ønsker å se resultater for og gjøre filtreringer.'),
             selectInput(inputId = "valgtVarGjsn", label="Velg variabel",
                         choices = c('Alder' = 'alder',
                                     'Liggetid' = 'liggetid',
                                     'Nas-skår (sykepleieraktivitet)' = 'Nas24',
                                     'NEMS-skår per døgn' = 'NEMS24',
                                     'NEMS-skår per opphold' = 'NEMS',
                                     'Respiratortid, invasiv og non-invasiv' = 'respiratortid',
                                     'Respiratortid, non-invasiv' = 'respiratortidNonInv',
                                     'Respiratortid, invasiv m/overførte' = 'respiratortidInvMoverf',
                                     'Respiratortid, invasiv u/overførte' = 'respiratortidInvUoverf',
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
             selectInput(inputId = "covidvalgGjsn", label= velgCovidTxt,
                         choices = covidValg),
             selectInput(inputId = "bildeformatGjsn",
                         label = "Velg format for nedlasting av figur",
                         choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
             actionButton("reset_gjsnValg", label="Tilbakestill valg"),
             br(),
             p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
             selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                         choices = enhetsUtvalg #c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
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
                        plotOutput("gjsnTid", height = 'auto'),
                        downloadButton(outputId = 'LastNedFigGjsnTid', label='Last ned figur'),
                        plotOutput("gjsnGrVar", height = 'auto'),
                        downloadButton(outputId = 'LastNedFigGjsnGrVar', label='Last ned figur')
             ),
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
           h3('Standardisert mortalitetsratio', align='center'),
           br(),
           sidebarPanel(
             width = 3,
             h4('Her kan man gjøre filtreringer.'),
             selectInput(inputId = "valgtVarMort", label="Velg variabel",
                        choices = c('SMR, SAPSII' = 'SMR',
                                    'SMR: PIM' = 'PIMdod')),
             selectInput(inputId = "covidvalgSMR", label= velgCovidTxt,
                         choices = covidValg),
             dateRangeInput(inputId = 'datovalgSMR', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannSMR", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
             ),
             sliderInput(inputId="alderSMR", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             ),
             br(),
             conditionalPanel(
               condition = "input.SMRfigtab == 'Figur' ",
               selectInput(inputId = "bildeformatSMR",
                         label = "Velg format for nedlasting av figur",
                         choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
             downloadButton('LastNedFigSMR', label='Last ned figur'))
           ),
           mainPanel(
             tabsetPanel(id='SMRfigtab',
               tabPanel("Figur",
                          plotOutput("SMRfig") #, height="auto")
                        # h5('Velg figurformat i nedtrekksmeny i venstre panel'),)
                        ),
               tabPanel("Tabell",
                        uiOutput("tittelSMR"),
                        br(),
                        tableOutput("SMRtab"))
             )
           )
  ), #tab

  #--------Type opphold--------------
  tabPanel('Type opphold',
           h3('Type opphold', align='center'),
           br(),
           sidebarPanel(
             width = 3,
             h4('Her kan man gjøre filtreringer.'),
             dateRangeInput(inputId = 'datovalgInnMaate', start = startDato, end = idag,
                            label = "Tidsperiode", separator="t.o.m.", language="nb"),
             selectInput(inputId = "erMannInnMaate", label="Kjønn",
                         choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
             ),
             sliderInput(inputId="alderInnMaate", label = "Alder", min = 0,
                         max = 110, value = c(0, 110)
             ),
             selectInput(inputId = "covidvalgInnMaate", label= velgCovidTxt,
                         choices = covidValg),
             br(),
             selectInput(inputId = "bildeformatTypeOpph",
                         label = "Velg format for nedlasting av figur",
                         choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
             downloadButton('LastNedFigTypeOpph', label='Last ned figur')
           ),
           mainPanel(
             plotOutput('innMaate')
           )
  ), #tab

  #-------Pårørendeskjema----------
#DENNE SKAL OPPDATERES TIL Å GJELDE NY VERSJON AV SKJEMAET
  # tabPanel(p("PREM-skjema", title='Enkeltspørsmål fra FS-ICU, samt totalskårer'),
  #          h2('Resultater fra Pårørendeskjema (FS-ICU)', align = 'center'),
  #          # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
  #          sidebarPanel(
  #            width = 3,
  #            h4('Her kan man velge hvilken variabel man ønsker å se resultater for og gjøre ulike filtreringer.'),
  #            selectInput(
  #              inputId = "valgtVarPaarorFord", label="Velg variabel",
  #              choices = c('S1.1 Pasient, høflighet og medfølelse' = 'BehandlingHoeflighetRespektMedfoelelse',
  #                          'S1.2 Smerte' = 'SymptomSmerte',
  #                          'S1.3 Pustebesvær' = 'SymptomPustebesvaer',
  #                          'S1.4 Uro' = 'SymptomUro',
  #                          'S1.5 Interesse for behov' = 'BehandlingBesvarerBehov',
  #                          'S1.6 Følelsesmessig støtte' = 'BehandlingBesvarerStoette',
  #                          'S1.7 Samarbeid' = 'BehandlingSamarbeid',
  #                          'S1.8 Pårørende, høflighet og medfølelse' = 'BehandlingBesvarerHoeflighetRespektMedfoelelse',
  #                          'S1.9 Omsorg, sykepleier' = 'SykepleierOmsorg',
  #                          'S1.10 Kommunikasjon, sykepleier' = 'SykepleierKommunikasjon',
  #                          'S1.11 Omsorg, lege' = 'LegeBehandling',
  #                          'S1.12 Atmosfære på avd.' = 'AtmosfaerenIntensivAvd',
  #                          'S1.13 Atmosfære, venterom' = 'AtmosfaerenPaaroerenderom',
  #                          'S1.14 Omfang av behandling' = 'OmfangetAvBehandlingen',
  #                          'S2.1 Legens informasjonsfrekvens' = 'LegeInformasjonFrekvens',
  #                          'S2.2 Svarvillighet, personale' = 'SvarPaaSpoersmaal',
  #                          'S2.3 Forståelige forklaringer' = 'ForklaringForstaaelse',
  #                          'S2.4 Informasjon, ærlighet' = 'InformasjonsAerlighet',
  #                          'S2.5 Informasjon' = 'InformasjonOmForloep',
  #                          'S2.6 Informasjon, overensstemmelse' = 'InformasjonsOverensstemmelse',
  #                          'S2.7 Beslutningsprosess, involvering' = 'BeslutningsInvolvering',
  #                          'S2.8 Beslutningsprosess, støtte' = 'BeslutningsStoette',
  #                          'S2.9 Beslutningsprosess, innflytelse' = 'BeslutningsKontroll',
  #                          'S2.10 Beslutningsprosess, tid' = 'BeslutningsTid',
  #                          'S2.11 Livslengde' = 'LivsLengde',
  #                          'S2.12 Komfort ved livsslutt, pasient' = 'LivssluttKomfor',
  #                          'S2.13 Involvering ved livsslutt' = 'LivssluttStoette',
  #                          'Totalskår, omsorg (skjema 1)' = 'SumScoreSatisfactionCare',
  #                          'Totalskår, beslutning (skjema 2)' = 'SumScoreSatisfactionDecision',
  #                          'Totalskår, alle spørsmål' = 'SumScoreAllQuestions')
  #            ),
  #            dateRangeInput(inputId = 'datovalgPaarorFord', start = "2015-01-01", end = idag,
  #                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
  #            dateInput(inputId = 'startDatoIntervensjon', label = 'Startdato, intervensjon', language="nb",
  #                      value = '2016-10-01', max = Sys.Date()),
  #            selectInput(inputId = 'enhetsUtvalgPaarorFord', label='Egen enhet / hele landet',
  #                        choices =  c("Hele landet"=0, "Egen enhet"=2)),
  #            h5('(NB: Hvis din avdeling ikke har registreringer, vises hele landet uansett valg)'),
  #            selectInput(inputId = "erMannPaarorFord", label="Kjønn, pasient",
  #                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0))
  #            #h3('Utvalg vedrørende den pårørende (alder, kjønn, relasjon,...)?')
  #          ),
  #
  #          mainPanel(
  #            tabsetPanel(
  #              tabPanel(
  #                'Figur',
  #                plotOutput('paarorFord')),
  #              tabPanel(
  #                'Tabell',
  #                h3('Her kommer en tabell')
  #                #uiOutput("tittelFord"),
  #                #tableOutput('fordelingTabPaaror')
  #              )
  #            )
  #          )
  # ), #tab Pårørende


  #-----------Abonnement--------------------------------

tabPanel(p("Abonnement",
           title='Bestill automatisk utsending av rapporter på e-post'),
         value = 'Abonnement',

         sidebarLayout(
           sidebarPanel(
             rapbase::autoReportInput("intensivAbb")
           ),
           shiny::mainPanel(
             rapbase::autoReportUI("intensivAbb")
           )
         )
), #tab abonnement


#-------Registeradministrasjon----------

tabPanel(p("Registeradministrasjon", title='Registeradministrasjonens side'),
         value = "Registeradministrasjon",
         h3('Bare synlig for SC-bruker'),

         tabsetPanel(
           tabPanel(
             h4("Utsendinger"),
                    #title = "Utsending av rapporter",
                    sidebarLayout(
                      sidebarPanel(
                        rapbase::autoReportOrgInput("NIRuts"),
                        rapbase::autoReportInput("NIRuts")
                      ),
                      mainPanel(
                        rapbase::autoReportUI("NIRuts")
                      )
                    )
           ),
           tabPanel(
             h4("Eksport av krypterte data"),
           sidebarLayout(
             sidebarPanel(
               rapbase::exportUCInput("intensivExport")
             ),
             shiny::mainPanel(
               rapbase::exportGuideUI("intensivExportGuide")
             )
           )
         ),
         tabPanel(h4('Nøkkeltall'),
                 h2('Nøkkeltall, for valgt HF/RHF', align='center'),
                 h4('Gjør utvalg'),
                 dateRangeInput(inputId = 'datoValgNok', label = 'Tidsperiode',
                              start = '2018-01-01', end = idag, #startDato
                              separator="t.o.m.", language="nb"),
                    selectInput(inputId = "covidvalgNok", label= velgCovidTxt,
                                           choices = covidValg),
                    selectInput(inputId = "enhetNok", label= 'Velg enhet',
                                choices =   c('Alle',
                                              unique(RegData$RHF),
                                              unique(RegData$HF),
                                              unique(RegData$HelseenhetKortnavn))),
                  br(),
                 h4('Andel opphold med *komplikasjon*, er definert som et opphold hvor det har
                    oppstått minst én av følgende komplikasjoner:
                    Alvorlig hypoglykemi, pneumotoraks, luftveisproblem, trakealtube/kanyle, dekubitus'),
                  tableOutput('tabNokkeltallUtvidet'),
                 downloadButton(outputId = 'lastNed_tabNokkel', label='Last ned tabell')
                  )
#         ),
         ) #tabset
) #tab SC

)  #navbarPage

#----------------- Define server logic ----------
server <- function(input, output, session) { #

#-----------Div serveroppstart------------------
  #rapbase::appLogger(session = session, msg = "Starter intensiv-app")

  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
  paaServer <- (context %in% c("DEV", "TEST", "QA","QAC", "PRODUCTION", "PRODUCTIONC")) #rapbase::isRapContext()
  message("Intensivapp server started in context: ", context)
  #if (paaServer) {


  map_avdeling <- data.frame(
    UnitId = unique(RegData$ReshId),
    orgname = RegData$ShNavn[match(unique(RegData$ReshId),
                                   RegData$ReshId)])
  message("Map avdeling created with ", nrow(map_avdeling), " rows.")
  #user inneholder både reshID: user$org() og  rolle: user$role()
  # "name", "fullName", "phone", "email", "group", "unit", "org", "role", "orgName"
  user <- rapbase::navbarWidgetServer2(
    id = "navbar-widget",
    orgName = "intensiv",
    map_orgname = shiny::req(map_avdeling),
    caller = "intensiv"
  )


  observeEvent(user$role(), {
    message("User role changed to: ", user$role())
    if (user$role() == 'SC') {
      shinyjs::show(id = 'velgResh')
      shinyjs::show(id = 'velgReshOverf')
      shinyjs::show(id = 'velgReshData')
      showTab(inputId = "hovedark", target = "Registeradministrasjon")
    } else {
      shinyjs::hide(id = 'velgResh')
      shinyjs::hide(id = 'velgReshOverf')
      shinyjs::hide(id = 'velgReshData')
      hideTab(inputId = "hovedark", target = "Registeradministrasjon")
    }
  })

  observeEvent(input$reset_fordValg, shinyjs::reset("brukervalg_fordeling"))
  observeEvent(input$reset_andelValg, shinyjs::reset("brukervalg_andeler"))
  observeEvent(input$reset_gjsnValg, shinyjs::reset("brukervalg_gjsn"))

  egenLokalitet <- c(0, 2, 4, 7)
  names(egenLokalitet) <- c('hele landet', 'egen enhet', 'egen sykehustype' , 'eget RHF')

  # Foreløpig ikke i bruk...??
  output$egetShNavn <- renderText(as.character(RegData$ShNavn[match(user$org(), RegData$ReshId)]))


  # widget
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', user$role(),
                                           '<br> ReshID: ', user$org(),
                                           '<br> Enhet: ', user$orgName()) )

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })



#--------startside--------------
  output$mndRapp.pdf <- downloadHandler(
    filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRmndRapp.Rnw",
                          reshID = user$org(), datoFra = startDato)
    }
  )

  output$samleRapp.pdf <- downloadHandler(
    filename = function(){ paste0('NIRsamleRapp', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporter(file, rnwFil="NIRSamleRapp.Rnw",
                  reshID = user$org(), datoFra = startDato)
    }
  )

  #test <- henteSamlerapporter('file.pdf', rnwFil="NIRinfluensa.Rnw")
  #Datadump
  observe({
    RegDataReinn <- FinnReinnleggelser(RegData)
     DataDump <- NIRUtvalgEnh(RegData=RegDataReinn,
                           datoFra = input$datovalgData[1],
                          datoTil = input$datovalgData[2])$RegData


    if (user$role() == 'SC') {
      valgtResh <- as.numeric(input$velgReshData)
      ind <- if (valgtResh == 0) {1:dim(DataDump)[1]
        } else {which(as.numeric(DataDump$ReshId) %in% as.numeric(valgtResh))}
      tabDataDump <- DataDump[ind,]

    } else {
      tabDataDump <-
        DataDump[which(DataDump$ReshId == user$org()), ]
      #output$test <- renderText(dim(tabDataDump)[1])
    } #Tar bort PROM/PREM til egen avdeling

    output$lastNed_dataDump <- downloadHandler(
      filename = function(){'dataDumpNIR.csv'},
      content = function(file, filename){write.csv2(tabDataDump, file, row.names = F, na = '')})
  })






#------------ Aktivitet (/Tabeller) --------

  output$NokkeltallUtvalgTxt <- renderText({
    paste0('Nøkkeltall på intensiv, ',
                as.character(names(egenLokalitet[which(egenLokalitet==as.numeric(input$enhetsNivaaStart))])))
  })
   output$tabNokkeltallStart <- function() {
    tab <- t(tabNokkeltall(RegData=RegData, tidsenhet='Mnd',
                           enhetsUtvalg=as.numeric(input$enhetsNivaaStart),
                           reshID = user$org()))
    kableExtra::kable(tab,
                      full_width=F,
                      digits = c(0,0,0,1,0,0,1,1,1,0,0,0,1,0,0)
    ) %>%
      kableExtra::column_spec(column = 1, width_min = '4em', width_max = 10) %>%
      kableExtra::column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
      kableExtra::row_spec(0, bold = T, align = 'c') %>%
      kableExtra::kable_styling(full_width = FALSE, position = 'left')
  }

   output$NokkeltallTxtReg <- renderText({
     c(paste0('Nøkkeltall på intensiv, ',
            as.character(names(egenLokalitet[which(egenLokalitet==as.numeric(input$enhetsNivaaReg))]))),
     c(paste0(c(', uten', ', med', ', invasiv', ', non-invasiv'), ' respiratorstøtte'), '')[as.numeric(input$respiratorReg) + 1]
     )
   })

   output$tabNokkeltall <- function() {
     RegDataCov <- NIRUtvalgEnh(RegData=RegData, luftvei = as.numeric(input$covidvalgReg))$RegData
     tab <- t(tabNokkeltall(RegData=RegDataCov,
                            tidsenhet=input$tidsenhetReg,
                            datoTil=input$sluttDatoReg,
                            respirator=input$respiratorReg,
                            enhetsUtvalg=as.numeric(input$enhetsNivaaReg),
                                     reshID = user$org()))
       kableExtra::kable(tab,
                         full_width=F,
                         digits = c(0,0,0,1,0,0,1,1,1,0,0,0,1,0,0)
                         ) %>%
                  kableExtra::column_spec(column = 1, width_min = '4em', width_max = 10) %>%
                  kableExtra::column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
         kableExtra::row_spec(0, bold = T, align = 'c') %>%
         kableExtra::kable_styling(full_width = FALSE, position = 'left')
      } # ,rownames=T, digits=0 )



   output$tabNokkeltallUtvidet <- function() {
     RegDataCov <- NIRUtvalgEnh(RegData=RegData, luftvei = as.numeric(input$covidvalgNok))$RegData
     tab <- t(tabNokkeltall(RegData=RegDataCov,
                                 tidsenhet='Aar',
                                 datoFra = input$datoValgNok[1],
                                 datoTil = input$datoValgNok[2],
                                sykehus=input$enhetNok,
                                utvidTab=1)
              )
     kableExtra::kable(tab,
                       full_width=F,
                       digits = c(0,0,0,1,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1)
     ) %>%
       kableExtra::column_spec(column = 1, width_min = '4em', width_max = 10) %>%
       kableExtra::column_spec(column = 2:(ncol(tab)), width = '4em')  %>%
       kableExtra::row_spec(0, bold = T, align = 'c') %>%
       kableExtra::kable_styling(full_width = FALSE, position = 'left') #"hover",
   }

   output$lastNed_tabNokkel <- downloadHandler(
     filename = function(){'NokkelTall.csv'
     },
     content = function(file, filename){
       RegDataCov <- NIRUtvalgEnh(RegData=RegData, luftvei = as.numeric(input$covidvalgNok))$RegData
       tab <- t(tabNokkeltall(RegData=RegDataCov,
                              datoFra = input$datoValgNok[1],
                              datoTil = input$datoValgNok[2],
                              sykehus=input$enhetNok,
                              utvidTab=1))
       write.csv2(tab, file, row.names = F, na = '')
     })

      output$tabAntOpphSh <- renderTable({
        RegDataCov <- NIRUtvalgEnh(RegData=RegData, luftvei = as.numeric(input$covidvalgReg))$RegData
            tab <- switch(input$tidsenhetReg,
                   Mnd=tabAntOpphShMnd(RegData=RegDataCov, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])
                   Aar=tabAntOpphSh5Aar(RegData=RegDataCov, datoTil=input$sluttDatoReg))

      }, rownames = T, digits=0, spacing="xs"
      )

      output$tabAntPasSh5Aar <- renderTable({
            tabAntOpphPasSh5Aar(RegData=RegData, gr='pas', datoTil=input$sluttDatoReg)
      }, rownames = T, digits=0, spacing="xs")

      output$tabOverfTil <- renderTable({
        tab <- tabOverforinger(RegData=RegData, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2],
                               reshID = user$org(), velgAvd=input$velgReshOverf,  overfFraSh=0)
        xtable::xtable(tab) #c('r','r','r')
      }, rownames=F, colnames = T, align = 'r')

      output$tabOverfFra <- renderTable({
        #tab <- tabOverforinger(RegData=RegData, reshID = user$org())
        tab <- tabOverforinger(RegData=RegData, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2],
                                    reshID = user$org(), velgAvd=input$velgReshOverf, overfFraSh=1)
         xtable::xtable(tab, rownames=F)
      }, rownames = F, colnames = T, align = 'r')

      output$tabDblReg <- renderTable({
        tabDBL <- finnDblReg(RegData, reshID = user$org()) #tabDBL <-
        finnDblReg(RegData, reshID = user$org())
      }, spacing="xs") #rownames = T,


      output$inklKrit <- renderPlot({
        NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar='inklKrit',
                      reshID = user$org(), enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                      datoFra=input$datovalg[1], datoTil=input$datovalg[2], session=session)
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )

#------------Fordelinger---------------------

      output$fordelinger <- renderPlot({
            NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                          reshID = user$org(), velgAvd = input$velgResh,
                          enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                          datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                          minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                          erMann=as.numeric(input$erMann), luftvei = as.numeric(input$covidvalg),
                          session = session)
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )

      output$LastNedFigFord <- downloadHandler(
        filename = function(){
          paste0('FigurFord_', input$valgtVar, Sys.Date(), '.', input$bildeformatFord)
        },
        content = function(file){
          NIRFigAndeler(RegData=RegData, preprosess = 0,
                        valgtVar=input$valgtVar,
                        reshID = user$org(),
                        velgAvd = input$velgResh,
                        enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                        datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                        erMann=as.numeric(input$erMann),
                        luftvei = as.numeric(input$covidvalg),
                        outfile = file)
        }
      )

      observe({
        UtDataFord <- NIRFigAndeler(RegData=RegData, preprosess = 0,
                                        valgtVar=input$valgtVar,
                                        reshID = user$org(),
                                        enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                        velgAvd = input$velgResh,
                                        datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                        erMann=as.numeric(input$erMann),
                                        luftvei = as.numeric(input$covidvalg),
                                        lagFig = 0, session = session)
            tab <- lagTabavFig(UtDataFraFig = UtDataFord)

            output$tittelFord <- renderUI({
                  tagList(
                        h3(HTML(paste(UtDataFord$tittel, sep='<br />'))),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(

                  #       kableExtra::kable_styling("hover", full_width = F)
                  antKol <- ncol(tab)
                  kableExtra::kable(tab, format = 'html'
                                    , full_width=F
                                    , digits = c(0,1,0,1)[1:antKol]
                                    ) %>%
                        kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                        kableExtra::column_spec(column = 1, width_min = '7em') %>%
                        kableExtra::column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
                        kableExtra::row_spec(0, bold = T)
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
            NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                               datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                               minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                               erMann=as.numeric(input$erMannAndel),
                               luftvei = as.numeric(input$covidvalgAndel),
                               session=session)
      }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
      )

      output$LastNedFigAndelGrVar <- downloadHandler(
        filename = function(){
          paste0('FigurAndelEnh_', Sys.time(), '.', input$bildeformatAndel)
        },
        content = function(file){
          NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                             datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                             minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                             erMann=as.numeric(input$erMannAndel),
                             luftvei = as.numeric(input$covidvalgAndel),
                          outfile = file)
        }
      )

            output$andelTid <- renderPlot({

                  NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                 reshID = user$org(),
                                 datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                 minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                 erMann=as.numeric(input$erMannAndel),
                                 luftvei = as.numeric(input$covidvalgAndel),
                                 tidsenhet = input$tidsenhetAndelTid,
                                 enhetsUtvalg = input$enhetsUtvalgAndelTid,
                                 session=session)
            }, height = 300, width = 1000
            )

            output$LastNedFigAndelTid <- downloadHandler(
              filename = function(){
                paste0('FigurAndelTid_',input$valgtVarAndel, Sys.time(), '.', input$bildeformatAndel)
              },
              content = function(file){
                NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                   reshID = user$org(),
                                   datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                   minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                   erMann=as.numeric(input$erMannAndel),
                                   luftvei = as.numeric(input$covidvalgAndel),
                                   tidsenhet = input$tidsenhetAndelTid,
                                   enhetsUtvalg = input$enhetsUtvalgAndelTid,
                                   session=session,
                                   outfile = file)
              }
            )
            observe({
                  #AndelTid

                  AndelerTid <- NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                               reshID = user$org(),
                                               datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                               minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                               erMann=as.numeric(input$erMannAndel),
                                               luftvei = as.numeric(input$covidvalgAndel),
                                               tidsenhet = input$tidsenhetAndelTid,
                                               enhetsUtvalg = input$enhetsUtvalgAndelTid,
                                               lagFig=0, session=session)
                  tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid)


                  output$andelTidTab <- function() {
                        antKol <- ncol(tabAndelTid)
                        kableExtra::kable(tabAndelTid, format = 'html'
                                          , full_width=F
                                          , digits = c(0,1,0,1)[1:antKol]
                        ) %>%
                              kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                              kableExtra::column_spec(column = 1, width_min = '7em') %>%
                              kableExtra::column_spec(column = 2:(antKol+1), width = '7em') %>%
                              kableExtra::row_spec(0, bold = T)
                  }
                  output$lastNed_tabAndelTid <- downloadHandler(
                    filename = function(){
                      paste0(input$valgtVarAndel, '_andelTid.csv')
                    },
                    content = function(file, filename){
                      write.csv2(tabAndelTid, file, row.names = T, na = '')
                    })


                  #AndelGrVar
                  AndelerShus <- NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                                                    datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                                                    minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                                                    erMann=as.numeric(input$erMannAndel),
                                                    luftvei = as.numeric(input$covidvalgAndel),
                                                    lagFig = 0, session=session)
                  tabAndelerShus <- cbind(Antall=AndelerShus$Ngr$Hoved,
                                          Andeler = AndelerShus$AggVerdier$Hoved)

                  output$andelerGrVarTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
                        antKol <- ncol(tabAndelerShus)
                        kableExtra::kable(tabAndelerShus, format = 'html'
                                          #, full_width=T
                                          , digits = c(0,1) #,0,1)[1:antKol]
                        ) %>%
                              kableExtra::column_spec(column = 1, width_min = '5em') %>%
                              kableExtra::column_spec(column = 2:(antKol+1), width = '4em') %>%
                              kableExtra::row_spec(0, bold = T)
                  }
                  output$lastNed_tabAndelGrVar <- downloadHandler(
                    filename = function(){
                      paste0(input$valgtVarAndel, '_andelGrVar.csv')
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


#--------------Gjennomsnitt---------------
       output$gjsnGrVar <- renderPlot({
            NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                            datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                            minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                            erMann=as.numeric(input$erMannGjsn),
                            luftvei = as.numeric(input$covidvalgGjsn),
                            valgtMaal = input$sentralmaal)
      }, height=900, width=700
      )
            output$LastNedFigGjsnGrVar <- downloadHandler(
              filename = function(){
                paste0('FigurGjsnGrVar_',input$valgtVarGjsn , Sys.time(), '.', input$bildeformatGjsn)
              },
              content = function(file){
                NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                erMann=as.numeric(input$erMannGjsn),
                                luftvei = as.numeric(input$covidvalgGjsn),
                                valgtMaal = input$sentralmaal,
                               outfile = file)
              }
            )

      output$gjsnTid <- renderPlot({
            NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                          reshID = user$org(),
                          datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                          minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                          erMann=as.numeric(input$erMannGjsn),
                          luftvei = as.numeric(input$covidvalgGjsn),
                          valgtMaal = input$sentralmaal,
                          tidsenhet = input$tidsenhetGjsn,
                          enhetsUtvalg = input$enhetsUtvalgGjsn,
                          session=session)
      }, height=400, width = 1200
      )

      output$LastNedFigGjsnTid <- downloadHandler(
        filename = function(){
          paste0('FigurGjsnTid',input$valgtVarGjsn , Sys.time(), '.', input$bildeformatGjsn)
        },
        content = function(file){
          NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                        reshID = user$org(),
                        datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                        minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                        erMann=as.numeric(input$erMannGjsn),
                        luftvei = as.numeric(input$covidvalgGjsn),
                        valgtMaal = input$sentralmaal,
                        tidsenhet = input$tidsenhetGjsn,
                        enhetsUtvalg = input$enhetsUtvalgGjsn,
                        session=session,
                          outfile = file)
        }
      )
      observe({
        dataUtGjsnGrVar <- NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                           datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                           minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                           erMann=as.numeric(input$erMannGjsn),
                                           luftvei = as.numeric(input$covidvalgGjsn),
                                           valgtMaal = input$sentralmaal, lagFig = 0)
        tabGjsnGrVar <- cbind(Antall = dataUtGjsnGrVar$Ngr$Hoved,
                              Sentralmål = dataUtGjsnGrVar$AggVerdier$Hoved)
        colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')

        output$tabGjsnGrVar <- function() {
          kableExtra::kable(tabGjsnGrVar, format = 'html'
                            , full_width=F
                            , digits = c(0,1) #,1,1)[1:antKol]
          ) %>%
            kableExtra::column_spec(column = 1, width_min = '7em') %>%
            kableExtra::column_spec(column = 2:3, width = '7em') %>%
            kableExtra::row_spec(0, bold = T)
        }

        output$lastNed_tabGjsnGrVar <- downloadHandler(
          filename = function(){
            paste0(input$valgtVarGjsn, '_gjsnGrVar.csv')
          },
          content = function(file, filename){
            write.csv2(tabGjsnGrVar, file, row.names = T, na = '')
          })

        output$tittelGjsn <- renderUI(
          tagList(
            h3(HTML(paste(dataUtGjsnGrVar$tittel, '<br />'))),
            #br(),
            h5(HTML(paste0(dataUtGjsnGrVar$utvalgTxt, '<br />')))
          ))
        dataUtGjsnTid <- NIRFigGjsnTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                                       reshID = user$org(),
                                       datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                       minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                                       erMann=as.numeric(input$erMannGjsn),
                                       luftvei = as.numeric(input$covidvalgGjsn),
                                       valgtMaal = input$sentralmaal,
                                       tidsenhet = input$tidsenhetGjsn,
                                       enhetsUtvalg = input$enhetsUtvalgGjsn,
                                       session = session) #, lagFig=0)

          if (dataUtGjsnTid$N$Hoved < 3) {
            tabGjsnTid <- 'N<3'
            output$tabGjsnTid <- renderText('Færre enn 3 registreringer')
          } else {
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
                kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                #kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                kableExtra::column_spec(column = 1, width_min = '7em') %>%
                kableExtra::column_spec(column = 2:(antKol+1), width = '7em') %>%
                kableExtra::row_spec(0, bold = T)
            }

          }
        output$lastNed_tabGjsnTid <- downloadHandler(
          filename = function(){
            paste0(input$valgtVarGjsn, '_gjsnTid.csv')
          },
          content = function(file, filename){
            write.csv2(tabGjsnTid, file, row.names = T, na = '')
          })

      })

#--------------SMR----------------------------------
      output$SMRfig <- renderPlot({
        #NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar = 'alder')
        NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar = input$valgtVarMort, #valgtVar='SMR',
                        datoFra=input$datovalgSMR[1], datoTil=input$datovalgSMR[2],
                        minald=as.numeric(input$alderSMR[1]), maxald=as.numeric(input$alderSMR[2]),
                        erMann=as.numeric(input$erMannSMR),
                        luftvei = as.numeric(input$covidvalgSMR)
                    )
      }, #, height=900, width=700 #heigth = 8000, width=800
      height = function() {2.2*session$clientData$output_SMRfig_height}, #
      width = function() {0.8*session$clientData$output_SMRfig_width}
      )

      output$LastNedFigSMR <- downloadHandler(
        filename = function(){
          paste0('FigurSMR_', Sys.time(), '.', input$bildeformatSMR)
        },
        content = function(file){
          NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar = input$valgtVarMort, #valgtVar='SMR',
                          datoFra=input$datovalgSMR[1], datoTil=input$datovalgSMR[2],
                          minald=as.numeric(input$alderSMR[1]), maxald=as.numeric(input$alderSMR[2]),
                          erMann=as.numeric(input$erMannSMR),
                          luftvei = as.numeric(input$covidvalgSMR),
                             outfile = file)
        }
      )


      observe({
        dataUtSMR <- NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarMort,
                                     datoFra=input$datovalgSMR[1], datoTil=input$datovalgSMR[2],
                                     minald=as.numeric(input$alderSMR[1]), maxald=as.numeric(input$alderSMR[2]),
                                     luftvei = as.numeric(input$covidvalgSMR),
                                     erMann=as.numeric(input$erMannSMR), lagFig = 0)
        output$SMRtab <- function() {
          tabSMR <- cbind(Antall = dataUtSMR$Ngr$Hoved,
                          SMR = dataUtSMR$AggVerdier$Hoved)
          #colnames(tabGjsnGrVar)[2] <- ifelse(input$sentralmaal == 'Med', 'Median', 'Gjennomsnitt')

          kableExtra::kable(tabSMR, format = 'html'
                            , full_width=F
                            , digits = c(0,2) #,1,1)[1:antKol]
          ) %>%
            kableExtra::column_spec(column = 1, width_min = '7em') %>%
            kableExtra::column_spec(column = 2:3, width = '7em') %>%
            kableExtra::row_spec(0, bold = T)
        }
        output$tittelSMR <- renderUI(
          tagList(
            h4(HTML(paste(dataUtSMR$tittel, sep= '<br />'))),
            h5(HTML(paste0(dataUtSMR$utvalgTxt, '<br />')))
          ))

      })

      output$innMaate <- renderPlot({
        NIRFigInnMaate(RegData=RegData, preprosess=0, valgtVar='InnMaate',
                       datoFra=input$datovalgInnMaate[1], datoTil=input$datovalgInnMaate[2],
                       minald=as.numeric(input$alderInnMaate[1]), maxald=as.numeric(input$alderInnMaate[2]),
                       erMann=as.numeric(input$erMannInnMaate),
                       luftvei= as.numeric(input$covidvalgInnMaate),
                       session=session)
      }, height = function() {2.2*session$clientData$output_innMaate_height},
      width = function() {0.7*session$clientData$output_innMaate_width}) #, height=900, width=700)

      output$LastNedFigTypeOpph <- downloadHandler(
        filename = function(){
          paste0('FigurTypeOpph_', Sys.time(), '.', input$bildeformatTypeOpph)
        },
        content = function(file){
          NIRFigInnMaate(RegData=RegData, preprosess=0, valgtVar='InnMaate',
                         datoFra=input$datovalgInnMaate[1], datoTil=input$datovalgInnMaate[2],
                         minald=as.numeric(input$alderInnMaate[1]), maxald=as.numeric(input$alderInnMaate[2]),
                         erMann=as.numeric(input$erMannInnMaate),
                         luftvei= as.numeric(input$covidvalgInnMaate),
                       session=session,
                          outfile = file)
        }
      )
#------------Pårørende-------------------

      # if (antPaaror>0){
      # output$paarorFord <- renderPlot(
      #   NIRFigPrePostPaaror(RegData=PaarorData, preprosess = 0, valgtVar=input$valgtVarPaarorFord,
      #                       startDatoIntervensjon = input$startDatoIntervensjon,
      #                       datoFra=input$datovalgPaarorFord[1], datoTil=input$datovalgPaarorFord[2],
      #                       reshID = user$org(),
      #                       enhetsUtvalg = input$enhetsUtvalgPaarorFord,
      #                       erMann=as.numeric(input$erMannPaarorFord,session=session)
      #   ), width=800, height = 800 #execOnResize=TRUE,
      # )}

#------------------ Abonnement ----------------------------------------------
      orgs <- as.list(sykehusValg[-1])
      paramNames <- shiny::reactive(c('reshID', 'brukernavn'))
      paramValues <- shiny::reactive(c(user$org(), user$name()))

    #  cat("WORKDIR: ", getwd())
     rapbase::autoReportServer(
        id = "intensivAbb",
        registryName = "intensiv",
        type = "subscription",
        # paramNames = paramNames,
        # paramValues = paramValues,
        reports = list(
          MndRapp = list(
            synopsis = "Intensiv: månedsrapport, abonnement",
            fun = "abonnement",
            paramNames = c('rnwFil',  "reshID"),  # ,"datoFra", 'datoTil'),
            paramValues = c('NIRmndRapp.Rnw', "user$org()") #  Sys.Date()-180, Sys.Date())
          )
          # ,SamleRapp = list(
          #   synopsis = "Intensiv/Rapporteket: Samlerapport, abonnement",
          #   fun = "abonnement",
          #   paramNames = c('rnwFil', "reshID"),   # ,"datoFra", 'datoTil'),
          #   paramValues = c('NIRSamleRapp.Rnw', "user$org()")   # ,Sys.Date()-180, Sys.Date())
          # )
        ),
        orgs = orgs,
        user = user
      )

#-------------Registeradministrasjon -----------------
     observeEvent(user$role(), {
       if (user$role() == 'SC') {

         #---Utsendinger---------------
         org <- rapbase::autoReportOrgServer("NIRuts", orgs)
         # oppdatere reaktive parametre, for å få inn valgte verdier (overskrive de i report-lista)
         paramNames <- shiny::reactive("reshID")
         paramValues <- shiny::reactive(org$value())
         vis_rapp <- shiny::reactiveVal(FALSE)
         shiny::observeEvent(user$role(), {
           vis_rapp(user$role() == "SC")
         })

         rapbase::autoReportServer2(
           id = "NIRuts",
           registryName = "intensiv",
           type = "dispatchment",
           org = org$value,
           paramNames = paramNames,
           paramValues = paramValues,
           reports = list(
             MndRapp = list(
               synopsis = "Rapporteket-Intensiv: Månadsrapport",
               fun = "abonnement",
               paramNames = c('rnwFil', "reshID"),
               paramValues = c('NIRmndRapp.Rnw',  "user$org()"))
             # SamleRapp = list(
             #   synopsis = "Rapporteket-Intensiv: Rapport, div. resultater",
             #   fun = "abonnement",
             #   paramNames = c('rnwFil', "reshID"),
             #   paramValues = c('NIRSamleRapp.Rnw', 'Alle'))
           ),
           orgs = orgs,
           eligible = vis_rapp,
           user = user )


         #----------- Eksport ----------------
         ## brukerkontroller
         rapbase::exportUCServer2("intensivExport", "nir",
                                 "intensiv",
                                 eligible = vis_rapp
         )
         ## veileding
         rapbase::exportGuideServer("intensivExportGuide", "intensiv")
       }
     })
} #serverdel

# Run the application
shinyApp(ui = ui, server = server)

