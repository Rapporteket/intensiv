#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
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

#ARBEIDSPLAN:
#Skill  ut brukerkontroller i egen funksjon


# Define UI for application that draws figures
ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = 'NORSK INTENSIVREGISTER',
      tabPanel("Viktigste resultater",
               #fluidRow(
               #column(width=5,
               h2("Månedsrapport"), #),
               #column(width=2,
               #downloadButton(outputId = 'mndRapp.pdf', label='FUNKER pt IKKE', class = "butt"),
               #tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               #)),
               br(),
               br(),
               br(),
               br(),
               br(),
               h2("Her kan det komme ei startside med de variable/resultater som er viktigst å overvåke", align='center' ),
               h2("Gi tilbakemelding på hva man ønsker her.", align='center' ),
               br()),
      
      tabPanel("Registreringsoversikter",
               #h3("1. Antall opphold + pasienter per sykehus og år"),
               #h3("2. Antall opphold + pasienter per sykehus og mnd, siste 12 mnd. ")
               h2("Belegg"),
               tableOutput('tabBelegg'),
               
               h2("Antall registreringer per måned og avdeling"),
               p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
               tableOutput("tabAvdMnd12")
               
               #          h2("Antall registreringer per år og avdeling, siste 5 år"),
               #          #tableOutput("tabAvdNAar5")
      ),
      
      
      #-------Fordelinger----------      
      
      tabPanel("Fordelinger",
               # fluidRow(column(width = 3, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
               sidebarPanel(
                     selectInput(inputId = "valgtVar", label="Velg variabel",
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
                                             'Spesielle tiltak' = 'spesTiltak')
                     ),
                     dateRangeInput(inputId = 'datovalg', start = "2017-01-01", end = Sys.Date(),
                                    label = "Tidsperiode", separator="t.o.m.", language="nb"),
                     selectInput(inputId = "erMann", label="Kjønn",
                                 choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                     ),
                     sliderInput(inputId="alder", label = "Alder", min = 0,
                                 max = 110, value = c(0, 110)
                     ),
                     selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                 choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
                     )
                     #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                     #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               mainPanel(
                     plotOutput('fordelinger')
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
                     selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
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
                                             'Utvidet hemodyn. overvåkning' = 'ExtendedHemodynamicMonitoring',
                                             'Trakeostomi' = 'trakeostomi',
                                             'Trakeostomi, åpen' = 'trakAapen')
                     ),
                     dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
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
                     fluidRow(column(6, plotOutput("andelTid"))),
                     br(),
                     br(),
                     fluidRow(
                           column(6, plotOutput("andelerGrVar") ) #, div(style = "height:100px")) #height='1000px') # '400px'
                     )   
               )
      ), #tab
      
      
      #------- Gjennomsnitt ----------      
      tabPanel("Gjennomsnitt",
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
                     dateRangeInput(inputId = 'datovalgGjsn', start = "2017-01-01", end = Sys.Date(),
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
                     h2("Sykehusvise gjennomsnitt/median og utvikling over tid for valgt variabel"),
                     h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                  til venstre. Man kan også gjøre ulike filtreringer."),
                     br(),
                     br(),
                     plotOutput("gjsnTid"),
                     plotOutput("gjsnGrVar")
               )
      ),
      
      #--------SMR--------------
      tabPanel('SMR',
               h3('SMR: Standardisert mortalitetsratio'),
               sidebarPanel(
                     dateRangeInput(inputId = 'datovalgSMR', start = "2017-01-01", end = Sys.Date(),
                                    label = "Tidsperiode", separator="t.o.m.", language="nb"),
                     selectInput(inputId = "erMannSMR", label="Kjønn",
                                 choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                     ),
                     sliderInput(inputId="alderSMR", label = "Alder", min = 0,
                                 max = 110, value = c(0, 110)
                     )
               ),
               mainPanel(
                     plotOutput('SMR')
               )
      ), #tab
      
      #--------Type opphold--------------
      tabPanel('Type opphold',
               h3('Type opphold'),
               sidebarPanel(
                     dateRangeInput(inputId = 'datovalgInnMaate', start = "2017-01-01", end = Sys.Date(),
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
      
      #-------Under utvikling----------      
      
      tabPanel("Under utvikling",
               br(),
               h3("HN-IKT-rapporter:"),
               h3("3. Dobbeltregistreringar"),
               h3("Resultater fra pårørendeskjema"),
               h3("4. \"Månedlig oversikt:\" Div. nøkkeltall for egen avd/hele landet, siste 12 måneder"),
               h3("5. Pasienter utskrevet mellom 17:00 og 08:00, helligdager beh. som vanlige dager"),
               h3('6. Potensielle donorer'),
               h3('7. Påvist opphevet sirkulasjon- organdonasjoner og grunn til ikke gjennomført'),
               h3('8. Overflyttinger mellom sykehus/avd.'),
               h3("9. Fordelinger alder og kjønn"),
               br()
      )
)  #navbarPage




#----------------- Define server logic ----------
server <- function(input, output, session) {
      
      library(intensiv)
      library(lubridate)
      library(zoo)
      
      context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
      if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
            RegData <- NIRRegDataSQL(datoFra = datoFr, datoTil = datoTil)
            
      } #hente data på server
      
      if (!exists('RegData')){
            #system.file('inst/IntensivMndRapp.Rnw', package='intensiv')
      load('A:/Intensiv/NIRdata10000.Rdata')
      #RegData <- read.table(fil, sep=';', header=T, encoding = 'UTF-8')
      #Funker:
      #  data('NIRRegDataSyn', package = 'intensiv')
      #try(data(package = "intensiv"))
      }
      datoTil <- as.POSIXlt(Sys.Date())
      AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
      aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
      reshIDdummy <- 109773 #Tromsø med.int
      RegData <- NIRPreprosess(RegData = RegData)
      
      # output$tabAvdMnd12 <- renderTable({
      #       datoFra12 <- as.Date(paste0(as.numeric(substr(input$datoTil,1,4))-1, substr(input$datoTil,5,8), '01'))
      #       SkjemaData12mnd <- SkjemaData[SkjemaData$InnDato < as.POSIXlt(input$datoTil)
      #                                     & SkjemaData$InnDato > as.POSIXlt(datoFra12), ]
      #       if (as.numeric(input$status) %in% 0:1) {SkjemaData12mnd <-
      #             SkjemaData12mnd[which(SkjemaData12mnd$SkjemaStatus == as.numeric(input$status)), ]
      #       }
      #       #Flyttes til overvåkning
      #       tabAvdSiste12mnd <- addmargins(table(SkjemaData12mnd[SkjemaData12mnd$SkjemaRekkeflg==2, c('Sykehusnavn', 'Mnd')]))
      #       colnames(tabAvdSiste12mnd) <- substring(colnames(tabAvdSiste12mnd),1,3)
      #       xtable::xtable(tabAvdSiste12mnd)
      # },
      # rownames = TRUE, digits=0 #, align = c('l', rep('r', ncol(tabAvdSiste12mnd)))
      # )
      # 
      
      
      #------------Tabeller 
      output$tabAvdSkjema12 <- renderTable({
            tabAntOpphSh12mnd(RegData = RegData, datoTil=input$datovalgTab[2])  
            #sprintf('%1.3f'
            #xtable::xtable(tabAvd12MndNskjema,  align = c('l', rep('r', ncol(tabAvd12MndNskjema))),
             #              caption= paste0('Tidsperiode: ', as.POSIXlt(datoFra12), 'til', as.POSIXlt(input$datoTil)))
      #},
      }, rownames = T, align= 'r' #
      ) 
      
      
      output$tabAvdNAar5 <- renderTable({
            
            tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
            rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle avdelinger:'
            colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
            xtable::xtable(tabAvdAarN)
            #xtable::xtable(tabAvdAarN)
      },
      rownames = T, digits=0)
      
      #output$tekstDash <- c('Figurer med kvalitetsindikatorer',
      #                      'hente ned månedsrapport'),
      output$mndRapp.pdf = downloadHandler(
            filename = 'MndRapp.pdf',
            #content = function(file) file.copy(system.file('NIRMndRapp.pdf', package = 'Nakke'), file, overwrite = TRUE),
            content = function(file) {
                  # permission to the current working directory
                  src <- normalizePath(system.file('NakkeMndRapp.Rnw', package='Nakke'))
                  owd <- setwd(tempdir())
                  on.exit(setwd(owd))
                  file.copy(src, 'NakkeMndRapp.Rnw', overwrite = TRUE)
                  
                  texfil <- knitr::knit(system.file('IntensivMndRapp.Rnw', package='intensiv'), encoding = 'UTF-8')
                  texi2pdf(system.file(texfil, package='Nakke'),clean = TRUE) #"NakkeMndRapp.tex"
                  # #help(render_latex)
                  #       out = system.file('NakkeMndRapp.pdf', package = 'Nakke')
                  #knit2pdf(system.file('NakkeMndRapp.Rnw', package='Nakke'), clean = TRUE, encoding = 'UTF-8')
                  #      file.rename(out, file) # move pdf to file for downloading
                  #file.copy(system.file('NakkeMndRapp.pdf', package='Nakke'), file)
                  file.copy('NakkeMndRapp.pdf', file)
                  
            },
            contentType = 'application/pdf'
      )
      #  If you already have made the PDF file, you can just copy it to file, i.e.
      #  content = function(file) file.copy('your_existing.pdf', file, overwrite = TRUE)
      
      output$tabBelegg <- renderTable({
            tabBelegg(RegData=RegData, personIDvar='PasientID' , tidsenhet='Mnd') })
      
      output$fordelinger <- renderPlot({
            
            NIRFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                          reshID=reshIDdummy, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                          datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                          minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                          erMann=as.numeric(input$erMann))
      }, height=600, width=600 #height = function() {session$clientData$output_fordelinger_width}
      )
      
      
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
      
      output$gjsnGrVar <- renderPlot({
            NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsn,
                            datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                            minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                            erMann=as.numeric(input$erMannGjsn),
                            valgtMaal = input$sentralmaal)
      }, height=800, width=700
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
      }, height=400, width = 1000
      )
      output$SMR <- renderPlot({
            NIRFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar='SMR',
                            datoFra=input$datovalgSMR[1], datoTil=input$datovalgSMR[2],
                            minald=as.numeric(input$alderSMR[1]), maxald=as.numeric(input$alderSMR[2]),
                            erMann=as.numeric(input$erMannSMR))
      }, heigth = 1000, width=700
      )
      
      output$innMaate <- renderPlot({
            NIRFigInnMaate(RegData=RegData, preprosess=0, valgtVar='InnMaate', 
                           datoFra=input$datovalgInnMaate[1], datoTil=input$datovalgInnMaate[2],
                           minald=as.numeric(input$alderInnMaate[1]), maxald=as.numeric(input$alderInnMaate[2]),
                           erMann=as.numeric(input$erMannInnMaate))
      }, height=800, width=800)
}


# Run the application
shinyApp(ui = ui, server = server)

