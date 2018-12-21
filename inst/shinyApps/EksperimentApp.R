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
library(gridExtra)


# Define UI for application that draws figures
ui <- #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      navbarPage(
            title= paste0("Testing testing, INTENSIV   ", 'Enhetsnavn   ', 'Brukernavn') ,
            #title = "Brukernavn", #theme = "bootstrap.css",
            #breddeSidekol <- 3,
            # Application title
            #titlePanel(paste0("Testing testing, INTENSIV   ", 'Enhetsnavn   ', 'Brukernavn') ),
            
            tabPanel(
                  "Andeler",
                  sidebarLayout(
                        sidebarPanel(
                              selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                                          choices = c('Alder minst 80 år' = 'alder_over80',
                                                      'Død innen 30 dager' = 'dod30d',
                                                      'Trakeostomi, åpen' = 'trakAapen')
                              ),
                              dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
                                             label = "Tidsperiode", separator="t.o.m.", language="nb"),
                              sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                                          max = 110, value = c(0, 110))
                        ),
                        mainPanel(  
                              h2("Sykehusvise andeler og utvikling over tid for valgt variabel"),
                              br(),
                              tabsetPanel(
                                    tabPanel(
                                          "Figurer",
                                          #column(10,
                                          h3(em("Utvikling over tid")),
                                          plotOutput("andelTidFig", height = 'auto'),
                                          br(),
                                          h3(em("Sykehusvise resultater")),
                                          plotOutput("andelerGrVarFig", height='auto')
                              ),
                              tabPanel("Tabeller",
                                       fluidRow(
                                             column(width = 3, 
                                                    h3("Utvikling over tid"),
                                                    tableOutput("andelTidTab")),
                                             column(width = 9, 
                                                    h3("Sykehusvise resultater"),
                                                    tableOutput("andelerGrVarTab")))
                                       #tableOutput('andelTidTab'),
                                       #tableOutput('andelerGrVarTab')
                                       #DT::DTOutput("andelerGrVarTab")
                                       
                              ))
                        ) #mainPanel
                  ) #SidebarLayout
            )
      ) #Navbarpage



#----------------- Define server logic required to draw a histogram
server <- function(input, output, session) { #session er med for å hente info fra R-sesjonen.
      
      library(intensiv)
      library(xtable)
      load('A:/Intensiv/NIRdata10000.Rdata')
      RegData <- NIRPreprosess(RegData = RegData)
      datoTil <- as.POSIXlt(Sys.Date())
      AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
      aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
      reshIDdummy <- 109773 #Tromsø med.int
      
      output$andelTidFig <- renderPlot({
            NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                           reshID=reshIDdummy, tidsenhet = 'Mnd',
                           datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                           minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2])
            )
      }, height = 400, width=1000 #height = function() {session$clientData$output_andelTid_width} #
      )
      output$andelTidTab <- renderTable({ #renderDT({
            AndelerTid <- NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                         reshID=reshIDdummy, tidsenhet = 'Mnd',
                                         datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                                         minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                                         lagFig=0)
            tabAndelTid <- cbind(Tidspunkt = names(AndelerTid$AggVerdier$Hoved),
                                    Andeler = sprintf('%.1f', AndelerTid$AggVerdier$Hoved,1))
            #as.table(tabAndelerShus)
            #xtable(tabAndelerShus, digits = c(0,0,1))
      }, 
      #digits=1,
      spacing="xs" #,height='60%' #width='60%', 
      )
      
      output$andelerGrVarFig <- renderPlot({
            AndelerShus <- NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                              datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                                              minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2])
            )
      }, height = 1000, width=800 #height = function() {session$clientData$output_andelerGrVarFig_width} #
      )
      
      output$andelerGrVarTab <- renderTable({ #renderDT({
            AndelerShus <- NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                                              datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                                              minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2]),
                                              lagFig = 0)
            tabAndelerShus <- cbind(Antall=AndelerShus$Ngr$Hoved,
                                    Andeler = sprintf('%.1f', AndelerShus$AggVerdier$Hoved,1))
            #as.table(tabAndelerShus)
            #xtable(tabAndelerShus, digits = c(0,0,1))
      }, 
      rownames=T,
      spacing="xs" #,height='60%' #width='60%', 
      )
      
      }

# Run the application
shinyApp(ui = ui, server = server)
