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
ui <- fluidPage( #"Hoved"Layout for alt som vises på skjermen
      #navbarPage(title = "INTENSIVREGISTERET", theme = "bootstrap.css",
      breddeSidekol <- 3,
  # Application title
  titlePanel(paste0("Testing testing, INTENSIV   ", 'Enhetsnavn   ', 'Brukernavn') ),

  # Velge sykehus og vise antall
  #sidebarLayout( #Definerer overordnet layout med en sidekolonne og ett hovedpanel.
    #sidebarPanel( #Området som viser "valgbokser"
	fluidRow(column(width = breddeSidekol, #Første kolonne. Alternativ til sidebarLayout(sidebarPanel())
	 conditionalPanel( #
        'input.ark == "Andeler"',
        #'input.ark === "Fordelinger" || input.ark === "Andeler" ',
        selectInput(inputId = "valgtVarAndelGrVar", label="Velg variabel",
                    choices = c('Alder minst 80 år' = 'alder_over80',
                                'Død innen 30 dager' = 'dod30d',
                                'Trakeostomi, åpen' = 'trakAapen')
        ),
        dateRangeInput(inputId = 'datovalgAndelGrVar', start = "2017-01-01", end = Sys.Date(),
                       label = "Tidsperiode", separator="t.o.m.", language="nb"),
        sliderInput(inputId="alderAndelGrVar", label = "Alder", min = 0,
                    max = 110, value = c(0, 110))
        )
      ),

      column(width = 9,
             tabsetPanel(id='ark', 
         tabPanel("Andeler",
                  h2("Sykehusvise andeler og utvikling over tid for valgt variabel"),
                  br(),
                  #column(10,
                         h3(em("Utvikling over tid")),
                         plotOutput("andelTid"),
                  br(),
                  h3(em("Sykehusvise resultater")),
                  column(width = 7, #height='200px',
                         plotOutput("andelerGrVar")),
                  column(width = 5, #height=9,
                         #tableOutput(
                         "TABELL")
          )))
      
  )
)



#----------------- Define server logic required to draw a histogram
server <- function(input, output) {

  library(intensiv)
  load('A:/Intensiv/NIRdata10000.Rdata')
  RegData <- NIRPreprosess(RegData = RegData)
  datoTil <- as.POSIXlt(Sys.Date())
  AarNaa <- as.numeric(format(Sys.Date(), "%Y"))
  aarFra <- paste0(1900+as.POSIXlt(Sys.Date())$year-5, '-01-01')
  reshIDdummy <- 109773 #Tromsø med.int
  
  
  output$andelerGrVar <- renderPlot({

 tabAndelerShus <- NIRFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                    datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                    minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2])
                    )
  }, height = 1000, width=800
  )

  output$andelTid <- renderPlot({

    NIRFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndelGrVar,
                         reshID=reshIDdummy, tidsenhet = 'Mnd',
                         datoFra=input$datovalgAndelGrVar[1], datoTil=input$datovalgAndelGrVar[2],
                         minald=as.numeric(input$alderAndelGrVar[1]), maxald=as.numeric(input$alderAndelGrVar[2])
                        )
  }, height = 400, width=1000)

}

# Run the application
shinyApp(ui = ui, server = server)
