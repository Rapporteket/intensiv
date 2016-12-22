# This is the user-interface definition for the 'report' Shiny web application
# for 'Intensiv'

require(highcharter)
data("AndelerGrVar")

# not implemented just yet, wait until sample data without preprocessing
if (1==0) {
  RegData <- NakkePreprosess(RegData=RegData)
}

shinyUI(
  navbarPage(title = "INTENSIVREGISTERET", theme = "bootstrap.css",
             tabPanel("Andeler",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("andelerValgtVar", "Variabel:",
                                      valgtVarFigAndeler(),
                                      selected = "Alder"),
                          nakkeStandardInput("figAndeler"),
                          downloadButton("downloadData",
                                         label = "Last ned data")
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel("Figur",
                                   highchartOutput("andelerPlot")
                          ),
                          tabPanel("Data",
                                   DT::dataTableOutput("andelerTable")
                          )
                        ))
                      )
             )
  )
)