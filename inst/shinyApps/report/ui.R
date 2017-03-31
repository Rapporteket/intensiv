# This is the user-interface definition for the 'report' Shiny web application
# for 'Intensiv'

require(highcharter)
#data("AndelerGrVarData")

# not implemented just yet, wait until sample data without preprocessing
if (1==0) {
  RegData <- NakkePreprosess(RegData=RegData)
}

shinyUI(
  navbarPage(title = "INTENSIVREGISTERET", theme = "bootstrap.css",
             tabPanel("AndelerGrVar",
                      sidebarLayout(
                        sidebarPanel(
                          uiInputModule("andelerGrVar"),
                          downloadButton("downloadDataAndelerGrVar",
                                         label = "Last ned data")
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel("Figur",
                                   highchartOutput("andelerGrVarPlot")
                          ),
                          tabPanel("Data",
                                   DT::dataTableOutput("andelerGrVarTable")
                          )
                        ))
                      )
             ),
             tabPanel("GjsnGrVar",
                      sidebarLayout(
                        sidebarPanel(
                          uiInputModule("gjsnGrVar"),
                          downloadButton("downloadDataGjsnGrVar",
                                         label = "Last ned data")
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel("Figur",
                                   highchartOutput("gjsnGrVarPlot")
                          ),
                          tabPanel("Data",
                                   DT::dataTableOutput("gjsnGrVarTable")
                          )
                        ))
                      )
             )
  )
)