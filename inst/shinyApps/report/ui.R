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
             tabPanel("Reinnleggelse innen 72 timer",
                      sidebarLayout(
                        sidebarPanel(
                          uiInputModule(id = "readmission72hours",
                                        dat = NIRdata01$NIRRegData01Off),
                          downloadButton("downloadDataAndelerGrVar",
                                         label = "Last ned data")
                        ),
                        mainPanel(tabsetPanel(
                          tabPanel("Figur",
                                   highchartOutput("readmission72hoursPlot")
                          ),
                          tabPanel("Data",
                                   DT::dataTableOutput("readmission72hoursTable")
                          )
                        ))
                      )
             ),
             tabPanel("GjsnGrVar",
                      sidebarLayout(
                        sidebarPanel(
                          uiInputModule(id = "gjsnGrVar",
                                        dat = NIRdata01$NIRRegData01Off),
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