## Only run this example in interactive R sessions
if (interactive()) {
    ui <- navbarPage('tittel',
                     id='jelp',
        sidebarPanel(
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
            )
        )
        # mainPanel(
        #     plotOutput("plot")
        # )
    )
    
    server <- function(input, output) {
        # x <- rnorm(100)
        # y <- rnorm(100)
        
        #output$plot <- renderPlot({
            # if (input$plotType == "scatter") {
            #plot(x, y)
            # } else {
            #   breaks <- input$breaks
            #   if (breaks == "custom") {
            #     breaks <- input$breakCount
            #   }
            # 
            #   hist(x, breaks = breaks)
            # }
       # })
    }
    
    shinyApp(ui, server)
}
