# This is the server logic for the 'report' Shiny web application for
# 'intensiv'

# load libs, scripts and data here, once
library(shiny)
library(DT)
require(intensiv)
require(highcharter)

shinyServer(function(input, output) {
  
  # reuse server module, but with different namespaces and per report user
  # controls outside namespace (if any)
  serverModuleFigAndelerGrVar <-
    callModule(serverModule, "figAndelerGrVar",
               session = getDefaultReactiveDomain()
    )
  
  serverModuleFigAndeler <-
    callModule(serverModule, "figAndeler",
               session = getDefaultReactiveDomain()
    )
  
  
  
  # return of report objects
  
  output$andelerGrVarPlot <- renderHighchart({
    out <- serverModuleFigAndelerGrVar()
    return(out$plotObj)
  })
  
  output$andelerGrVarTable <- DT::renderDataTable({
    out <- serverModuleFigAndelerGrVar()
    return(out$tableObj$w1)
  })
  
#   output$andelerGrVarTable <- DT::renderDataTable(DT::datatable({
#     out <- serverModuleFigAndelerGrVar()
#     out$tableObj
#   }, 
#   rownames = FALSE,
#   options = list(processing = FALSE,
#                  paging = FALSE,
#                  searching = FALSE,
#                  order = list())))
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(file) {
      out <- serverModuleFigAndelerGrVar()
      write.table(out$tableObj$t1, file)
    }
  )
  
  output$andelerPlot <- renderHighchart({
    reportObjects <- serverModuleFigAndeler()
    return(reportObjects)
  })
})
