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
    callModule(serverModule, "andelerGrVar",
               session = getDefaultReactiveDomain()
    )
  
  serverModuleGjsnGrVar <-
    callModule(serverModule, "gjsnGrVar",
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
  
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(file) {
      out <- serverModuleFigAndelerGrVar()
      write.table(out$tableObj$t1, file, row.names = FALSE)
    }
  )
  
  output$gjsnGrVarPlot <- renderHighchart({
    out <- serverModuleGjsnGrVar()
    return(out$plotObj)
  })
  
  output$gjsnGrVarTable <- DT::renderDataTable({
    out <- serverModuleGjsnGrVar()
    return(out$tableObj$w1)
  })
  
})
