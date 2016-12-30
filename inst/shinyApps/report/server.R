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
  serverModuleAndelerGrVar <-
    callModule(serverModule, "andelerGrVar",
               session = getDefaultReactiveDomain()
    )
  
  serverModuleGjsnGrVar <-
    callModule(serverModule, "gjsnGrVar",
               session = getDefaultReactiveDomain()
    )
  
  
  
  # return of report objects
  
  output$andelerGrVarPlot <- renderHighchart({
    out <- serverModuleAndelerGrVar()
    return(out$plotObj)
  })
  
  output$andelerGrVarTable <- DT::renderDataTable({
    out <- serverModuleAndelerGrVar()
    return(out$tableObj$w1)
  })
  
  
  output$downloadDataAndelerGrVar <- downloadHandler(
    filename = "andelerGrVar.csv",
    content = function(file) {
      out <- serverModuleAndelerGrVar()
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
  
  output$downloadDataGjsnGrVar <- downloadHandler(
    filename = "gjsnGrVar.csv",
    content = function(file) {
      out <- serverModuleGjsnGrVar()
      write.table(out$tableObj$t1, file, row.names = FALSE)
    })
  
})
