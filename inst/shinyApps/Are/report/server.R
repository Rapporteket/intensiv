# This is the server logic for the 'report' Shiny web application for
# 'intensiv'

# load libs, scripts and data here, once
library(shiny)
library(DT)
require(intensiv)
require(highcharter)

shinyServer(function(input, output, session) {
  
  # reuse server module, but with different namespaces and per report user
  # controls outside namespace (if any)
  serverModuleReadmission72hours <-
    callModule(serverModule, "readmission72hours",
               session = getDefaultReactiveDomain()#,
               #erMann = reactive(input$erMann),
               
    )
  
  serverModuleGjsnGrVar <-
    callModule(serverModule, "gjsnGrVar",
               session = getDefaultReactiveDomain()
    )

  
  # return of report objects
  
  output$readmission72hoursPlot <- renderHighchart({
    out <- serverModuleReadmission72hours()
    return(out$plotObj)
  })
  
  output$readmission72hoursTable <- DT::renderDataTable({
    out <- serverModuleReadmission72hours()
    return(out$tableObj$w1)
  })
  
  
  output$downloadDataReadmission72hours <- downloadHandler(
    filename = "andelerGrVar.csv",
    content = function(file) {
      out <- serverModuleReadmission72hours()
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
