# This is the server logic for the 'report' Shiny web application for
# 'intensiv'

# load libs, scripts and data here, once
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
  
  output$andelerGrVarTable <- DT::renderDataTable(DT::datatable({
    out <- serverModuleFigAndelerGrVar()
    out$tableObj
  }, 
  rownames = FALSE,
  options = list(processing = FALSE,
                 paging = FALSE,
                 searching = FALSE)))
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(file) {
      out <- serverModuleFigAndelerGrVar()
      write.table(out$tableObj, file)
    }
  )
  
  output$andelerPlot <- renderHighchart({
    reportObjects <- serverModuleFigAndeler()
    return(reportObjects)
  })
})
