# This is the server logic for the 'report' Shiny web application for
# 'intensiv'

# load libs, scripts and data here, once
require(intensiv)
require(highcharter)

shinyServer(function(input, output) {
  
  # reuse server module, but with different namespaces and per report user
  # controls outside namespace (if any)
  serverModuleFigAndeler <-
    callModule(serverModule, "figAndelerGrVar", session = getDefaultReactiveDomain(),
               valgtVar=reactive(input$andelerValgtVar)
    )
  

  
  # return of report objects
  output$andelerPlot <- renderHighchart({
    out <- serverModuleFigAndeler()
    return(out$plotObj)
  })
  
  output$andelerTable <- DT::renderDataTable(DT::datatable({
    out <- serverModuleFigAndeler()
    out$tableObj
  }, container = AndelerTableContainer(groupText = names(out$tableObj)[1],
                                       deptName = names(out$tableObj)[2]),
  rownames = FALSE,
  options = list(processing = FALSE,
                 paging = FALSE,
                 searching = FALSE)))
  
  output$downloadData <- downloadHandler(
    filename = "test.csv",
    content = function(file) {
      out <- serverModuleFigAndeler()
      write.table(out$tableObj, file)
    }
  )
})
