#' Server logic function for shiny 'intensiv'
#'
#' nakkeStandard is the module server function for shiny Intensiv
#'
#' @param input
#' @param output
#' @param session
#' @return reportObjects list of objects returned from the module, typical
#' plotObj and tableObj
#' @export

serverModule <- function(input, output, session) {
  sessionName <- session$ns("name")
  # namespace id comes with an extra '-name'. Remove it
  sessionName <- gsub("-name", "", sessionName)
  
  if (sessionName == "figAndelerGrVar") {
    plotObj <- reactive({
      emptyReport(Tittel = "Test1")
    })
    # empty for initial testing
    tableObj <- list() 
  }
  
  if (sessionName == "figAndeler") {
    plotObj <- reactive({
      emptyHighchart(Tittel = "Test2")
    })
  }
  
  #reportObjects = list(plotObj=plotObj, tableObj=tableObj)
  
  return(plotObj)
}
