#' Server logic function for shiny 'intensiv'
#'
#' nakkeStandard is the module server function for shiny Intensiv
#'
#' @param input Aggregert informasjon inn
#' @param output Figur ut
#' @param session kjøring
#' @return reportObjects list of objects returned from the module, typical
#' plotObj and tableObj
#' @export

serverModule <- function(input, output, session) {
  sessionName <- session$ns("name")
  # namespace id comes with an extra '-name'. Remove it
  sessionName <- gsub("-name", "", sessionName)
  
  if (sessionName == "readmission72hours") {
    plotObj <- reactive({
      readmission72hours()
    })
  }
  
  if (sessionName == "gjsnGrVar") {
    plotObj <- reactive({
      gjsnGrVar()
    })
  }
  
  #reportObjects = list(plotObj=plotObj, tableObj=tableObj)
  
  return(plotObj)
}
