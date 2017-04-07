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
  
  # observe to catch changes in depending uc at client
  hospitalType <- reactive({
    if (is.null(input$hospitalType)) {
      return(NULL)
    } else {
      return(input$hospitalType)
    }
  })
  
  observe({
    h_type <- hospitalType()
    if (!is.null(h_type)) {
      c_subset <- dplyr::filter(reinnData$RegData, ShType == h_type) %>% 
        dplyr::distinct(ShNavn)
      
      updateSelectInput(session, "hospital",
                        choices = c_subset$ShNavn,
                        selected = c_subset$ShNavn)
    }
    
  })
  
  if (sessionName == "readmission72hours") {
    reportObj <- reactive({
      readmission72hours(selectErMann = as.numeric(input$erMann),
                         selectHospital = as.character(input$hospital))
    })
  }
  
  if (sessionName == "gjsnGrVar") {
    reportObj <- reactive({
      gjsnGrVar()
    })
  }
  
  #reportObjects = list(plotObj=plotObj, tableObj=tableObj)
  
  return(reportObj)
}
