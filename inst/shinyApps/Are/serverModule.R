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
      # NB need a generic data set for uc-content
      d <- NIRdata01reinn$NIRRegData01Off
      c_subset <- dplyr::filter(d, ShType %in% h_type) %>% 
        dplyr::distinct(ShNavn)
      hospital_names <- sort(c_subset$ShNavn)
      
      updateSelectInput(session, "hospital",
                        choices = hospital_names,
                        selected = hospital_names)
    }
    
  })
  
  if (sessionName == "readmission72hours") {
    reportObj <- reactive({
      
      ## do data filtering here and replace by call to generic report function
      fRegData <- NIRdata01reinn$NIRRegData01Off
      
      # the inputs
      selectYear <- as.numeric(input$year)
      selectQuarter <- as.numeric(input$quarter)
      selectHospital <- as.character(input$hospital)
      selectHospitalType <- as.numeric(input$hospitalType)
      selectErMann <- as.numeric(input$erMann)
      selectAgeGroup <- as.character(input$ageGroup)
      
      # apply all filters for RegData and make
      fRegData <- dplyr::filter(fRegData, ShNavn %in% selectHospital &
                                  Aar %in% selectYear & Kvartal %in% selectQuarter &
                                  AldersGr %in% selectAgeGroup)
      
      # Here: replace NIRRegData01Off with fRegData, all to be sent into NirAnderleGrVar
      RegData <- NIRdata01reinn
      RegData$NIRRegData01Off <- fRegData
      
      # in case filtering makes empty data
      if (is.data.frame(fRegData) && nrow(fRegData) == 0) {
        emptyReport(Tittel = fRegData$tittel, infoText = "Ingen data")
      } else {
        shinyReport(RegData, valgtVar = "reinn", selectErMann = selectErMann)
      }
      
      # readmission72hours(selectYear = as.numeric(input$year),
      #                    selectQuarter = as.numeric(input$quarter),
      #                    selectHospital = as.character(input$hospital),
      #                    selectHospitalType = as.numeric(input$hospitalType),
      #                    selectErMann = as.numeric(input$erMann),
      #                    selectAgeGroup = as.character(input$ageGroup))
    })
  }
  
  if (sessionName == "gjsnGrVar") {
    reportObj <- reactive({
      ## do data filtering here and replace by call to generic report function
      fRegData <- NIRdata01respiratortid$NIRRegData01Off
      
      # the inputs
      selectYear <- as.numeric(input$year)
      selectQuarter <- as.numeric(input$quarter)
      selectHospital <- as.character(input$hospital)
      selectHospitalType <- as.numeric(input$hospitalType)
      selectErMann <- as.numeric(input$erMann)
      selectAgeGroup <- as.character(input$ageGroup)
      
      # apply all filters for RegData and make
      fRegData <- dplyr::filter(fRegData, ShNavn %in% selectHospital &
                                  Aar %in% selectYear & Kvartal %in% selectQuarter &
                                  AldersGr %in% selectAgeGroup)
      
      # Here: replace NIRRegData01Off with fRegData, all to be sent into NirAnderleGrVar
      RegData <- NIRdata01respiratortid
      RegData$NIRRegData01Off <- fRegData
      
      # in case filtering makes empty data
      if (is.data.frame(fRegData) && nrow(fRegData) == 0) {
        emptyReport(Tittel = fRegData$tittel, infoText = "Ingen data")
      } else {
        shinyReport(RegData, valgtVar = "respStotte", selectErMann = selectErMann)
      }
    })
  }
  
  #reportObjects = list(plotObj=plotObj, tableObj=tableObj)
  
  return(reportObj)
}
