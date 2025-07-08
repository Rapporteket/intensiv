#' Modulised UI for shiny
#'
#' uiInputModule provides common ui uc for shiny intensiv
#'
#'  @param id namespace for this module
#'  @param label Label for this module
#'  @export

uiInputModule <- function(id, dat, label = "Brukervalg") {
  
  # create namespace
  ns <- NS(id)
  
  # make values and lables
  years <- sort(dplyr::distinct(dat, Aar)$Aar)
  hospital_names <- sort(dplyr::distinct(dat, ShNavn)$ShNavn)
  age_groups <- levels(dat$AldersGr)

  tagList(
    selectInput(inputId = ns("year"), label = "År:",
                choices = years,
                selected = years,
                multiple = TRUE
                ),
    selectInput(inputId = ns("quarter"), label = "Kvartal:",
                choices = 1:4,
                selected = 1:4,
                multiple = TRUE
                ),
    selectInput(inputId = ns("hospitalType"),
                label = "Sykehustype:",
                choices = c("Lokal" = 1,
                            "Sentral" = 2,
                            "Regional" = 3),
                selected = c(1, 2, 3),
                multiple = TRUE
    ),
    selectInput(inputId = ns("hospital"),
                label = "Sykehus:",
                choices = hospital_names,
                selected = hospital_names,
                multiple = TRUE
    ),
    selectInput(inputId = ns("erMann"),
                label = "Kjønn:",
                choices = c("Alle" = 2, "Menn" = 1, "Kvinner" = 0)
    ),
    selectInput(inputId = ns("ageGroup"), label = "Aldersgruppe:",
                choices = age_groups,
                selected = age_groups,
                multiple = TRUE
                )
    # sliderInput(ns("alder"), label = "Alder", min = 0,
    #             max = 130, value = c(0, 130)
    # ),
    # dateRangeInput(ns("periode"), start = "2012-01-01", end = Sys.Date(),
    #               label = "Periode", separator="til", language="nb")
    
  )
}