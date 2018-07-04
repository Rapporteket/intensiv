#' runShinyAppReports starts shiny reports
#'
#' runShinyAppReports presents figs and stuff from the package in shiny
#' @export

runShinyAppReports <- function() {
  
  appName <- "report"
  appsDirectoryName <- "shinyApps"
  packageName <- "intensiv"
  
  rapbase::runShinyApp(appName, appsDirectoryName, packageName)
}
