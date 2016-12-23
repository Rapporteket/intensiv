#' Provider of report objects
#' 
#' Provide report object when no data (test replacement of NIRHighcharts)
#' 
#' @param Tittel String Title to show in empty chart
#' @param infoText String Text to shown in empty chart
#' @return plotObj Highchart object
#' @return tableObj Table object
#' @export

emptyReport <- function(Tittel, utvalg = "", infoText = "Tomt...") {
  
  # highchart object
  h1 <- highcharter::highchart(hc_opts = list(lang=list(noData=infoText))) %>%
    hc_title(text = Tittel) %>%
    hc_subtitle(text = utvalg) %>%
    hc_add_series(name = infoText,
                  type ="bar",
                  data = NULL)
  
  # table object
  d <- data("AndelerGrVarData")
  
  t1 <- list()
  
  
  list(plotObj=h1, tableObj=t1)
}
