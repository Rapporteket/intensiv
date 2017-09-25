#' Provider of Highchart object
#' 
#' Provide Highchart object when no data
#' 
#' @param Tittel String Title to show in empty chart
#' @param infoText String Text to shown in empty chart
#' @return h1 Highchart object
#' @export

emptyHighchart <- function(Tittel, utvalg = "", infoText = "Tomt...") {
  
  h1 <- highcharter::highchart(hc_opts = list(lang=list(noData=infoText))) %>%
    hc_title(text = Tittel) %>%
    hc_subtitle(text = utvalg) %>%
    hc_add_series(name = infoText,
                  type ="bar",
                  data = NULL)
  
  return(h1)
}
