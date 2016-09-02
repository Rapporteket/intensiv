#' Test of Highchart for intensiv
#' 
#' asæglksa
#' 
#' @param outfile String providing fully qualified path and name of html widget
#' @importFrom magrittr "%>%"
#' @import highcharter
#' @export

AndelerHighchart <- function(outfile) {
  
  # just for testing
  h1 <- highchart() %>% 
    hc_chart(type = "column") %>% 
    hc_title(text = "A highcharter chart") %>% 
    hc_xAxis(categories = 2012:2016) %>% 
    hc_add_series(data = c(3900,  4200,  5700,  8500, 11900),
                  name = "Downloads")
  
  htmlwidgets::saveWidget(h1, outfile, selfcontained = TRUE)
}