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


#' Provider of report objects
#' 
#' Provide report object fractions of readmittance within 72 hours
#' 
#' @param Tittel String Title to show in empty chart
#' @param infoText String Text to shown in empty chart
#' @return plotObj Highchart object
#' @return tableObj Table object
#' @export

readmission72hours <-  function() {
  
  # get (static) data
  data("AndelerGrVarData")
  
  ## hc
  # get actual color from name...
  figProps <- rapbase::figtype(fargepalett=AndelerGrVarData$fargepalett)
  farger <- figProps$UtFarger
  
  # to use extra data in tooltips, make a data series from data frame
  df <- data.frame(y = as.vector(AndelerGrVarData$AggVerdier$Hoved),
                   N = as.vector(AndelerGrVarData$Ngr$Hoved),
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_title(text = AndelerGrVarData$tittel) %>%
    hc_subtitle(text = AndelerGrVarData$utvalgTxt) %>%
    hc_xAxis(categories=names(AndelerGrVarData$Ngr$Hoved),
             # show every category
             #labels=list(step=1),
             reversed = FALSE) %>%
    hc_yAxis(title = list(text=AndelerGrVarData$xAkseTxt),
             min = -0.01,
             startOnTick = FALSE) %>%
    hc_add_series(name = "Andeler",
                  data = ds,
                  type = "bar",
                  color = farger[3],
                  tooltip = list(pointFormat='<b>Andel:</b>
                                 {point.y:.1f}<br><b>N:</b>
                                 {point.N}<br/>')) %>%
    hc_exporting(enabled = TRUE)
  
  # add global ratio, later then...
#   h1 <- hc_add_series(h1, name = paste0("Hele landet (", sprintf('%.1f',
#                                                                  AndelHele),
#                                         " %), N=", N),
#                       data = rep(AndelHele, length(GrNavnSort)),
#                       type = "line",
#                       color = farger[2],
#                       marker = list(enabled=FALSE),
#                       enableMouseTracking = FALSE
#   )
  
  ## table, data frame needed for download, widget for pres
  t1 <- data.frame(Enhet=names(AndelerGrVarData$Ngr$Hoved),
                   Andel=as.vector(AndelerGrVarData$AggVerdier$Hoved),
                   N = as.vector(AndelerGrVarData$Ngr$Hoved),
                   stringsAsFactors = FALSE)
  t1 <- t1[order(-t1$Andel), ]
  w1 <- DT::datatable(t1, options = list(dom='t', ordering=FALSE,
                                         paging = FALSE))
  tableObj = list(t1=t1, w1=w1)
  
  list(plotObj=h1, tableObj=tableObj)
}