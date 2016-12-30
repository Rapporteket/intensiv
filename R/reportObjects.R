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
    hc_chart(height=800) %>%
    hc_title(text = AndelerGrVarData$tittel) %>%
    hc_subtitle(text = AndelerGrVarData$utvalgTxt) %>%
    hc_xAxis(categories=names(AndelerGrVarData$Ngr$Hoved),
             # show every category
             labels=list(step=1),
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
  
  # add global ratio
  AggTot <- AndelerGrVarData$AggTot
  N <- AndelerGrVarData$N$Hoved
  obs <- length(AndelerGrVarData$Ngr$Hoved)
  h1 <- hc_add_series(h1,
                      name = paste0("Hele landet (",
                                    sprintf('%.1f', AggTot),
                                        " %), N=", N),
                      data = rep(AggTot, obs),
                      type = "line",
                      color = farger[2],
                      marker = list(enabled=FALSE),
                      enableMouseTracking = FALSE
  )
  
  ## table, data frame needed for download, widget for pres
  t1 <- data.frame(Enhet=names(AndelerGrVarData$Ngr$Hoved),
                   Andel=as.vector(AndelerGrVarData$AggVerdier$Hoved),
                   N = as.vector(AndelerGrVarData$Ngr$Hoved),
                   row.names = NULL,
                   stringsAsFactors = FALSE)
  t1 <- t1[order(-t1$Andel), ]
  w1 <- DT::datatable(t1, rownames = FALSE,
                      options = list(dom='t', ordering=FALSE, paging=FALSE))
  tableObj = list(t1=t1, w1=w1)
  
  list(plotObj=h1, tableObj=tableObj)
}


GjsnGrVar <- function() {
  
  #get data
  data("GjsnGrVarData")
  
  # make data series
  df <- data.frame(y = Midt,
                   N = Ngrtxt,
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_title(text = paste(tittel, "med 95% konfidensintervall")) %>%
    hc_subtitle(text = utvalgTxt) %>%
    hc_xAxis(categories = as.character(GrNavnSort),
             reversed = FALSE) %>%
    hc_yAxis(title = list(text=xaksetxt),
             min = -0.01,
             startOnTick = FALSE,
             plotBands = list(from=KIHele[1],
                              to=KIHele[2],
                              color=farger[4])) %>%
    hc_add_series(name = deltittel,
                  data = ds,
                  type = "bar",
                  color = farger[3],
                  tooltip = list(pointFormat='<b>{series.name}</b>:
                               {point.y:.1f}<br><b>N:</b>
                               {point.N} <br>')) %>%
    hc_tooltip(shared = TRUE)
  
  
  # add groups ci
  df <- data.frame(low = KIned,
                   high = KIopp,
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- hc_add_series(h1, name = "KI",
                      data = ds,
                      type = "errorbar",
                      color = farger[1],
                      tooltip = list(pointFormat='<b>KI:</b> {point.low:.1f} - {point.high:.1f} <br/>'))
  
  # add global score, ci as band defined i yAxis above
  h1 <- hc_add_series(h1, name = paste0(tittel, ", alle: ",
                                        sprintf('%.1f',MidtHele),
                                        ", N: ", N, ", KI: ",
                                        sprintf('%.1f', KIHele[1]), " - ",
                                        sprintf('%.1f', KIHele[2])),
                      data = rep(MidtHele, length(GrNavnSort)),
                      type = "line",
                      color = farger[2],
                      marker = list(enabled=FALSE),
                      enableMouseTracking = FALSE)
  
  h1 <- hc_exporting(h1, enabled = TRUE)
  #htmlwidgets::saveWidget(h1, "~/tmp/FromRShiny.html", selfcontained = FALSE)
  
  return(h1)
}