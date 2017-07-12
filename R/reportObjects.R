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

readmission72hours <-  function(selectYear, selectQuarter, selectHospital,
                                selectHospitalType, selectErMann, selectAgeGroup) {
  
  
  fRegData <- NIRdata01reinn$NIRRegData01Off
  
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
    # get (static) data, lazy loaded
    d <- NIRAndelerGrVar(RegData = RegData, grVar = 'ShNavn', erMann = selectErMann,
                         valgtVar = 'reinn', hentData = 0, outfile = '',
                         lagFig = 0, offData = 1)
    
    ## hc
    # get actual color from name...
    figProps <- rapbase::figtype(fargepalett=d$fargepalett)
    farger <- figProps$farger
    
    # to use extra data in tooltips, make a data series from data frame
    df <- data.frame(y = as.vector(d$AggVerdier$Hoved),
                     N = as.vector(d$Ngr$Hoved),
                     stringsAsFactors = FALSE)
    ds <- rlist::list.parse(df)
    names(ds) <- NULL
    
    h1 <- highcharter::highchart() %>%
      hc_chart(height=800) %>%
      hc_title(text = d$tittel) %>%
      hc_subtitle(text = d$utvalgTxt) %>%
      hc_xAxis(categories=names(d$Ngr$Hoved),
      #hc_xAxis(categories=d$grtxt,
               # show every category
               labels=list(step=1),
               reversed = TRUE) %>%
      hc_yAxis(title = list(text=d$xAkseTxt),
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
    AggTot <- d$AggTot
    N <- d$N$Hoved
    obs <- length(d$Ngr$Hoved)
    h1 <- hc_add_series(h1,
                        name = paste0("Hele utvalget (",
                                      sprintf('%.1f', AggTot),
                                      " %), N=", N),
                        data = rep(AggTot, obs),
                        type = "line",
                        color = farger[2],
                        marker = list(enabled=FALSE),
                        enableMouseTracking = FALSE
    )
    
    # add target level
    tl <- d$KImaal
    h1 <- hc_add_series(h1,
                        name = paste0("Målnivå (", sprintf('%.1f', tl), " %)"),
                        data = rep(tl, obs),
                        type = "line",
                        color = "#FF7260",
                        marker = list(enabled=FALSE),
                        enableMouseTracking = FALSE
    )
    
    ## table, data frame needed for download, widget for pres
    t1 <- data.frame(Enhet=names(d$Ngr$Hoved),
                     Andel=as.vector(d$AggVerdier$Hoved),
                     N = as.vector(d$Ngr$Hoved),
                     row.names = NULL,
                     stringsAsFactors = FALSE)
    t1 <- t1[order(-t1$Andel), ]
    w1 <- DT::datatable(t1, rownames = FALSE,
                        options = list(dom='t', ordering=FALSE, paging=FALSE))
    tableObj = list(t1=t1, w1=w1)
    
    list(plotObj=h1, tableObj=tableObj)
  }
}


#' Provider of report objects
#' 
#' Provide report object GjsnGrVar
#' 
#' @export

gjsnGrVar <- function() {
  
  #get data
  data("GjsnGrVarData")
  
  # get actual color from name...
  figProps <- rapbase::figtype(fargepalett=GjsnGrVarData$fargepalett)
  farger <- figProps$farger
  
  # make data series
  df <- data.frame(y = GjsnGrVarData$AggVerdier$Hoved,
                   N = as.vector(GjsnGrVarData$Ngr$Hoved),
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- highcharter::highchart() %>%
    hc_chart(height=800) %>%
    hc_title(text = GjsnGrVarData$tittel) %>%
    hc_subtitle(text = GjsnGrVarData$utvalgTxt) %>%
    hc_xAxis(categories=names(GjsnGrVarData$Ngr$Hoved),
             # show every category
             labels=list(step=1),
             reversed = FALSE) %>%
    hc_yAxis(title = list(text=GjsnGrVarData$xAkseTxt),
             min = -0.01,
             startOnTick = FALSE,
             plotBands = list(from=GjsnGrVarData$AggVerdier$KIHele[1],
                              to=GjsnGrVarData$AggVerdier$KIHele[2],
                              color=farger[4])) %>%
    hc_add_series(name = GjsnGrVarData$xAkseTxt,
                  data = ds,
                  type = "bar",
                  color = farger[3],
                  tooltip = list(pointFormat='<b>{series.name}</b>:
                               {point.y:.1f}<br><b>N:</b>
                               {point.N} <br>')) %>%
    hc_tooltip(shared = TRUE)
  
  
  # add groups ci
  df <- data.frame(low = as.vector(GjsnGrVarData$AggVerdier$KIned),
                   high = as.vector(GjsnGrVarData$AggVerdier$KIopp),
                   stringsAsFactors = FALSE)
  ds <- rlist::list.parse(df)
  names(ds) <- NULL
  
  h1 <- hc_add_series(h1, name = "KI",
                      data = ds,
                      type = "errorbar",
                      color = farger[1],
                      tooltip = list(pointFormat='<b>KI:</b> {point.low:.1f} - {point.high:.1f} <br/>'))
  
  # add global score, ci as band defined i yAxis above
  obs <- length(GjsnGrVarData$Ngr$Hoved)
  h1 <- hc_add_series(h1,
                      name = paste0(GjsnGrVarData$tittel, ": ",
                                    sprintf('%.1f', GjsnGrVarData$medKI),
                                    ", N: ", GjsnGrVarData$N$Hoved, ", KI: ",
                                    sprintf('%.1f', GjsnGrVarData$AggVerdier$KIHele[1]),
                                    " - ",
                                    sprintf('%.1f', GjsnGrVarData$AggVerdier$KIHele[2])),
                      data = rep(GjsnGrVarData$medKI, obs),
                      type = "line",
                      color = farger[2],
                      marker = list(enabled=FALSE),
                      enableMouseTracking = FALSE)
  
  h1 <- hc_exporting(h1, enabled = TRUE)
  #htmlwidgets::saveWidget(h1, "~/tmp/FromRShiny.html", selfcontained = FALSE)
  
  # make table objects
  t1 <- data.frame(Enhet=names(GjsnGrVarData$Ngr$Hoved),
                   Verdi=as.vector(GjsnGrVarData$AggVerdier$Hoved),
                   N = as.vector(GjsnGrVarData$Ngr$Hoved),
                   KIned = as.vector(GjsnGrVarData$AggVerdier$KIned),
                   KIopp = as.vector(GjsnGrVarData$AggVerdier$KIopp),
                   row.names = NULL,
                   stringsAsFactors = FALSE)
  t1 <- t1[order(t1$Verdi), ]
  
  w1 <- DT::datatable(t1, rownames = FALSE,
                      options = list(dom='t', ordering=FALSE, paging=FALSE))
  
  tableObj <- list(t1=t1, w1=w1)
  
  list(plotObj=h1, tableObj=tableObj)
}