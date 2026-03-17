yearControlUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "position: relative; height: 0px;",
    shiny::div(
      style = "
        position: absolute;
        right: 15px;
        top: 6px;
        display: flex;
        align-items: center;
        gap: 6px;
        background: white;
        padding: 2px 6px;
        border-radius: 4px;
        z-index: 1000;
      ",
      shiny::tags$small("Viser data siden:"),
      shiny::selectInput(
        ns("since_year"),
        NULL,
        choices = rev(1990:as.integer(format(Sys.Date(), "%Y"))),
        selected = paste0(as.numeric(format(Sys.Date()-90, "%Y"))),
        width = "80px"
      ),
      shiny::actionButton(
        ns("refresh"),
        label = NULL,
        icon = shiny::icon("rotate-right"),
        class = "btn btn-default btn-sm",
        style = "height: 30px; padding: 2px 6px;",
        title = "Oppdater"
      )
    )
  )
}

yearControlServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    observeEvent(shiny::getQueryString(session), once = TRUE, {
      qs <- shiny::getQueryString(session)
      year <- if (!is.null(qs$since)) substr(qs$since, 1, 4) 
        else paste0(as.numeric(format(Sys.Date()-90, "%Y")))

      shiny::updateSelectInput(session, "since_year", selected = year)
    })

    observeEvent(input$refresh, {
      print('boi')
      shiny::updateQueryString(
        paste0("?since=", input$since_year, "-01-01"),
        mode = "replace",
        session = session
      )
      session$reload()
    })
  })
}