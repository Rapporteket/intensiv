yearControlUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "position: relative; height: 0px;",

    shiny::div(
      style = "
        position: absolute;
        right: 15px;
        top: 6px;
      ",

      shiny::tags$small("Tilgjengelige data f.o.m.:"),

      shiny::div(
        style = "display: flex; gap: 4px; align-items: center;",

        shiny::selectInput(
          ns("since_year"),
          NULL,
          choices = rev(2014:as.integer(format(Sys.Date()-100, "%Y"))),
          selected = as.character(as.integer(format(Sys.Date(), "%Y")) - 1),
          width = "90px"
        ) |>
            shiny::tagAppendAttributes(style = "margin-bottom: 0;"),

        shiny::actionButton(
          ns("refresh"),
          NULL,
          icon = shiny::icon("rotate-right"),
          class = "btn btn-default btn-sm",
        )
      ),
      shiny::tags$small('Når du trykker på knappen,'),
      br(),
      shiny::tags$small('startes Rapporteket på nytt')
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
      shiny::updateQueryString(
        paste0("?since=", input$since_year, "-01-01"),
        mode = "replace",
        session = session
      )
      session$reload()
    })
  })
}
