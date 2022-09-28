



#' @export
ui <- function() {
  {
    box::use(
      shiny[addResourcePath, tags, div, fluidPage, column, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      . / offcanvas,
      . / button,
      . / button_toolbar[button_toolbar],
      esquisse,
      . / utilities / datatable
    )
    box::use(shiny[tags, actionButton, icon])
  }
  addResourcePath("loaders", "./www/images/loaders")
  fluidPage(
    id = "homepage",
    useShinyjs(),
    extendShinyjs(
      text = paste0(readLines("www/scripts/fullscreen.js"), collapse = "\n"), functions = "fullScreen"
    ),
    includeCSS("./www/styles.css"),
    includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    column(
      12,
      fluidRow(button_toolbar(
        id = "button_toolbar",
        button$button(
          label = icon("table"), class = "btn",
          id = "console", data_bs_toggle = "offcanvas"
        ),
        button$button(
          label = icon("cog"), class = "btn",
          id = "settings", data_bs_toggle = "offcanvas"
        ),
        actionButton("full", icon("expand"))
      )),
      esquisse$esquisse_ui("esquisse"),
      datatable$ui_dt("submissionsTable"),
      div(
        offcanvas$offcanvas(
          id = "console",
          location = "bottom",
          header = NULL,
          body = reddit$ui_subreddit(),
          close_icon = "arrow-down"
        ),
        offcanvas$offcanvas(
          id = "settings",
          location = "end",
          header = tags$h1("Settings and Options"),
          body = tags$h1("More here"),
          close_icon = "arrow-right"
        )
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  box::use(shiny[observeEvent, reactive, reactiveValues], shinyjs[js])
  box::use(
    . / utilities / datatable,
    esquisse,
    . / reddit
  )
  observeEvent(input$full, {
    js$fullScreen("homepage")
  })

  subreddit_data <- reddit$server_subreddit()

  observeEvent(subreddit_data(), {
    esquisse$esquisse_server("esquisse", data_rv = reactiveValues(data = subreddit_data(), name = "subdata"))
  })

  observeEvent(subreddit_data(), {
    datatable$server_dt("submissionsTable", subreddit_data())
  })
}

#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp])
  box::use(. / app[ui, server])
  runApp(
    shinyApp(ui, server)
  )
}
