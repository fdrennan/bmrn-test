



#' @export
ui <- function() {
  {
    box::use(
      shiny[addResourcePath, tags, fluidPage, column, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      ./offcanvas,
      ./button,
      ./button_toolbar[button_toolbar]
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
      fluidRow(button_toolbar(id='button_toolbar',
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
      reddit$ui_subreddit(),
      fluidRow(
        offcanvas$offcanvas(
          id = "console",
          location = "bottom",
          header = tags$h1("Console"),
          body = tags$h1("Development Information"),
          close_icon = 'arrow-down'
        ),
        offcanvas$offcanvas(
          id = "settings",
          location = "end",
          header = tags$h1("Settings and Options"),
          body = tags$h1("More here"),
          close_icon = 'arrow-right'
        )
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  box::use(shiny[observeEvent], shinyjs[js])
  box::use(
    . / reddit
  )
  observeEvent(input$full, {
    js$fullScreen("homepage")
  })
  reddit$server_subreddit()
}

#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp])
  box::use(. / app[ui, server])
  runApp(
    shinyApp(ui, server)
  )
}
