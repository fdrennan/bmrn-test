

#' @export
ui <- function() {
  box::use(. / app)
  {
    box::use(
      shiny[addResourcePath, tags, div, fluidPage, column, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      . / offcanvas,
      . / button,
      esquisse,
      . / utilities / datatable
    )
    box::use(shiny[tags, actionButton, icon])
  }
  addResourcePath("loaders", "./www/images/loaders")

  fluidPage(
    {
      div(
        useShinyjs(),
        extendShinyjs(
          text = paste0(readLines("www/scripts/fullscreen.js"), collapse = "\n"), functions = "fullScreen"
        ),
        includeCSS("./www/styles.css"),
        includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
        includeScript("./www/scripts/enter.js"),
      )
    },
    style = "max-height: 100vh; overflow-y: auto;",
    app$app_ui(id = "app")
  )
}

#' @export
server <- function(input, output, session) {
  box::use(. / app)
  app$app_server(id = "app")
}

#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp])
  box::use(. / start[ui, server])
  runApp(
    shinyApp(ui, server)
  )
}
