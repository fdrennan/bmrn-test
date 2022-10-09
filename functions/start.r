

#' @export
ui <- function() {
  box::use(. / app)
  {
    box::use(
      shiny[addResourcePath, HTML, tags, div, fluidPage, column, fluidRow, includeCSS, includeScript],
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

  tags$body(
    HTML('<meta charset="utf-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1">'),
    style = "max-height: 100vh; overflow-y: auto;",
    useShinyjs(),
    extendShinyjs(
      text = paste0(readLines("www/scripts/fullscreen.js"), collapse = "\n"), functions = "fullScreen"
    ),
    includeCSS("./www/styles.css"),
    includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    includeScript("./www/scripts/enter.js"),
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
  shinyApp(ui, server)
}
