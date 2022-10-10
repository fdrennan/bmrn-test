#' @export
ui <- function(router) {
  function() {{
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
    router$ui
  )  }
}

#' @export
server <- function(router) {
  function(input, output, session) {
    box::use(. / app)
    box::use(. / nfl)
    router$server(input, output, session)
    app$app_server(id = "app")
    nfl$server_pigskin_analytics("pigskin_analytics")
  }
}

#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp])
  box::use(. / main[ui, server])
  box::use(shiny.router[make_router, route, page404])
  box::use(. / app)
  box::use(. / nfl)
  router <- make_router(
    route("", app$app_ui(id = "app")),
    route("pigskin", nfl$ui_pigskin_analytics("pigskin_analytics")),
    page_404 = page404(message404 = "ABC")
  )
  shinyApp(ui(router), server(router))
}
