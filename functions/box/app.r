
#' @export
collapser <- function(target = NULL, ..., class = "btn btn-primary") {
  box::use(shiny)
  shiny::tag("button", varArgs = list(
    class = class,
    type = "button",
    `data-bs-toggle` = "collapse",
    `data-bs-target` = target,
    `aria-expanded` = "false",
    `aria-controls` = "collapseExample", ...
  ))
}

#' @export
ui <- function() {
  box::use(shiny, shinyjs, shinycssloaders)
  shiny$addResourcePath("loaders", "./www/images/loaders")
  shiny$fluidPage(
    shinyjs$useShinyjs(),
    shiny$includeCSS("./www/styles.css"),
    shiny$includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    shiny$uiOutput('app', container = function(...) {
      shiny$fluidRow(class = "vh-100", shiny$column(
        12, ...
      ))
    })
  )
}

#' @export
server <- function(input, output, session) {
  box::use(shiny, shinyjs, shiny[tags], )
  box::use(./app)
  output$app <- shiny$renderUI({
    shiny$fluidRow(
      class = "vh-100",
      shiny$div(
        id = "sidebar", class = "col-3",
        class = "bg-dark",
        app$collapser(target = ".collapseExample", "Collapse"),
        shiny$actionButton("closeSidebar", "Close Sidebar")
      ),
      shiny$div(
        class = "collapseExample",
        id = "body", class = "col-9",
        tags$h1("Hello", id = "hello")
      )
    )
  })
}

#' @export
start <- function() {
  box::use(shiny)
  box::use(./app)
  # browser()
  debug(app$ui)
  debug(app$server)
  shiny$runApp(
    shiny$shinyApp(app$ui, app$server)
  )
}
