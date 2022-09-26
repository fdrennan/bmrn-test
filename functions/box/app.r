
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
  box::use(shiny, shiny[tags], shinyjs)
  box::use(./app)
  shiny$addResourcePath("loaders", "./www/images/loaders")
  shiny$fluidPage(
    shinyjs$useShinyjs(),
    shiny$includeCSS("./www/styles.css"),
    shiny$includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    shiny$fluidRow(
      class = "vh-100",
      shiny$div(
        id = "sidebar", class = "col-3",
        class = "bg-dark",
        app$collapser(".collapseExample", "Collapse"),
        shiny$actionButton("closeSidebar", "Close Sidebar")
      ),
      shiny$div(
        class = "collapseExample",
        id = "body", class = "col-9",
        tags$h1("Hello", id = "hello")
      )
    )
  )
}

#' @export
server <- function(input, output, session) {
  box::use(shiny, shinyjs)
}

#' @export
start <- function() {
  box::use(shiny)
  box::use(./app)
  shiny$runApp(
    shiny$shinyApp(app$ui, app$server)
  )
}
