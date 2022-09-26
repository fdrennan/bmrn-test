
#' @export
collapser <- function(target = NULL, ..., class = "btn btn-primary") {
  box::use(shiny)
  shiny::tag("button", varArgs = list(
    class = class,
    type = "button",
    `data-bs-toggle` = "collapse",
    `data-bs-target` = target,
    `aria-expanded` = "true",
    `aria-controls` = "collapseExample", ...
  ))
}

#' @export
ui <- function() {
  box::use(shiny, shinyjs, shinycssloaders)
  box::use(shiny[tags])
  box::use(. / app)
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
        app$collapser(
          target = "#collapseExample",
          "Collapse",
          type = "button",
          class = "btn btn-primary btn-block text-light",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = target,
          `aria-expanded` = "true",
          `aria-controls` = "collapseExample"
        ),
        tags$p(id = "collapseExample", "Ok, so here is some text", class = "bg-light")
      ),
      shiny$div(
        id = "body", class = "col-9",
        tags$h1("Hello", id = "hello")
      )
    )
  )
}

#' @export
server <- function(input, output, session) {


}

#' @export
start <- function() {
  box::use(shiny)
  box::use(. / app)
  shiny$runApp(
    shiny$shinyApp(app$ui, app$server)
  )
}
