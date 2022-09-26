
#' @export
ui <- function() {
  box::use(shiny, shinyjs, shinycssloaders)
  box::use(shiny[tags])
  box::use(./app)
  shiny$addResourcePath("loaders", "./www/images/loaders")
  shiny$fluidPage(
    shinyjs$useShinyjs(),
    shiny$includeCSS("./www/styles.css"),
    shiny$includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    shiny$fluidRow(
      class = "vh-100",
      shiny$div(
        id = "sidebar", class = "col-3 p-3",
        class = "bg-dark",
        shiny::tag("button", varArgs = list(
          class = 'btn btn-primary btn-block',
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = '#collapseExample',
          `aria-expanded` = "false",
          `aria-controls` = "collapseExample",
          tags$h4('Submit')
        )),
        shiny$div(class='collapse', tags$p(id = "collapseExample", "Ok, so here is some text", class='bg-light'))
      ),
      shiny$div(
        id = "body", class = "col-9",
        tags$h1("Hello", id = "hello", class='text-center text-dark')
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
  box::use(./app)
  shiny$runApp(
    shiny$shinyApp(app$ui, app$server)
  )
}
