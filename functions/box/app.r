#' @export
run <- function() {
  box::use(shiny, shiny[tags], shinyjs)
  shiny$addResourcePath("loaders", "./www/images/loaders")
  
  collapser <- function(...) {
    shiny::tag("button", varArgs = list(
      class="btn btn-primary",
      type="button",
      `data-bs-toggle`="collapse",
      `data-bs-target`=".collapseExample",
      `aria-expanded`="false",
      `aria-controls`="collapseExample",
      ...
    ))
  }
  ui <- function() {
    shiny$fluidPage(
      shinyjs$useShinyjs(),
      shiny$includeCSS("./www/styles.css"),
      shiny$includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
      shiny$fluidRow(
        class = "vh-100",
        shiny$div(
          id = "sidebar", class = "col-3",
          class = "bg-dark",
          
    
          shiny$HTML(
            '<button >
                     Button with data-bs-target
            </button>'
          ),
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
  server <- function(input, output, session) {
    box::use(shiny, shinyjs)
  }
  shiny$runApp(
    shiny$shinyApp(ui, server)
  )
}
