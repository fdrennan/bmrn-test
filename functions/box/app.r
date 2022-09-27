#' @example
#' collapser(id ='asdf'),
#' shiny$div(
#'  id = "asdf", class = "collapse show",
#'  tags$div("Hello", class = "card card-body")
#' )
#' @export
collapser <- function(id = NULL,
                      data_bs_toggle = c("collapse", "offcanvas"),
                      class = "btn btn-primary", ...) {
  data_bs_toggle <- match.arg(data_bs_toggle)
  box::use(shiny[tags, tag])
  tag("button", varArgs = list(
    class = class,
    type = "button",
    `data-bs-toggle` = data_bs_toggle,
    `data-bs-target` = paste0("#", id),
    `aria-expanded` = "false",
    `aria-controls` = id,
    tags$h4("Submit"),
    ...
  ))
}

#' @export
offcanvas <- function() {
  box::use(shiny, shiny[tags])
  shiny$fluidRow(
    shiny$column(12, collapser(id = "offcanvasScrolling", data_bs_toggle = "offcanvas")),
    tags$div(
      class = "offcanvas offcanvas-start",
      `data-bs-scroll` = "true",
      `data-bs-backdrop` = "false",
      tabindex = "-1",
      id = "offcanvasScrolling",
      `aria-labelledby` = "offcanvasScrollingLabel",
      tags$div(
        class = "offcanvas-header",
        tags$h5(class = "offcanvas-title", id = "offcanvasScrollingLabel", "Colored with scrolling"),
        tags$button(class = "btn-close text-reset", `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")
      ),
      tags$div(class = "offcanvas-body", tags$p("Try scrolling the rest of the page to see this option in action."))
    )
  )
}

#' @export
ui <- function() {
  box::use(shiny, shinyjs, shinycssloaders)
  box::use(shiny[tags, HTML])
  box::use(. / app)
  shiny$addResourcePath("loaders", "./www/images/loaders")
  shiny$fluidPage(
    shinyjs$useShinyjs(),
    shiny$includeCSS("./www/styles.css"),
    shiny$includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    shiny$fluidRow(app$offcanvas()),
    shiny$fluidRow(
      # class = "vh-100",
      shiny$div(
        id = "body", class = "col-9 p-3",
        shiny$fluidRow(
          lapply(
            1:9,
            function(x) tags$div(tags$h3("Body", class = "display-3"), class = "col-3 card card-body")
          )
        )
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
