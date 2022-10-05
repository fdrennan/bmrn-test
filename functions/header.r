#' @export
header_ui <- function(id = "header") {
  box::use(shiny[tags, includeHTML, div])
  tags$header(
    class = "navbar navbar-dark sticky-top bg-dark flex-md-nowrap p-0 shadow",
    tags$a(class = "navbar-brand col-md-3 col-lg-2 me-0 px-3", href = "#", "ndexr"),
    HTML('<a href="https://gitlab.com/fdrennan/ndexr/-/commits/main"><img alt="pipeline status" src="https://gitlab.com/fdrennan/ndexr/badges/main/pipeline.svg" /></a>'),
    tags$button(
      class = "navbar-toggler position-absolute d-md-none collapsed",
      type = "button", `data-bs-toggle` = "collapse",
      `data-bs-target` = "#sidebarMenu", `aria-controls` = "sidebarMenu",
      `aria-expanded` = "false", `aria-label` = "Toggle navigation",
      tags$span(class = "navbar-toggler-icon")
    ),
    includeHTML("www/html/navbar.html"),
    div(class = "navbar-nav", div(
      class = "nav-item text-nowrap",
      tags$a(class = "nav-link px-3", href = "#", "Sign Out")
    ))
  )
}
