#' @export
ui_navbar <- function() {
  box::use(. / navbar[nav, form, button], shiny[tags, div, HTML])

  nav(
    a("Navbar w/ text", href = "#"),
    button(tags$span(class = "navbar-toggler-icon"), type = "toggler")
  )
}

#' @export
nav <- function(...) {
  box::use(shiny[tag])
  tag("nav", varArgs = list(..., class = "navbar navbar-light"))
}

#' @export
form <- function(...) {
  box::use(shiny[tag])
  tag("form", varArgs = list(..., class = "form-inline"))
}

#' @export
button <- function(..., type = "secondary") {
  box::use(shiny[tag])
  toggler <- list(..., class = "navbar-toggler", type = "button", `data-toggle` = "collapse", `data-target` = "#navbarSupportedContent", `aria-controls` = "navbarSupportedContent", `aria-expanded` = "false", `aria-label` = "Toggle navigation")
  secondary <- list(..., class = "btn btn-outline-secondary", type = "button")
  varArgs <- switch(type,
    toggler = toggler,
    secondary = secondary
  )
  tag("button", varArgs = varArgs)
}

#' @export
a <- function(...) {
  box::use(shiny[tag])
  tag("a", varArgs = list(..., class = "navbar-brand"))
}
