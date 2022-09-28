#' @export
button_toolbar <- function(id = "button_toolbar") {
  box::use(shiny[div, icon, actionButton])
  box::use(. / button)
  div(
    class = "btn-toolbar d-flex justify-content-end",
    role = "toolbar",
    `aria-label` = "Top application toolbar",
    div(
      class = "btn-group me-2", role = "group", `aria-label` = "First group",
      button$button(label = icon("arrow-up"), class = "btn", id = "offcanvasScrolling", data_bs_toggle = "offcanvas"),
      actionButton("full", icon("expand"))
    )
  )
}
