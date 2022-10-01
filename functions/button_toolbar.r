#' @export
button_toolbar <- function(ns, id = "button_toolbar", ..., class = "btn-toolbar d-flex justify-content-start bg-light") {
  box::use(shiny[div, icon, actionButton], . / button)

  div(
    class = class,
    role = "toolbar",
    `aria-label` = "Top application toolbar",
    div(
      class = "btn-group", role = "group", `aria-label` = "First group",
      button$button(
        label = icon("table", class = "fa-2x"), class = "btn",
        id = ns("console"), data_bs_toggle = "offcanvas"
      ),
      button$button(
        label = icon("cog", class = "fa-2x"), class = "btn",
        id = ns("settings"), data_bs_toggle = "offcanvas"
      ),
      actionButton(
        ns("full"),
        icon("expand", class = "fa-2x")
      )
    ),
    div()
  )
}
