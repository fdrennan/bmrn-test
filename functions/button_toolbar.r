#' @export
button_toolbar <- function(ns, id = "button_toolbar", ..., class = "btn-toolbar d-flex justify-content-between") {
  box::use(shiny[div, icon, actionButton], . / button)
  div(
    class = class,
    role = "toolbar",
    `aria-label` = "Top application toolbar",
    div(
      class = "button-group me-2",
      actionButton(ns("plots"), icon("chart-simple", class = "fa-2x")),
      actionButton(ns("data"), icon("table-cells", class = "fa-2x")),
      actionButton(ns("display"), icon("display", class = "fa-2x"))
    ),
    div(
      class = "btn-group me-2", role = "group", `aria-label` = "First group",
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
