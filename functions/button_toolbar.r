#' @export
button_toolbar <- function(id = "button_toolbar", ...) {
  box::use(shiny[div])
  div(
    class = "btn-toolbar d-flex justify-content-center",
    role = "toolbar",
    `aria-label` = "Top application toolbar",
    div(
      class = "btn-group me-2", role = "group", `aria-label` = "First group",
      ...
    )
  )
}
