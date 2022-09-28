#' @description Button for interacting with bootstrap 5.
#' https://getbootstrap.com/docs/5.0/components/collapse/
#' @export
button <- function(id = NULL,
                   data_bs_toggle = c("collapse", "offcanvas"),
                   class = "btn", label = shiny::icon("arrow-up"), open = FALSE) {
  data_bs_toggle <- match.arg(data_bs_toggle)
  box::use(shiny[tags, tag])
  if (open) {
    params <- list(
      class = class,
      type = "button",
      `data-bs-target` = paste0("#", id),
      `aria-expanded` = "false",
      `aria-controls` = id,
      `data-bs-dismiss` = data_bs_toggle,
      label
    )
  } else {
    # class = paste(c(class, "btn-close"))
    params <- list(
      class = class,
      type = "button",
      `data-bs-target` = paste0("#", id),
      # `aria-expanded` = "false",
      `aria-controls` = id,
      `data-bs-toggle` = data_bs_toggle,
      label
    )
  }
  do.call("tag",
          args = list("button",
                      varArgs = params
          )
  )
}
