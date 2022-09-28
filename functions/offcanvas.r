
#' @export
offcanvas <- function(id,
                      header = "offcanvas header",
                      body = "offcanvas body",
                      location = c("start", "end", "top", "bottom"),
                      close_icon = "x") {
  box::use(shiny, shiny[tags])
  box::use(. / button)

  location <- match.arg(location)
  shiny$fluidRow(
    class = "p-1",
    tags$div(
      class = paste(
        paste("offcanvas", paste0(c("offcanvas", location), collapse = "-"))
      ),
      `data-bs-scroll` = "true",
      `data-bs-backdrop` = "false",
      tabindex = "-1",
      id = id,
      `aria-labelledby` = paste0(id, "Label"),
      tags$div(
        class = "offcanvas-header",
        tags$h5(class = "offcanvas-title", id = paste0(id, "Label"), header),
        button$button(
          id = id, open = FALSE,
          label = shiny::icon(close_icon, class = "text-light")
        )
      ),
      tags$div(class = "offcanvas-body", body)
    )
  )
}
