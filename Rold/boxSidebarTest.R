#' boxSidebarTest
#' @export
boxSidebarTest <- function(..., id = NULL, width = 50, background = "#333a40",
                           startOpen = FALSE, icon = shiny::icon("cogs"), easyClose = TRUE) {
  # stopifnot(width >= 25 && width <= 100)
  toolbarTag <- shiny::tags$button(
    id = id, `data-background` = background,
    `data-width` = width, `data-widget` = "chat-pane-toggle",
    `data-toggle` = "tooltip", `data-original-title` = "More",
    `data-start-open` = tolower(startOpen), `data-easy-close` = tolower(easyClose),
    type = "button", icon
  )
  contentTag <- shiny::tags$div(
    style = "z-index: 1; height: inherit;",
    class = "direct-chat-contacts", shiny::tags$ul(
      class = "contacts-list",
      shiny::tags$li(...)
    )
  )
  shiny::tagList(toolbarTag, contentTag)
}
