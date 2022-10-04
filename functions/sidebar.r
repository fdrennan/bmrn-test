#' @export
sidebar_ui <- function(id = "sidebar") {
  box::use(shiny[tags, div, includeHTML])
  tags$nav(
    id = "sidebarMenu", class = "col-md-3 col-lg-2 d-md-block bg-light sidebar collapse",
    div(
      class = "position-sticky pt-3",
      tags$h6("Reddit Search", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
      tags$ul(
        class = "nav flex-column",
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link active", `aria-current` = "page", href = "#",
            tags$span(`data-feather` = "home", "Subreddit")
          )
        ),
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link", href = "#",
            tags$span(`data-feather` = "home", "Author")
          )
        )
      )
    )
  )
}
