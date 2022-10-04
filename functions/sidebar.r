#' @export
sidebar_ui <- function(id = "sidebar") {
  box::use(shiny[tags, div, includeHTML, NS])
  ns <- NS(id)
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
            id = "goToSubreddit",
            class = "nav-link active action-button", `aria-current` = "page",
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
    ),
    div(
      class = "position-sticky pt-3",
      tags$h6("Administration", class = "sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted"),
      tags$ul(
        class = "nav flex-column",
        tags$li(
          class = "nav-item",
          tags$a(
            class = "nav-link", href = "#",
            tags$span(`data-feather` = "home", "Connections")
          )
        )
      )
    )
  )
}

#' @export
sidebar_server <- function(id = "sidebar") {
  box::use(shiny[moduleServer, reactive])
  moduleServer(
    id = id,
    function(input, output, session) {
      out <- reactive({
        print("In Sidebar server")
        print(input)
        TRUE
      })
      out
    }
  )
}
