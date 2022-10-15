#' make_test
#' @export
make_test <- function(ns,
                      test_name = "TEST 1",
                      details = c(
                        "Single Treatment with Multiple Doses",
                        "Multiple Time Points",
                        "Controls and Comparators"
                      )) {
  column(
    3,
    box(
      title = h4(test_name, class = "display-4"),
      collapsible = FALSE,
      width = 12,
      height = "310px",
      div(
        class = "h-100",
        tags$ul(
          class = "h-75",
          class = "px-3",
          map(details, ~ tags$li(h4(.)))
        ),
        div(
          class = "text-center",
          actionButton(
            inputId = ns("analysisaGo"), h6("Start", class = "display-6 m-0 p-0"),
            class = "btn btn-primary pb-2"
          )
        )
      )
    )
  )
}

#' ui_home
#' @export
ui_home <- function(id = "home") {
  ns <- NS(id)
  base <- Sys.getenv("BASE_DOMAIN")
  div(
    class = "m-5",
    uiOutput(ns("testLocations"))
  )
}

#' server_home
#' @export
server_home <- function(id = "home") {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        showModal(modalDialog(
          class = "h-100",
          ui_landing(),
          easyClose = TRUE,
          title = NULL,
          footer = div(
            class = "text-right",
            "Contact xxx-xxx-xxxx",
            tags$br(),
            getOption("test_version"),
          ),
          size = "xl"
        ))
      })

      output$testLocations <- renderUI({
        ns <- NS(id)
        fluidRow(
          fluidRow(
            class = "d-flex justify-content-around",
            make_test(ns),
            make_test(ns, test_name = "TEST 2", "Under Construction"),
            make_test(ns, test_name = "TEST 3", "Under Construction")
          )
        )
      })
      observeEvent(
        input$analysisaGo,
        {
          change_page("analysisasetup")
        }
      )
    }
  )
}
