#' @export ui
ui <- function() {
  ui_app("app")
}

#' @export
server <- function(input, output, session) {
  server_app("app")
}

#' @export
ui_app <- function(id = "app") {
  box::use(shiny)
  ns <- shiny$NS(id)
  router <- router(ns)
  div(
    class = "bg-light",
    dashboardPage(
      fullscreen = FALSE, dark = FALSE,
      header = dashboardHeader(div(
        headers(),
        ui_navbar(ns("navbar"))
      )),
      body = dashboardBody(
        router$ui
      ),
      sidebar = dashboardSidebar(disable = T),
      footer = dashboardFooter(
        div(
          class = "d-flex justify-content-around",
          div(
            getOption("test_version")
          ),
          div(
            "© 2022 BioMarin"
          )
        )
      )
    )
  )
}

#' @export
server_app <- function(id = "app") {
  box::use(shiny)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      router <- router(ns)
      router$server(input, output, session)

      
      server_home("home")
      server_navbar("navbar")

      session_out <- test_session_setup_server("test_session_setup")

      test_1_output_data <- analysis_a_run_server("analysis_a_run", session_out, getOption("cachetest", FALSE))

      prism_input <- reactive({
        req(test_1_output_data())
        # debug(server_prism)
        test_1_output_data()
      })
      
      observeEvent(prism_input(), {
        server_input <- server_prism(test_1_output_data = prism_input())
        server_analysis_a_report(server_input = server_input)
      })
      
    }
  )
}
