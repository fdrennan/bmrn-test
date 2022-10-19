
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

      server_landing("landing")
      server_home("home")
      server_navbar("navbar")

      session_out <- test_session_setup_server("test_session_setup")
      shiny$observe({
        shiny$req(session_out())
        # browser()
      })

      setup_out <- analysis_a_setup_server("analysis_a_setup", session_out)

      shiny$observe({
        shiny$req(setup_out())
        browser()
      })

      test_1_output_data <- analysis_a_run_server(ns("test_1"), setup_out)

      shiny$observe({
        shiny$req(test_1_output_data())
      })
      #
      # observe({
      #   req(setup_out())
      #   showNotification(
      #     h6(class = "text-center p-2", "Setup complete, you may now review the other panels.")
      #   )
      # })
      #
      # test_1_output_data <- callModule(
      #   analysis_a_run_server, "test_1", user, is_admin, setup_out,
      #   cache = FALSE
      # )
      #
      # prism_input <- reactive({
      #   req(test_1_output_data())
      #   test_1_output_data()
      # })
      # server_input <- server_prism(test_1_output_data = prism_input)
      # server_analysis_a_report(server_input = server_input)
    }
  )
}
