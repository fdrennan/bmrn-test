library(test)

devtools::load_all()
plan(multiprocess)
options("test_version" = "Version 1.13")
if (isTRUE(getOption("production"))) {
  options(require_validation = TRUE)
  options(shiny.port = 5000, shiny.host = "0.0.0.0")
  Sys.setenv(BASE_DOMAIN = "/qsci/test")
  options(send = TRUE)
  options("devmode" = FALSE)
} else {
  options(require_validation = FALSE)
  options(send = FALSE)
  options("devmode" = FALSE)
}


router <- make_router(
  route("home", ui_home()),
  route(
    "analysisasetup",
    analysis_a_session_setup(user = "testuser", is_admin = TRUE)
  ),
  route("analysisa_run", analysis_a_run(id = "test_1")),
  route("report", ui_analysis_a_report()),
  page_404 = page404(message404 = "...hmmm")
)


ui <- div(
  class = "bg-light",
  dashboardPage(
    fullscreen = FALSE, dark = FALSE,
    header = dashboardHeader(
      div(
        headers(),
        ui_navbar(),
      )
    ),
    body = dashboardBody(router$ui),
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

server <- function(input, output, session,
                   user = "fr904103", is_admin = FALSE, cache = FALSE) {
  router$server(input, output, session)

  server_landing()
  server_home()
  server_navbar()
  server_template()
  observeEvent(input$gohome, {
    change_page("home")
  })

  callModule(administration_server, "administration", user, is_admin)

  session_out <- callModule(
    analysis_a_session_setup_server,
    "analysis_a_session_setup"
  )

  setup_out <- callModule(
    analysis_a_setup_server,
    "analysis_a_setup",
    user, is_admin, session_out
  )


  observe({
    req(setup_out())
    showNotification(
      h6(class = "text-center p-2", "Setup complete, you may now review the other panels.")
    )
  })

  test_1_output_data <- callModule(
    analysis_a_run_server, "test_1", user, is_admin, setup_out,
    cache = FALSE
  )


  prism_input <- reactive({
    req(test_1_output_data())
    test_1_output_data()
  })
  server_input <- server_prism(test_1_output_data = prism_input)
  server_analysis_a_report(server_input = server_input)
}


# thematic::thematic_shiny()
runApp(shinyApp(ui = ui, server = server))
