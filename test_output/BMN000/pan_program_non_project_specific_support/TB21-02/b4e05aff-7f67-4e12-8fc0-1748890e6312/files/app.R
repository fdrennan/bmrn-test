library(test)
library(glue)
plan(multiprocess)

# devtools::load_all()
options(require_validation = FALSE)

if (TRUE) {
  options(shiny.port = 5000, shiny.host = "0.0.0.0")
  Sys.setenv(BASE_DOMAIN = "/qsci/test")
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
  headers(),
  dashboardPage(
    fullscreen = TRUE,
    header = dashboardHeader(
      div(
        HTML(
          '<button id="gohome" type="button" class="btn action-button m-1 px-2 py-1">
            <i class="bi bi-house-door-fill"></i>
           </button>'
        )
      )
    ),
    body = dashboardBody(router$ui),
    sidebar = dashboardSidebar(disable = T),
    footer = dashboardFooter(
      div(
        class = "d-flex justify-content-around",
        div(
          "Version 1.07"
        ),
        div(
          "© 2022 BioMarin"
        )
      )
    )
    # )
  )
)

server <- function(input, output, session,
                   user = "fr904103", is_admin = FALSE, cache = FALSE) {
  router$server(input, output, session)
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
  server_prism(test_1_output_data = prism_input)
  server_analysis_a_report()
}


# thematic::thematic_shiny()
runApp(shinyApp(ui = ui, server = server))
