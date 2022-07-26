library(test)

devtools::load_all()
plan(multiprocess)

options("test_version" = "Version 1.21")
if (isTRUE(getOption("production"))) {
  options(shiny.port = 5000, shiny.host = "0.0.0.0")
  options(require_validation = TRUE)
  options(send = TRUE)
  options("devmode" = FALSE)
} else {
  options(require_validation = FALSE)
  options(send = FALSE)
  options("devmode" = TRUE)
}

router <- make_router(
  route("home", ui_home()),
  route("backend", ui_backend()),
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
        ui_navbar()
      )
    ),
    body = dashboardBody(router$ui),
    sidebar = dashboardSidebar(disable = T),
    footer = dashboardFooter(
      div(
        class = "d-flex justify-content-around",
        {
          if (getOption("devmode")) {
            actionButton(
              inputId = "backend",
              label = "Backend"
            )
          } else {
            NULL
          }
        },
        actionButton("report", "Contact Us"),
        div(class = "", getOption("test_version")),
        {
          if (getOption("devmode")) {
            actionButton(
              inputId = "analysisasetup",
              label = "Analysis A Setup"
            )
          } else {
            NULL
          }
        }
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
  server_backend()

  observeEvent(input$gohome, {
    change_page("home")
  })

  observeEvent(input$backend, {
    change_page("backend")
  })

  observeEvent(input$report, {
    showModal(
      ui = modalDialog(
        div(
          textAreaInput("emailMessage", label = "Message", width = "100%"),
          actionButton("sendEmail", "Send Message")
        )
      )
    )
  })

  observeEvent(input$sendEmail, {
    tryCatch(
      {
        send_email(
          all_files = FALSE,
          files = NULL,
          email_message = input$emailMessage,
          from = "fr904103@bmrn.com", to = "fr904103@bmrn.com"
        )
        showNotification("Email received!")
      },
      error = function(err) {
        showNotification("Oops, the email failed to send. Please contact fr904103@bmrn.com")
      }
    )
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
