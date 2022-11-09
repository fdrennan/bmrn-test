library(test)
library(shinyjs)
library(shiny.router)
library(shinyvalidate)
library(storr)
library(httr)
library(furrr)


devtools::load_all()

if (FALSE) {
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


#' ui_admin
#' @export
ui_admin <- function(id = "admin") {
  ns <- NS(id)
  # tableOutput(ns('table'))
}

#' server_admin
#' @export
server_admin <- function(id = "admin") {
  moduleServer(
    id,
    function(input, output, session) {
      output$table <- renderTable({
        con <- connect_table()
        tbl(con, "sessions") %>% collect()
      })
    }
  )
}

ui <-
  function(incoming, user = "testuser", is_admin = TRUE) {
    fluidPage(
      headers(),
      ui_navbar(),
      fluidRow(router$ui)
    )
  }



server <- function(input, output, session,
                   user = "fr904103", is_admin = FALSE, cache = FALSE) {
  router$server(input, output, session)
  server_home()
  server_navbar()
  server_template()

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
    showNotification(div(
      h6(class = "text-center", "Setup complete, you may now review the other panels.")
    ))
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
