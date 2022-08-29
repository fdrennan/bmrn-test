#' analysis_a_setup
#' @export
analysis_a_setup <- function(id = "analysis_a_setup") {
  ns <- NS(id)
  uiOutput(ns("analysis_a_body"))
}

#' analysis_a_setup_server
#' @export
analysis_a_setup_server <- function(input, output, session, user, is_admin, signal) {
  ns <- session$ns

  input_data <- reactive({
    req(signal())
    input_data <- signal()$input_data
    con <- connect_table()
    data <- tbl(con, "sessions") %>%
      arrange(desc(timestamp)) %>%
      first() %>%
      collect()
    input_data$session_data <- data
    input_data
  })



  output$analysis_a_body <- renderUI({
    fluidRow(
      box(
        width = 4, collapsible = F,
        title = h4("Map Subject Type"),
        uiOutput(ns("typeAssignmentTable"))
      ),
      box(
        width = 4, collapsible = F,
        title = h4("Map Treatment Code"),
        uiOutput(ns("groupAssignmentTable"))
      ),
      box(
        width = 4, collapsible = F,
        uiOutput(ns("analysisInputUI"))
      ),
      div(
        class = "d-flex justify-content-end",
        actionButton(
          ns("runAnalysis"),
          h6("Run Analysis"),
          class = "btn-primary"
        )
      )
    )
  })

  output$typeAssignmentTable <- renderUI({
    data <- input_data()$data
    type_inputs <- distinct(data, Type, type_snake)
    make_type_assignment_table(type_inputs, ns)
  })



  output$groupAssignmentTable <- renderUI({
    data <- input_data()$data

    treatment_input <-
      distinct(data, treatment_snake, Treatment) %>%
      filter(complete.cases(.))


    map2(
      treatment_input$treatment_snake,
      treatment_input$Treatment,
      function(treatmentid, treatment) {
        selectizeInput(ns(treatmentid), treatment, choices = c(
          "Negative Control", "Positive Control", "Vehicle",
          "Treatment", "Other Comparator"
        ))
      }
    )
  })

  output$analysisInputUI <- renderUI({
    id <- input_data()
    data <- id$data
    req(data)
    nd <- names(data)

    base_col = which(colnames(data) == 'Baseline')
    type_snake_col = which(colnames(data) == 'type_snake')
    date_cols = colnames(data)[(base_col+1):(type_snake_col-1)]

    sessionMode <- id$session_data$sessionMode

    if (sessionMode == "Exploratory") {
      selected <- last(date_cols)
      choices <- last(date_cols)
    } else {
      selected <- NULL
      choices <- date_cols
    }
    div(
      selectInput(ns("timeSelectionInput"),
        label = h6("Select the time point for analysis:"),
        selected = selected,
        choices = choices
      ),
      {
        if (getOption("devmode")) {
          checkboxInput(
            inputId = ns("changeFromBaseline"),
            label = "Change from Baseline Analysis",
            value = FALSE
          )
        } else {
          prettyCheckbox(
            inputId = ns("changeFromBaseline"),
            label = "Change from Baseline Analysis",
            value = FALSE
          )
        }
      }
    )
  })


  out <- eventReactive(input$runAnalysis, {
    data <- update.list(signal(), reactiveValuesToList(input))
    st <- storr_rds("storr")
    st$set(data$session_data$uuid, data)
    data
  })

  out
}
