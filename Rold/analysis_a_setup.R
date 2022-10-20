#' analysis_a_setup
#' @export
analysis_a_setup <- function(id) {
  ns <- NS(id)
  uiOutput(ns("analysis_a_body"))
}

#' analysis_a_setup_server
#' @export
analysis_a_setup_server <- function(id, input_data) {
  box::use(shiny)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$analysis_a_body <- renderUI({
        shiny$req(input_data())
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
            actionButton(ns("runAnalysis"), h6("Run Analysis"), class = "btn-primary")
          )
        )
      })

      output$typeAssignmentTable <- renderUI({
        shiny$req(input_data())
        
        data <- input_data()$input_data$data
        type_inputs <- distinct(data, Type, type_snake)
        make_type_assignment_table(type_inputs, ns)
      })



      output$groupAssignmentTable <- renderUI({
        shiny$req(input_data())
        data <- input_data()$input_data$data

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
        shiny$req(input_data())
        session_data <- input_data()$session_data
        data <- input_data()$input_data$data
 
        nd <- names(data)

        date_cols <- str_detect(names(data), "[0-9]")
        date_cols <- names(data)[date_cols]
        date_cols <- date_cols[order(as.numeric(gsub("[A-z]| ", "", date_cols)))]

        sessionMode <- session_data$sessionMode[[1]]

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
        shiny$showNotification("input run analysis pressed")
        # 
        data <- update.list(input_data(), list(selections = reactiveValuesToList(input)))
        st <- storr_rds("storr")
        st$set(unique(data$session_data$uuid), data)

        data
      })

      out
    }
  )
}
