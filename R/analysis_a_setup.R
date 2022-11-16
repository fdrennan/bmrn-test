#' analysis_a_setup
#' @export
analysis_a_setup <- function(id) {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("analysis_a_body"))
}

#' analysis_a_setup_server
#' @export
analysis_a_setup_server <- function(id, input_data) {
  box::use(shiny)
  box::use(bs4Dash)
  box::use(purrr)
  box::use(dplyr)
  box::use(stats[complete.cases])
  box::use(stringr)
  box::use(shinyWidgets)
  box::use(gdata)
  box::use(storr)
  box::use(. / make_type_assignment_table)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$analysis_a_body <- shiny$renderUI({
        shiny$req(input_data())
        shiny$fluidRow(
          bs4Dash$box(
            width = 4, collapsible = F,
            title = shiny$h4("Map Subject Type"),
            shiny$uiOutput(ns("typeAssignmentTable"))
          ),
          bs4Dash$box(
            width = 4, collapsible = F,
            title = shiny$h4("Map Treatment Code"),
            shiny$uiOutput(ns("groupAssignmentTable"))
          ),
          bs4Dash$box(
            width = 4, collapsible = F,
            shiny$uiOutput(ns("analysisInputUI"))
          ),
          shiny$div(
            class = "d-flex justify-content-end",
            shiny$actionButton(ns("runAnalysis"), shiny$h6("Run Analysis"), class = "btn-primary")
          )
        )
      })

      output$typeAssignmentTable <- shiny$renderUI({
        shiny$req(input_data())

        data <- input_data()$input_data$data
        type_inputs <- dplyr$distinct(data, Type, type_snake)
        make_type_assignment_table$make_type_assignment_table(type_inputs, ns)
      })



      output$groupAssignmentTable <- shiny$renderUI({
        shiny$req(input_data())
        data <- input_data()$input_data$data

        treatment_input <- dplyr$distinct(data, treatment_snake, Treatment)
        treatment_input <- dplyr$filter(treatment_input, complete.cases(treatment_input))


        purrr$map2(
          treatment_input$treatment_snake,
          treatment_input$Treatment,
          function(treatmentid, treatment) {
            shiny$selectizeInput(ns(treatmentid), treatment, choices = c(
              "Negative Control", "Positive Control", "Vehicle",
              "Treatment", "Other Comparator"
            ))
          }
        )
      })

      output$analysisInputUI <- shiny$renderUI({
        shiny$req(input_data())
        session_data <- input_data()$session_data
        data <- input_data()$input_data$data

        nd <- names(data)

        date_cols <- stringr$str_detect(names(data), "[0-9]")
        date_cols <- names(data)[date_cols]
        date_cols <- date_cols[order(as.numeric(gsub("[A-z]| ", "", date_cols)))]

        sessionMode <- session_data$sessionMode

        if (sessionMode == "Exploratory") {
          selected <- dplyr$last(date_cols)
          choices <- dplyr$last(date_cols)
        } else {
          selected <- NULL
          choices <- date_cols
        }
        shiny$div(
          shiny$selectInput(ns("timeSelectionInput"),
            label = shiny$h6("Select the time point for analysis:"),
            selected = selected,
            choices = choices
          ),
          {
            if (getOption("devmode")) {
              shiny$checkboxInput(
                inputId = ns("changeFromBaseline"),
                label = "Change from Baseline Analysis",
                value = FALSE
              )
            } else {
              shinyWidgets$prettyCheckbox(
                inputId = ns("changeFromBaseline"),
                label = "Change from Baseline Analysis",
                value = FALSE
              )
            }
          }
        )
      })

      out <- shiny$eventReactive(input$runAnalysis, {
        shiny$showNotification("input run analysis pressed")
        data <- gdata$update.list(input_data(), list(selections = shiny$reactiveValuesToList(input)))
        st <- storr$storr_rds("storr")
        st$set(unique(data$session_data$uuid), data)

        data
      })

      out
    }
  )
}
