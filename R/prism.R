#' ui_prism
#' @export
ui_prism <- function(id = "prism") {
  ns <- NS(id)
  fluidRow(
    testSpinner(uiOutput(ns("plotsInputs"))),
    testSpinner(div(class = "py-5", box(
      title = "",
      uiOutput(ns("plots")), maximizable = TRUE, collapsible = TRUE, width = 12,
      sidebar = boxSidebarTest(
        id = ns("boxSidebar"),
        startOpen = TRUE,
        background = "none",
        width = 10,
        easyClose = FALSE,
        div(
          class = "text-dark p-3", # style='width: 100px;',
          selectInput(ns("plotType"), "Plot Type", c("Bar", "Box"), "Bar"),
          numericInput(ns("fontSize"), value = 14, min = 5, max = 40, label = "Font Size"),
          numericInput(ns("plotWidth"), label = "Width", value = 1200, min = 0, max = 3000, step = 50),
          numericInput(ns("plotHeight"), label = "Height", value = 750, min = 0, max = 3000, step = 50),
        )
      )
    )))
  )
}
#' server_prism
#' @export
server_prism <- function(id = "prism", test_1_output_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      pre_prism_data <- reactive({
        # input$update
        # TODO add select y, change from baseline
        #
        data <- isolate(test_1_output_data())
        plot_data <- data$plot$data$transformed_data
        print(levels(plot_data$Treatment))
        #%>%
        #  filter(Treatment %in% input$treatmentPlotSelectors) %>%
        #  mutate(Treatment = droplevels(Treatment))
        list(
          plot_data = plot_data, tables = data$tables$tables,
          trt_sel = input$treatmentPlotSelectors, time_sel = input$timePlotSelectors,
          ylab = data$plot$endpoint,
          cfb = data$input_data$changeFromBaseline, endpoint = data$plot$endpoint,
          power = data$tables$power,
          num_groups = length(levels(plot_data$Treatment))
        )
      })


      output$plotsInputs <- renderUI({
        input_prism <- isolate(test_1_output_data())
        data <- input_prism$pre_modeling_input
        treatmentPlotSelectors <- levels(data$transformed_data$Treatment)
        timePlotSelectors <- levels(data$transformed_data$Time)
        toi <- input_prism$input_data$timeSelectionInput

        fluidRow(
          box(
            title = "Inputs",
            width = 12,
            div(
              class = "d-flex justify-content-around align-items-center",
              tooltip(
                selectizeInput(
                  inputId = ns("treatmentPlotSelectors"),
                  label = div(
                    class = "d-flex justify-content-between",
                    h4("Select Treatments to be Plotted"),
                    icon("info-circle")
                  ),
                  selected = treatmentPlotSelectors,
                  choices = treatmentPlotSelectors, multiple = TRUE
                ),
                "Use delete key to remove, mouse click to add."
              ),
              radioButtons(
                inputId = ns("timePlotSelectors"),
                label = h4("Select Time to be Plotted"),
                selected = toi,
                choices = timePlotSelectors,
                inline = TRUE
              ),
              radioButtons(ns("y_axisPrism"), h4("Select y axis"),
                choiceNames = list(
                  "Transform (suggested by Box-Cox)",
                  "No Transform (original scale)",
                  "Change from Baseline"
                ),
                choiceValues = list(
                  "transform", "no_transform", "change_from_baseline"
                )
              )
            ),
            div(
              class = "d-flex justify-content-end",
              actionButton(ns("update"), "Update", class = "btn btn-primary")
            )
          ),
          downloadButton(ns("download"), "Download Prism Data", class = "text-right")
        )
      })


      output$download <- downloadHandler(
        filename = function() {
          paste("prism-output-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          data <- test_1_output_data()
          tfd <- data$pre_modeling_input$transformed_data
          pow <- data$tables$power
          cfb <- data$input_data$changeFromBaseline
          full_path_file <- data$input_data$session_data$full_path_files
          full_path_file <- path_join(c(full_path_file, "prism_data.xlsx"))
          save_prism_output(full_path_file, tfd, pow, as.logical(cfb))
          save_prism_output(file, tfd, pow, as.logical(cfb))
        }
      )


      output$plots <-
        renderUI({
          req(input$plotType)
          plotHeight <- paste0(input$plotHeight, "px")
          plotWidth <- paste0(input$plotWidth, "px")
          if (input$plotType == "Box") {
            out <- plotOutput(ns("prismPlot_box"), width = input$plotWidth, height = plotHeight)
          } else if (input$plotType == "Bar") {
            out <- plotOutput(ns("prismPlot_bar"), width = input$plotWidth, height = plotHeight)
          }

          out
        })

      output$prismPlot_box <- renderPlot({
        isolate(input)
        req(pre_prism_data())
        input$update
        input <- reactiveValuesToList(input)
        input$border <- FALSE
        input$palette <- "black_and_white"
        data <- pre_prism_data()
        path <- path_join(c(test_1_output_data()$input_data$session_data$full_path_files, "prism_plots_box.jpg"))
        
        plot <- prism_plot(
          data = data$plot_data,
          tables = data$tables,
          trt_sel = input$treatmentPlotSelectors,
          time_sel = input$timePlotSelectors,
          endpoint = data$endpoint,
          format = "html",
          cfb = data$cfb,
          power = data$power,
          num_groups = data$num_groups,
          inputs = input,
          type = "box"
        )
        ggsave(filename = path, plot = plot, device = "jpg", width = 9, height = 6, units = "in")
        plot
      })

      output$prismPlot_bar <- renderPlot({
        isolate(input)
        req(pre_prism_data())
        data <- pre_prism_data()
        
        path <- path_join(c(test_1_output_data()$input_data$session_data$full_path_files, "prism_plots_bar.jpg"))
        
        print(path)
        plot <- prism_plot(
          data = data$plot_data,
          tables = data$tables,
          trt_sel = input$treatmentPlotSelectors,
          time_sel = input$timePlotSelectors,
          endpoint = data$endpoint,
          format = "html",
          cfb = data$cfb,
          power = data$power,
          num_groups = data$num_groups,
          inputs = reactiveValuesToList(input),
          type = "bar"
        )
        ggsave(filename = path, plot = plot, device = "jpg", width = 9, height = 6, units = "in")
        plot
      })



      observe({
        uuid <- test_1_output_data()$input_data$session_data$uuid
        req(uuid)
        input
        st <- storr_rds("storr")

        id <- paste0(uuid, "-prism")
        st$set(id, reactiveValuesToList(input))
        data
      })
      input
    }
  )
}
