#' ui_prism
#' @export
ui_prism <- function(id = "prism") {
  box::use(shiny, bs4Dash, . / testSpinner, . / boxSidebarTest)
  ns <- shiny$NS(id)
  shiny$fluidRow(
    testSpinner$testSpinner(shiny$uiOutput(ns("plotsInputs"))),
    testSpinner$testSpinner(shiny$div(class = "py-5", bs4Dash$box(
      title = "",
      shiny$uiOutput(ns("plots")), maximizable = TRUE, collapsible = TRUE, width = 12,
      sidebar = boxSidebarTest$boxSidebarTest(
        id = ns("boxSidebar"),
        startOpen = TRUE,
        background = "none",
        width = 10,
        easyClose = FALSE,
        shiny$div(
          class = "text-dark p-3", # style='width: 100px;',
          shiny$selectInput(ns("plotType"), "Plot Type", c("Bar", "Box"), "Bar"),
          shiny$numericInput(ns("fontSize"), value = 14, min = 5, max = 40, label = "Font Size"),
          shiny$numericInput(ns("plotWidth"), label = "Width", value = 1200, min = 0, max = 3000, step = 50),
          shiny$numericInput(ns("plotHeight"), label = "Height", value = 750, min = 0, max = 3000, step = 50),
          shiny$numericInput(ns("bottom_percent"), label = "Size Percentage of Data Plot", value = 70, min = 0, max = 100, step = 5),
          shiny$selectInput(
            ns("palette"), "Color Palette",
            sort(c("floral", "colorblind_safe", "prism_light", "black_and_white")),
            "floral"
          )
        )
      )
    )))
  )
}
#' server_prism
#' @export
server_prism <- function(id = "prism", signal) {
  box::use(shiny)
  box::use(fs)
  box::use(. / prism_output)
  box::use(ggplot2)
  box::use(storr)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny$observe({
        signal()
      })

      pre_prism_data <- shiny$reactive({
        shiny$req(signal())
        data <- signal()
        plot_data <- data$plot$data$transformed_data
        input <- data$inputs
        list(
          plot_data = plot_data, tables = data$tables$tables,
          trt_sel = input$treatmentPlotSelectors, time_sel = input$timePlotSelectors,
          ylab = data$plot$endpoint,
          cfb = data$input_data$selections$changeFromBaseline,
          endpoint = data$plot$endpoint,
          power = data$tables$power,
          num_groups = length(levels(plot_data$Treatment))
        )
      })


      output$plotsInputs <- shiny$renderUI({
        input_prism <- shiny$isolate(signal())
        data <- input_prism$pre_modeling_input
        treatmentPlotSelectors <- levels(data$transformed_data$Treatment)
        timePlotSelectors <- levels(data$transformed_data$Time)
        toi <- input_prism$input_data$timeSelectionInput
        shiny$fluidRow(
          bs4Dash$box(
            title = "Inputs",
            width = 12,
            shiny$div(
              class = "d-flex justify-content-around align-items-center",
              bs4Dash$tooltip(
                shiny$selectizeInput(
                  inputId = ns("treatmentPlotSelectors"),
                  label = shiny$div(
                    class = "d-flex justify-content-between",
                    shiny$h4("Select Treatments to be Plotted"),
                    shiny$icon("info-circle")
                  ),
                  selected = treatmentPlotSelectors,
                  choices = treatmentPlotSelectors, multiple = TRUE
                ),
                "Use delete key to remove, mouse click to add."
              ),
              shiny$radioButtons(
                inputId = ns("timePlotSelectors"),
                label = h4("Select Time to be Plotted"),
                selected = toi,
                choices = timePlotSelectors,
                inline = TRUE
              ),
              shiny$radioButtons(ns("y_axisPrism"), shiny$h4("Select y axis"),
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
            shiny$div(
              class = "d-flex justify-content-end",
              shiny$actionButton(ns("update"), "Update", class = "btn btn-primary")
            )
          ),
          shiny$downloadButton(ns("download"), "Download Prism Data", class = "text-right")
        )
      })
      prismData <- shiny$reactive({
        shiny$req(signal())
        shiny$req(input$treatmentPlotSelectors)
        #
        data <- signal()
        tfd <- data$pre_modeling_input$transformed_data
        pow <- data$tables$power
        cfb <- data$input_data$selections$changeFromBaseline %>% as.logical()
        full_path_file <- data$input_data$session_data$full_path_files
        full_path_file <- fs$path_join(c(full_path_file, "prism_data.xlsx"))

        prism_output$save_prism_output(full_path_file, tfd, pow, as.logical(cfb))
        # showNotification("Storing prism data")
        list(full_path_file = full_path_file, tfd = tfd, pow = pow, cfb = cfb)
      })

      shiny$observe({
        shiny$showNotification("fix prism output")
      })
      # output$download <- downloadHandler(
      #   filename = function() {
      #     paste("prism_data.xlsx", sep = "")
      #   },
      #   content = function(file) {
      #     req(prismData())
      #     save_prism_output(file, prismData()$tfd, prismData()$pow, prismData()$cfb)
      #   }
      # )
      output$plots <-
        shiny$renderUI({
          shiny$req(input$plotType)

          plotHeight <- paste0(input$plotHeight, "px")
          plotWidth <- paste0(input$plotWidth, "px")
          if (input$plotType == "Box") {
            out <- shiny$plotOutput(ns("prismPlot_box"), width = input$plotWidth, height = plotHeight)
          } else if (input$plotType == "Bar") {
            out <- shiny$plotOutput(ns("prismPlot_bar"), width = input$plotWidth, height = plotHeight)
          }

          out
        })

      output$prismPlot_box <- shiny$renderPlot({
        shiny$isolate(input)
        shiny$req(pre_prism_data())

        input$update
        input <- shiny$reactiveValuesToList(input)
        input$border <- FALSE
        data <- pre_prism_data()
        path <- fs$path_join(c(signal()$input_data$session_data$full_path_files, "prism_plots_box.jpg"))
        if (length(input$timePlotSelectors) > 0 && length(input$treatmentPlotSelectors) > 0) {
          plot <- prism_plot$prism_plot(
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
          ggplot2$ggsave(filename = path, plot = plot, device = "jpg", width = 14, height = 10, units = "in", dpi = 300)
          plot
        }
      })

      output$prismPlot_bar <- shiny$renderPlot({
        isolate(input)
        req(pre_prism_data())
        #
        data <- pre_prism_data()

        path <- fs$path_join(c(signal()$input_data$session_data$full_path_files, "prism_plots_bar.jpg"))

        if (length(input$timePlotSelectors) > 0 && length(input$treatmentPlotSelectors) > 0) {
          plot <- prism_plot$prism_plot(
            data = data$plot_data,
            tables = data$tables,
            trt_sel = input$treatmentPlotSelectors,
            time_sel = input$timePlotSelectors,
            endpoint = data$endpoint,
            format = "html",
            cfb = data$cfb,
            power = data$power,
            num_groups = data$num_groups,
            inputs = shiny$reactiveValuesToList(input),
            type = "bar"
          )
          ggplot2$ggsave(filename = path, plot = plot, device = "jpg", width = 14, height = 12, units = "in", dpi = 300)
          plot
        }
      })



      shiny$observe({
        shiny$req(prismData())

        uuid <- signal()$input_data$session_data$uuid
        shiny$req(uuid)
        input
        st <- storr$storr_rds("storr")

        id <- paste0(uuid, "-prism")
        st$set(id, shiny$reactiveValuesToList(input))
        data
      })
      input
    }
  )
}
