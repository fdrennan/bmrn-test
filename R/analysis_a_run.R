#' analysis_a_run
#' @description Each test has a letter label
#' @export
analysis_a_run <- function(id = "analysis_a", user, is_admin) {
  box::use(shiny)
  ns <- NS(id)
  tabsetPanel(
    id = ns("test_1_tabs"),
    tabPanel(
      h5("Analysis Setup", class = "p-2"),
      value = "Analysis Setup",
      div(
        analysis_a_setup(ns("analysis_a_setup")),
        class = "p-3"
      )
    ),
    tabPanel(
      h5("Exploratory Plots", class = "p-2"),
      value = "Plots",
      fluidRow(
        class = "p-2",
        testSpinner(uiOutput(ns("PlotsPanel"))),
        testSpinner(uiOutput(ns("Plots")))
      )
    )
    # tabPanel(
    #   h5("Analysis Results", class = "p-2"),
    #   value = "Analysis Results",
    #   fluidRow(
    #     class = "p-3",
    #     testSpinner(uiOutput(ns("tableSelectors"))),
    #     testSpinner(
    #       uiOutput(ns("analysisPanel"))
    #     )
    #   )
    # ),
    # tabPanel(
    #   h5("Custom Plots", class = "p-2"),
    #   value = "Prism Plots",
    #   fluidRow(
    #     class = "p-2",
    #     ui_prism()
    #   )
    # ),
    # tabPanel(
    #   h5("Export", class = "p-2"),
    #   value = "Export",
    #   div(ui_analysis_a_report(), class = "p-3")
    # )
  )
}



#' analysis_a_run_server
#' @export
analysis_a_run_server <- function(id, input_signal, cache = FALSE) {
  box::use(shiny)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      input_data <- reactive({
        
        if(cache) {
          input_signal <- read_rds('input_signal.rda')
        } else {
          req(input_signal())
          input_signal <- input_signal()
          write_rds(input_signal, 'input_signal.rda')
        }
        
        change_page("analysisa_run")
        input_data <- input_signal$input_data
        con <- connect_table()
        session_data <- tbl(con, "sessions") %>%
          arrange(desc(timestamp)) %>%
          first() %>%
          collect()

        
        list(
          input_data = input_data,
          session_data = session_data
        )
      })

      signal <- analysis_a_setup_server("analysis_a_setup", input_data)

      observeEvent(signal(),
        {
          
          updateTabItems(
            session,
            inputId = "test_1_tabs",
            selected = "Plots"
          )
        },
        ignoreInit = FALSE
      )


      output$typeAssignmentTablePlots <- renderUI({
        shiny$req(signal())
        # 
        data <- signal()$input_data
        type_inputs <- distinct(data, Type, type_snake)
        make_type_assignment_table(type_inputs, ns)
      })

      output$groupAssignmentTablePlots <- renderUI({
        shiny$req(signal())
        
        data <- input_data()$input_data

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

      analysis_input <- reactive({
        req(signal())
        # 
        if (getOption("devmode")) {
          session_message <- glue("Your session ID is {signal()$session_data$uuid}")
          showNotification(session_message, closeButton = TRUE, duration = NULL)
        }
        browser()
        
        sig <- signal()
        id <- sig$input_data$data
        # 
        # id <- signal()$input_data
        # id <- id$data$data
        names_input <- names(sig)
        type_inputs <- str_detect(names_input, "type_")
        treatment_inputs <- str_detect(names_input, "treatment_")
        type_list <- distinct(sig[type_inputs])
        treatment_list <- distinct(id[treatment_inputs])
        type_table <- distinct(bind_rows(imap(type_list, function(x, y) tibble(Type = x, type_snake = y))))
        filtered_1 <- inner_join(id$data$data, type_table)

        treatment_table <- distinct(bind_rows(imap(treatment_list, function(x, y) tibble(Treatment = x, treatment_snake = y))))
        filtered_1 <- left_join(filtered_1, treatment_table)
        filtered_1 <-
          filtered_1 %>%
          mutate(Treatment = ifelse(Type == "Wild Type", "Wild Type", Treatment))

        filtered_1
      })

      analysis_input_data <- reactive({
        req(analysis_input())
        
        data <- analysis_input()
        data <-
          data %>%
          mutate(
            trt = Treatment,
            Treatment = replace_na(Treatment, "Wild Type"),
            basic_model = str_detect(Treatment, "Vehicle|Treatment")
          )

        data <- pivot_longer(data, cols = c(
          contains("Week"), contains("Day"),
          contains("Year"), contains("Month"),
          contains("Second"), contains("Minute"),
          contains("Time"),
        ), names_to = "Time", values_to = "Response")

        data
      })


      #       # BEGIN BRANCH FOR PLOTS AND TABLES
      pre_modeling_output <- reactive({
        req(analysis_input_data())
        
        data <- analysis_input_data()

        data <- data %>%
          mutate(Treatment = factor(ifelse(is.na(Dose) | Dose == "NA", Treatment,
            paste(Treatment, Dose)
          )))
        data <- tryCatch(expr = {
          pre_modeling(data, signal()$changeFromBaseline)
        }, error = function(err) {
          err <- as.character(err)
          showNotification(err, duration = NULL)
          showNotification("An error occurred, please check your configuration.")
          FALSE
        })
        req(data)
        data
      })

      # observe({
      #   req(pre_modeling_output())
      #   showNotification(
      #     h6(class = "text-center p-2", "Setup complete, you may now review the other panels.")
      #   )
      # })
      pre_plot_input <- reactive({
        req(signal())
        endpoint <- signal()$input_data$endpoint
        data <- pre_modeling_output()
        req(input$y_axis)
        req(input$treatmentPlotSelectors)
        req(input$timePlotSelectors)
        ui_selections <- list(
          y_axis = input$y_axis,
          trt_sel = input$treatmentPlotSelectors,
          time_sel = input$timePlotSelectors
        )

        times <- unique(data$transformed_data$Time)[
          order(as.numeric(gsub("[A-z]| ", "", unique(data$transformed_data$Time))))
        ]
        data$transformed_data$Time <- factor(data$transformed_data$Time,
          levels = times
        )

        data$transformed_data <- filter(data$transformed_data, Treatment %in% input$treatmentPlotSelectors)
        data$transformed_data <- filter(data$transformed_data, Time %in% input$timePlotSelectors)

        list(
          data = data, endpoint = endpoint, ui_selections = ui_selections
        )
      })

      interactive_plots <- reactive({
        req(input$y_axis)
        req(pre_plot_input())
        data <- pre_plot_input()$data
        endpoint <- pre_plot_input()$endpoint
        baseline_selected <- "Baseline" %in% pre_plot_input()$ui_selections$time_sel
        req(input$y_axis)
        if (input$y_axis == "transform") {
          plots <- vizualization(
            transformed_data = data$transformed_data,
            power = data$box_cox,
            endpoint = endpoint,
            baseline = FALSE || !baseline_selected, # logic is backwards in the functions
            transformation = TRUE
          )
        }
        if (input$y_axis == "no_transform") {
          plots <- vizualization(
            transformed_data = data$transformed_data,
            power = data$box_cox,
            endpoint = endpoint,
            transformation = FALSE,
            baseline = FALSE || !baseline_selected
          )
        }

        if (input$y_axis == "change_from_baseline") {
          transformed_data <- data$transformed_data %>%
            mutate(Response_Transformed = as.numeric(Response_Transformed) - as.numeric(Baseline))

          plots <- vizualization(
            transformed_data = data$transformed_data,
            power = data$box_cox,
            endpoint = endpoint,
            transformation = FALSE,
            baseline = TRUE
          )
        }
        return(list(plots = plots, data = data, baseline_selected = baseline_selected))
      })

      pre_tables_input <- reactive({
        req(signal())
        req(pre_modeling_output())
        input <- signal()$input_data
        data <- pre_modeling_output()
        analysis_type <- signal()$session_data$sessionMode
        print_tables <- ifelse(all(!data$error), TRUE, FALSE)
        if (!print_tables) {
          message <- if_else(data$error$error_trans == TRUE,
            "Consult Statistician: Transformation did not
                             lead to normally distributed residuals",
            "Consult Statistician: Variance within basic model (Vehicle and Treatment
                             groups) are statistically different."
          )
          showNotification(
            div(
              message,
              actionButton(ns("submitError"), "Request Review", class = "btn btn-primary")
            ),
            type = "error", closeButton = T, duration = NULL
          )
          req(FALSE)
        } else {
          # showNotification("Analysis setup complete, you may visit the other panels.")
          # TODO timeshow is the new selector
          if (analysis_type == "Exploratory") {
            final_model <- final_modeling(data, analysis_type = analysis_type)
          } else {
            final_model <- final_modeling(data,
              toi = signal()$timeSelectionInput,
              analysis_type = analysis_type
            )
          }

          tables <- html_tables(data$transformed_data, final_model)

          trans_name <- transform_table() %>%
            filter(power == final_model$power) %>%
            select(transform_name) %>%
            unlist() %>%
            unname()

          footer <- if_else(
            trans_name == "No transformation",
            "No transformation was applied to the data. Difference and CI are estimated using model based LSmean",
            paste(trans_name, "transformation was applied to the data.  Difference and CI are estimated using model based LSmean")
          )

          tables$tab0 <- bind_rows(tables$tab1, tables$tab2) %>%
            dplyr::select(Treatment, `Time Points`, grep("Original", colnames(.)))
          if (analysis_type == "Exploratory") {
            tables$tab0 <- tables$tab0 %>%
              arrange(Treatment, `Time Points`)
          }
          list(tables = tables, footer = footer, power = data$box_cox, print_tables = print_tables)
        }
      })

      observeEvent(input$submitError, {
        showNotification("Data submitted for review")
      })


      output$tableSelectors <- renderUI({
        data <- pre_modeling_output()
        req(signal())
        sessionMode <- signal()$session_data$sessionMode
        if (sessionMode == "Exploratory") {
          timePlotSelectors <- unique(as.character(data$transformed_data$Time))
          toi <- timePlotSelectors[length(timePlotSelectors)]
        } else {
          toi <- signal()$timeSelectionInput
          timePlotSelectors <- toi
        }

        box(
          width = 12,
          title = "Options",
          collapsible = TRUE,
          div(
            class = "d-flex justify-content-around",
            tooltip(
              selectizeInput(
                inputId = ns("timeTreatmentSelectorsTable"),
                label = h4("Select Times (up to 5) to be Displayed"),
                selected = toi,
                choices = timePlotSelectors, multiple = TRUE, options = list(maxItems = 5)
              ),
              "Use delete key to remove, mouse click to add."
            )
          )
        )
      })

      output$analysisInputsData <- renderUI({
        tables <- pre_tables_input()$tables
        footer <- pre_tables_input()$footer
        transformation <- pre_tables_input()$power != 1
        print_tables <- pre_tables_input()$print_tables
        analysis_type <- signal()$session$sessionMode
        times <- input$timeTreatmentSelectorsTable
        if (analysis_type == "Exploratory") {
          tables$tab0 <- tables$tab0 %>% filter(`Time Points` %in% times)
          tables$tab1 <- tables$tab1 %>% filter(`Time Points` %in% times)
          tables$tab2 <- tables$tab2 %>% filter(`Time Points` %in% times)
          tables$tab3 <- tables$tab3 %>% filter(`Time Points` %in% times)
        }

        if (print_tables) {
          if (transformation) {
            wb <- createWorkbook()
            addWorksheet(wb = wb, sheetName = "Table 1")
            addWorksheet(wb = wb, sheetName = "Table 2")
            addWorksheet(wb = wb, sheetName = "Table 3")
            addWorksheet(wb = wb, sheetName = "Table 4")
            writeData(wb = wb, sheet = "Table 1", x = tables$tab0)
            writeData(wb = wb, sheet = "Table 2", x = tables$tab1)
            writeData(wb = wb, sheet = "Table 3", x = tables$tab2)
            writeData(wb = wb, sheet = "Table 4", x = tables$tab3)

            table_gt <- list(
              list(
                table = html_table_gt(
                  data = tables$tab0, title = paste("Table 1: Summary Statistics for", signal()$input_data$endpoint),
                  footer = "", include_summary = F, summary_only = T, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = html_table_gt(
                  data = tables$tab1, title = paste(
                    "Table 2: Comparison between Controls and Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = html_table_gt(
                  data = tables$tab2, title = paste(
                    "Table 3: Comparison between Doses as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = html_table_gt(
                  data = tables$tab3, title = paste(
                    "Table 4: Comparison between Doses and Controls/Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = F, summary_only = F, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              )
            )
          } else {
            wb <- createWorkbook()
            addWorksheet(wb = wb, sheetName = "Table 1")
            addWorksheet(wb = wb, sheetName = "Table 2")
            addWorksheet(wb = wb, sheetName = "Table 3")
            writeData(wb = wb, sheet = "Table 1", x = tables$tab1)
            writeData(wb = wb, sheet = "Table 2", x = tables$tab2)
            writeData(wb = wb, sheet = "Table 3", x = tables$tab3)
            table_gt <- list(
              list(
                table = html_table_gt(
                  data = tables$tab1, title = paste(
                    "Table 1: Comparison between Controls and Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = F, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = html_table_gt(
                  data = tables$tab2, title = paste(
                    "Table 2: Comparison between Doses as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = F, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = html_table_gt(
                  data = tables$tab3, title = paste(
                    "Table 3: Comparison between Doses and Controls/Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = F, summary_only = F, transformation = F, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              )
            )
          }
          tables_path <- path_join(c(input_data()$session_data$full_path_files, "tables.xlsx"))
          saveWorkbook(wb, file = tables_path, overwrite = TRUE)
          div(
            map(
              .x = table_gt, .f = function(x) {
                box(
                  maximizable = TRUE, collapsible = TRUE,
                  width = 12, x$table
                  # style = "padding: 10px;", class = "flex-center p-3"
                )
              }
            )
          )
        }
      })

      output$analysisPlot_1 <- renderPlotly({
        plots <- interactive_plots()
        plot <- ylab_move(plot = ggplotly(plots$plots[[1]]), parameter = 0.02)
        plot$x$layout$margin$t <- 75
        plot$x$layout$margin$l <- 75
        plot
      })

      output$analysisPlot_2 <- renderPlotly({
        plots <- interactive_plots()
        label_fix(plot = ggplotly(plots$plots[[2]]))
      })


      output$analysisPlot_3 <- renderPlotly({
        req(pre_plot_input())
        plots <- interactive_plots()
        tmp <- ggplotly(plots$plots[[3]])
        for (i in 1:length(tmp$x$data)) {
          tmp2 <- tmp$x$data[[i]]
          if (tmp2$type == "scatter") {
            if (tmp2$mode == "markers") {
              data <- plots$data$transformed_data %>% filter(Treatment == tmp2$name)
              if (plots$baseline_selected) {
                times <- as.character(unique(data$Time))
                data <- data %>%
                  pivot_wider(
                    id_cols = c(SubjectID, Treatment, Baseline),
                    values_from = "Response_Transformed",
                    names_from = "Time"
                  ) %>%
                  pivot_longer(
                    cols = c("Baseline", times),
                    values_to = "Response", names_to = "Time"
                  )
              }
              tmp$x$data[[i]]$text <- map_chr(.x = 1:nrow(data), .f = ~ {
                text <- paste0(tmp2$text[.x], "<br />SubjectID: ", data$SubjectID[.x])
                split <- str_split(
                  paste0(tmp2$text[.x], "<br />SubjectID: ", data$SubjectID[.x]),
                  "<br />"
                )[[1]]
                paste(split[c(1, 2, 4, 3)], collapse = "<br />")
              })
            }
          }
        }
        tmp
      })


      output$analysisPlot_4 <- renderPlotly({
        plots <- interactive_plots()
        plot <- label_fix(ggplotly(plots$plots[[4]]))
        plot$x$layout$margin$t <- 75
        plot$x$layout$margin$l <- 75
        plot
      })


      output$analysisPanel <- renderUI({
        data <- pre_modeling_output()
        req(data)
        treatmentPlotSelectors <- levels(data$transformed_data$Treatment)
        timePlotSelectors <- c("Baseline", unique(as.character(data$transformed_data$Time)))

        fluidRow(
          column(12, testSpinner(uiOutput(ns("analysisInputsData"))))
        )
      })

      output$PlotsPanel <- renderUI({
        req(pre_modeling_output())

        data <- pre_modeling_output()

        # TODO we need to add timePlotSelectors to the table panel
        # TODO dont include baseline as a time, just check changefrombaseline logic and dont use it
        treatmentPlotSelectors <- levels(data$transformed_data$Treatment)
        timePlotSelectors <- c("Baseline", unique(as.character(data$transformed_data$Time)))

        box(
          width = 12,
          title = "Options",
          collapsible = TRUE,
          div(
            class = "d-flex justify-content-around",
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
            selectizeInput(
              inputId = ns("timePlotSelectors"),
              label = h4("Select Times to be Plotted"),
              selected = timePlotSelectors,
              choices = timePlotSelectors, multiple = TRUE
            ),
            radioButtons(ns("y_axis"), h4("Select y axis"),
              choiceNames = list(
                "Transform (suggested by Box-Cox)",
                "No Transform (original scale)",
                "Change from Baseline"
              ),
              choiceValues = list(
                "transform", "no_transform", "change_from_baseline"
              )
            )
          )
        )
      })

      output$Plots <- renderUI({
        map(
          c("analysisPlot_1", "analysisPlot_2", "analysisPlot_3", "analysisPlot_4"),
          function(x) {
            box(
              width = 12,
              collapsible = TRUE,
              maximizable = TRUE,
              testSpinner(
                plotlyOutput(ns(x))
              )
            )
          }
        )
      })

      # Split data for UI AND Markdown
      test_1_output_data <- reactive({
        req(pre_plot_input())
        req(pre_modeling_output())
        req(pre_tables_input())
        req(signal())
        data <- list(
          plot = pre_plot_input(),
          tables = pre_tables_input(),
          pre_modeling_input = pre_modeling_output(),
          input_data = signal(),
          inputs = reactiveValuesToList(input)
        )

        st <- storr_rds("storr")

        id <- paste0(signal()$session_data$uuid, "-final")
        st$set(id, data)
        data
      })
      eventReactive(input$runReport, {
        data <- test_1_output_data()
      })

      test_1_output_data
    }
  )
}
