#' analysis_a_run
#' @description Each test has a letter label
#' @export
analysis_a_run <- function(id = "analysis_a", user, is_admin) {
  box::use(shiny)
  box::use(. / analysis_a_setup)
  box::use(. / testSpinner)
  box::use(. / prism)
  box::use(. / analysis_a_report)
  box::use(glue)
  ns <- shiny$NS(id)
  shiny$tabsetPanel(
    id = ns("test_1_tabs"),
    shiny$tabPanel(
      shiny$h5("Analysis Setup", class = "p-2"),
      value = "Analysis Setup",
      shiny$div(
        analysis_a_setup$analysis_a_setup(ns("analysis_a_setup")),
        class = "p-3"
      )
    ),
    shiny$tabPanel(
      shiny$h5("Exploratory Plots", class = "p-2"),
      value = "Plots",
      shiny$fluidRow(
        class = "p-2",
        shiny$column(
          12,
          testSpinner$testSpinner(shiny$uiOutput(ns("PlotsPanel"))),
          testSpinner$testSpinner(shiny$uiOutput(ns("Plots")))
        )
      )
    ),
    shiny$tabPanel(
      shiny$h5("Analysis Results", class = "p-2"),
      value = "Analysis Results",
      shiny$fluidRow(
        class = "p-3",
        shiny$column(
          12,
          testSpinner$testSpinner(shiny$uiOutput(ns("tableSelectors"))),
          testSpinner$testSpinner(
            shiny$uiOutput(ns("analysisPanel"))
          )
        )
      )
    ),
    shiny$tabPanel(
      shiny$h5("Custom Plots", class = "p-2"),
      value = "Prism Plots",
      shiny$fluidRow(
        class = "p-2",
        prism$ui_prism(ns("prism"))
      )
    ),
    shiny$tabPanel(
      shiny$h5("Export", class = "p-2"),
      value = "Export",
      shiny$div(analysis_a_report$ui_analysis_a_report(ns("analysis_a_report")), class = "p-3")
    )
  )
}



#' analysis_a_run_server
#' @export
analysis_a_run_server <- function(id, input_signal, cache = FALSE) {
  box::use(shiny)
  box::use(readr[read_rds, write_rds])
  box::use(. / connect_table)
  box::use(shiny.router)
  box::use(purrr)
  box::use(dplyr)
  box::use(stats[complete.cases])
  box::use(. / analysis_a_setup)
  box::use(stringr)
  box::use(tibble)
  box::use(tidyr)
  box::use(forcats)
  box::use(tictoc)
  box::use(. / vizualization)
  box::use(. / final_modeling)
  box::use(plotly)
  box::use(. / final_output)
  box::use(. / prism)
  box::use(openxlsx)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      input_data <- shiny$reactive({
        if (cache) {
          input_signal <- read_rds("input_signal.rda")
        } else {
          shiny$req(input_signal())
          input_signal <- input_signal()
          write_rds(input_signal, "input_signal.rda")
        }

        shiny.router$change_page("analysisa_run")
        input_data <- input_signal$input_data
        con <- connect_table$connect_table()
        session_data <- dplyr$tbl(con, "sessions")
        browser()
        session_data <- dplyr$arrange(session_data, desc(timestamp))
        session_data <- dplyr$first(session_data)
        session_data <- dplyr$collect(session_data)


        list(
          input_data = input_data,
          session_data = session_data
        )
      })

      signal <- analysis_a_setup$analysis_a_setup_server("analysis_a_setup", input_data)


      shiny$observeEvent(signal(),
        {
          shiny$updateTabItems(
            session,
            inputId = "test_1_tabs",
            selected = "Plots"
          )
        },
        ignoreInit = FALSE
      )


      output$typeAssignmentTablePlots <- shiny$renderUI({
        shiny$req(signal())
        box::use(. / make_type_assignment_table)
        data <- signal()$input_data
        type_inputs <- dplyr$distinct(data, Type, type_snake)
        make_type_assignment_table$make_type_assignment_table(type_inputs, ns)
      })

      output$groupAssignmentTablePlots <- shiny$renderUI({
        shiny$req(signal())
        data <- input_data()$input_data

        treatment_input <- dplyr$distinct(data, treatment_snake, Treatment)
        treatment_input <- dplyr$filter(treatment_input, complete.cases(data))

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

      analysis_input <- shiny$reactive({
        req(signal())

        if (getOption("devmode")) {
          session_message <- glue$glue("Your session ID is {signal()$session_data$uuid}")
          shiny$showNotification(session_message, closeButton = TRUE, duration = NULL)
        }
        sig <- signal()
        id <- sig$selections
        input_data <- sig$input_data$data
        names_input <- names(id)
        type_inputs <- stringr$str_detect(names_input, "type_")
        treatment_inputs <- stringr$str_detect(names_input, "treatment_")
        type_list <- id[type_inputs]
        treatment_list <- id[treatment_inputs]
        type_table <- dplyr$bind_rows(imap(type_list, function(x, y) tibble$tibble(TypeNew = x, type_snake = y)))
        treatment_table <- dplyr$bind_rows(purrr$imap(treatment_list, function(x, y) tibble$tibble(TreatmentNew = x, treatment_snake = y)))
        filtered_1 <- dplyr$inner_join(input_data, type_table)
        filtered_1 <- dplyr$left_join(filtered_1, treatment_table)
        filtered_1 <-
          dplyr$mutate(
            filtered_1,
            TreatmentNew = ifelse(TypeNew == "Wild Type", "Wild Type", TreatmentNew),
            Treatment = ifelse(TypeNew == "Wild Type", "Wild Type", Treatment)
          )

        filtered_1
      })

      analysis_input_data <- shiny$reactive({
        req(analysis_input())

        data <- analysis_input()
        base_col <- which(colnames(data) == "Baseline")
        type_snake_col <- which(colnames(data) == "type_snake")
        data <-
          dplyr$mutate(
            data,
            trt = TreatmentNew,
            TreatmentNew = dplyr$if_else(TypeNew == "Wild Type", "Wild Type", TreatmentNew),
            basic_model = stringr$str_detect(TreatmentNew, "Vehicle|Treatment")
          )
        data <- tidyr$pivot_longer(data,
          cols = (base_col + 1):(type_snake_col - 1),
          names_to = "Time", values_to = "Response"
        )

        data <- dplyr$mutate(data, Time = forcats$as_factor(Time))

        data
      })

      #       # BEGIN BRANCH FOR PLOTS AND TABLES
      pre_modeling_output <- shiny$reactive({
        shiny$req(analysis_input_data())

        data <- analysis_input_data()
        selections <- signal()$selections
        data <-
          dplyr$mutate(
            data,
            Treatment = factor(ifelse(
              is.na(Dose) | Dose == "NA",
              Treatment,
              paste(Treatment, Dose)
            ))
          )

        data <- tryCatch(expr = {
          tictoc$tic()
          data <- pre_modeling(data, selections$changeFromBaseline)
          time <- tictoc$toc()
          timeInSeconds <- as.character(round(time$toc - time$tic, 3))
          showNotification(tags$p("pre_modeling", tags$pre(timeInSeconds)), closeButton = TRUE, duration = NULL)
          # data
          data
        }, error = function(err) {
          err <- as.character(err)
          showNotification(err, duration = NULL)
          showNotification("An error occurred, please check your configuration.", closeButton = TRUE, duration = NULL)
          FALSE
        })
        req(data)
        data
      })

      pre_plot_input <- shiny$reactive({
        shiny$req(input$y_axis)
        shiny$req(input$treatmentPlotSelectors)
        shiny$req(input$timePlotSelectors)
        shiny$req(signal())
        shiny$req(pre_modeling_output())

        endpoint <- signal()$input_data$endpoint
        tictoc$tic()
        data <- pre_modeling_output()
        time <- tictoc$toc()
        timeInSeconds <- as.character(round(time$toc - time$tic, 3))
        shiny$showNotification(tags$p("pre_modeling_output", tags$pre(timeInSeconds)), closeButton = TRUE, duration = NULL)
        ui_selections <- list(
          y_axis = input$y_axis,
          trt_sel = input$treatmentPlotSelectors,
          time_sel = input$timePlotSelectors,
          num_rows = input$num_rows
        )

        list(
          data = data, endpoint = endpoint, ui_selections = ui_selections
        )
      })

      interactive_plots <- shiny$reactive({
        shiny$req(input$y_axis)
        shiny$req(pre_plot_input())
        #
        data <- pre_plot_input()$data
        ui_sel <- pre_plot_input()$ui_selections
        endpoint <- pre_plot_input()$endpoint
        baseline_selected <- "Baseline" %in% pre_plot_input()$ui_selections$time_sel

        tictoc$tic()

        plots <- vizualization$vizualization(
          transformed_data = data$transformed_data,
          power = data$box_cox,
          endpoint = endpoint,
          ui_sel = ui_sel
        )

        time <- tictoc$toc()
        timeInSeconds <- as.character(round(time$toc - time$tic, 3))
        shiny$showNotification(tags$p("vizualization", tags$pre(timeInSeconds)), closeButton = TRUE, duration = NULL)

        return(list(plots = plots, data = data, baseline_selected = baseline_selected))
      })


      output$analysisPlot_1 <- plotly$renderPlotly({
        shiny$req(interactive_plots())

        plots <- interactive_plots()
        plot <- plots$plots$box
        plot <- ylab_move(
          plot = plotly$ggplotly(plot),
          x_parameter = 0.06,
          y_parameter = 0.00
        )
        plot$x$layout$margin$t <- 75
        plot$x$layout$margin$l <- 75
        plot <- vizualization$bold_interactive(plot, panel = TRUE)
        plot <- plotly$layout(
          vizualization$label_fix(plot = plot),
          height = 525
        )
      })

      output$analysisPlot_2 <- plotly$renderPlotly({
        shiny$req(interactive_plots())
        plots <- interactive_plots()
        plot <- plots$plots$bar
        plot <- vizualization$bold_interactive(plot, panel = FALSE)
        p <- vizualization$label_fix(plot = plotly$ggplotly(plot))
        p <- plotly$layout(p, height = 525)
        p
      })


      output$analysisPlot_3 <- plotly$renderPlotly({
        req(pre_plot_input())
        plots <- interactive_plots()
        tmp <- plotly$ggplotly(plots$plots$group_line)
        for (i in 1:length(tmp$x$data)) {
          tmp2 <- tmp$x$data[[i]]
          if (tmp2$type == "scatter") {
            if (tmp2$mode == "markers") {
              data <- dplyr$filter(
                plots$data$transformed_data,
                Treatment == tmp2$name
              )
              if (plots$baseline_selected) {
                times <- as.character(unique(data$Time))
                data <-
                  tidyr$pivot_wider(
                    data,
                    id_cols = c(SubjectID, Treatment, Baseline),
                    values_from = "Response_Transformed",
                    names_from = "Time"
                  )
                data <-
                  tidyr$pivot_longer(
                    data,
                    cols = c("Baseline", times),
                    values_to = "Response", names_to = "Time"
                  )
              }
              tmp$x$data[[i]]$text <- purrr$map_chr(.x = 1:nrow(data), .f = ~ {
                text <- paste0(tmp2$text[.x], "<br />SubjectID: ", data$SubjectID[.x])
                split <- stringr$str_split(
                  paste0(tmp2$text[.x], "<br />SubjectID: ", data$SubjectID[.x]),
                  "<br />"
                )[[1]]
                paste(split[c(1, 2, 4, 3)], collapse = "<br />")
              })
            }
          }
        }
        tmp <- vizualization$bold_interactive(tmp, panel = FALSE)
        vizualization$label_fix(plot = plotly$ggplotly(tmp)) %>% plotly$layout(height = 600)
      })


      output$analysisPlot_4 <- plotly$renderPlotly({
        plots <- interactive_plots()
        plot <- label_fix(ggplotly(plots$plots$sub_line))
        plot$x$layout$margin$t <- 75
        plot$x$layout$margin$l <- 75
        plot <- ylab_move(plot = plot, x_parameter = 0.06, y_parameter = 0.00)
        plot <- bold_interactive(plot, panel = TRUE)
        label_fix(plot = ggplotly(plot)) %>% layout(height = 525)
      })


      output$analysisPanel <- shiny$renderUI({
        shiny$req(pre_modeling_output())
        data <- pre_modeling_output()
        req(data)
        treatmentPlotSelectors <- levels(data$transformed_data$TreatmentNew)
        timePlotSelectors <- c("Baseline", unique(as.character(data$transformed_data$Time)))

        shiny$fluidRow(
          shiny$column(12, testSpinner$testSpinner(uiOutput(ns("analysisInputsData"))))
        )
      })

      output$PlotsPanel <- shiny$renderUI({
        req(pre_modeling_output())

        data <- pre_modeling_output()

        # TODO we need to add timePlotSelectors to the table panel
        # TODO dont include baseline as a time, just check changefrombaseline logic and dont use it
        treatmentPlotSelectors <- levels(data$transformed_data$Treatment)
        timePlotSelectors <- c("Baseline", unique(as.character(data$transformed_data$Time)))

        bs4Dash$box(
          width = 12,
          title = "Options",
          collapsible = TRUE,
          shiny$div(
            class = "d-flex justify-content-around",
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
            shiny$selectizeInput(
              inputId = ns("timePlotSelectors"),
              label = shiny$h4("Select Times to be Plotted"),
              selected = timePlotSelectors,
              choices = timePlotSelectors, multiple = TRUE
            ),
            shiny$radioButtons(ns("y_axis"), shiny$h4("Select y axis"),
              choiceNames = list(
                "Transform (suggested by Box-Cox)",
                "No Transform (original scale)",
                "Change from Baseline"
              ),
              choiceValues = list(
                "transform", "no_transform", "change_from_baseline"
              )
            ),
            shiny$numericInput(ns("num_rows"),
              label = "Number of Rows for Panel Plots",
              value = 1, min = 1, max = length(treatmentPlotSelectors), step = 1
            )
          )
        )
      })

      output$Plots <- shiny$renderUI({
        purrr$map(
          c("analysisPlot_1", "analysisPlot_2", "analysisPlot_3", "analysisPlot_4"),
          function(x) {
            bs4Dash$box(
              width = 12,
              collapsible = TRUE,
              maximizable = TRUE,
              testSpinner$testSpinner(
                plotly$plotlyOutput(ns(x), height = ifelse(x == "analysisPlot_3", "600px", "525px"))
              )
            )
          }
        )
      })






      # TABLES ------------------------------------------------------------------

      pre_tables_input <- shiny$reactive({
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
          shiny$showNotification(
            shiny$div(
              message,
              shiny$actionButton(ns("submitError"), "Request Review", class = "btn btn-primary")
            ),
            type = "error", closeButton = T, duration = NULL
          )
          shiny$req(FALSE)
        } else {
          tictoc$tic()
          if (analysis_type == "Exploratory") {
            final_model <- final_modeling$final_modeling(data, analysis_type = analysis_type, overall_trend = FALSE)
          } else {
            final_model <- final_modeling$final_modeling(data,
              toi = signal()$timeSelectionInput,
              analysis_type = analysis_type,
              overall_trend = FALSE # Change this to TRUE to include the overall average
            )
          }
          time <- tictoc$toc()
          timeInSeconds <- as.character(round(time$toc - time$tic, 3))
          shiny$showNotification(tags$p("final_modeling", tags$pre(timeInSeconds)), closeButton = TRUE, duration = NULL)

          tables <- final_output$html_tables(data$transformed_data, final_model)

          trans_name <- final_output$transform_table() %>%
            dplyr$filter(power == final_model$power) %>%
            dplyr$select(transform_name) %>%
            unlist() %>%
            unname()

          footer <- dplyr$if_else(
            trans_name == "No transformation",
            "No transformation was applied to the data. Mean and SE are estimated using model based LSmean",
            paste(trans_name, "transformation was applied to the data.  Mean and SE are estimated using model based LSmean")
          )

          tables$tab0 <- dplyr$bind_rows(tables$tab1, tables$tab2) %>%
            dplyr::select(Treatment, `Time Points`, grep("Original", colnames(.)))
          if (analysis_type == "Exploratory") {
            tables$tab0 <- tables$tab0 %>%
              dplyr$mutate(num = as.numeric(gsub("[A-z]| ", "", `Time Points`))) %>%
              dplyr$arrange(Treatment, num) %>%
              dplyr$select(-num)
          }
          list(tables = tables, footer = footer, power = data$box_cox, print_tables = print_tables)
        }
      })

      shiny$observeEvent(input$submitError, {
        shiny$showNotification("Data submitted for review")
      })
      #

      output$tableSelectors <- shiny$renderUI({
        shiny$req(signal())
        shiny$req(pre_tables_input())
        data <- pre_modeling_output()

        sessionMode <- signal()$session_data$sessionMode
        if (sessionMode == "Exploratory") {
          timePlotSelectors <- unique(as.character(data$transformed_data$Time))
          toi <- timePlotSelectors[length(timePlotSelectors)]
        } else {
          toi <- signal()$timeSelectionInput
          timePlotSelectors <- toi
        }

        if (signal()$session$sessionMode == "Exploratory") {
          bs4Dash$box(
            width = 12,
            title = "Options",
            collapsible = TRUE,
            dplyr$div(
              class = "d-flex justify-content-around",
              bas4Dash$tooltip(
                shiny$selectizeInput(
                  inputId = ns("timeTreatmentSelectorsTable"),
                  label = h4("Select Times (up to 5) to be Displayed"),
                  selected = toi,
                  choices = timePlotSelectors, multiple = TRUE, options = list(maxItems = 5)
                ),
                "Use delete key to remove, mouse click to add."
              )
            )
          )
        } else {
          shiny$div()
        }
      })

      output$analysisInputsData <- shiny$renderUI({
        shiny$req(pre_tables_input())
        tables <- pre_tables_input()$tables
        wb <- openxlsx$createWorkbook()
        openxlsx$addWorksheet(wb = wb, sheetName = "Table 1")
        openxlsx$addWorksheet(wb = wb, sheetName = "Table 2")
        openxlsx$addWorksheet(wb = wb, sheetName = "Table 3")
        openxlsx$addWorksheet(wb = wb, sheetName = "Table 4")
        openxlsx$writeData(wb = wb, sheet = "Table 1", x = tables$tab0)
        openxlsx$writeData(wb = wb, sheet = "Table 2", x = tables$tab1)
        openxlsx$writeData(wb = wb, sheet = "Table 3", x = tables$tab2)
        openxlsx$writeData(wb = wb, sheet = "Table 4", x = tables$tab3)
        tables_path <- fs$path_join(c(input_data()$session_data$full_path_files, "analysis_results.xlsx"))

        openxlsx$saveWorkbook(wb, file = tables_path, overwrite = TRUE)

        footer <- pre_tables_input()$footer
        transformation <- pre_tables_input()$power != 1
        print_tables <- pre_tables_input()$print_tables
        analysis_type <- signal()$session$sessionMode

        if (analysis_type == "Exploratory") {
          times <- input$timeTreatmentSelectorsTable
          tables$tab0 <- tables$tab0 %>% dplyr$filter(`Time Points` %in% times)
          tables$tab1 <- tables$tab1 %>% dplyr$filter(`Time Points` %in% times)
          tables$tab2 <- tables$tab2 %>% dplyr$filter(`Time Points` %in% times)
          tables$tab3 <- tables$tab3 %>% dplyr$filter(`Time Points` %in% times)
        }

        if (print_tables) {
          if (transformation) {
            table_gt <- list(
              list(
                table = final_output$html_table_gt(
                  data = tables$tab0, title = paste("Table 1: Summary Statistics for", signal()$input_data$endpoint),
                  footer = "", include_summary = F, summary_only = T, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = final_output$html_table_gt(
                  data = tables$tab1, title = paste(
                    "Table 2: Comparison between Controls and Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = final_output$html_table_gt(
                  data = tables$tab2, title = paste(
                    "Table 3: Comparison among the Vehicle and Treatment Groups as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = final_output$html_table_gt(
                  data = tables$tab3, title = paste(
                    "Table 4: Comparison between Doses and Controls/Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = T, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              )
            )
          } else {
            table_gt <- list(
              list(
                table = final_output$html_table_gt(
                  data = tables$tab1, title = paste(
                    "Table 1: Comparison between Controls and Wild Type as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = F, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = final_output$html_table_gt(
                  data = tables$tab2, title = paste(
                    "Table 2: Comparison among the Vehicle and Treatment Groups as to",
                    signal()$input_data$endpoint
                  ),
                  footer = footer, include_summary = T, summary_only = F, transformation = F, analysis_type = analysis_type,
                  endpoint = signal()$input_data$endpoint
                )
              ),
              list(
                table = final_output$html_table_gt(
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

          shiny$div(
            purrr$map(
              table_gt,
              function(x) {
                bs4Dash$box(
                  maximizable = TRUE, collapsible = TRUE,
                  width = 12, x$table
                )
              }
            )
          )
        }
      })

      # Split data for UI AND Markdown
      test_1_output_data <- shiny$reactive({
        shiny$req(pre_plot_input())
        shiny$req(pre_modeling_output())
        shiny$req(pre_tables_input())
        shiny$req(signal())

        data <- list(
          plot = pre_plot_input(),
          tables = pre_tables_input(),
          pre_modeling_input = pre_modeling_output(),
          input_data = signal(),
          inputs = shiny$reactiveValuesToList(input)
        )
        st <- storr$storr_rds("storr")
        id <- paste0(signal()$session_data$uuid, "-final")
        st$set(id, data)

        data
      })

      prism$server_prism(
        signal = test_1_output_data
      )
    }
  )
}
