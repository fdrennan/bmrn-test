#' analysis_a_run
#' @description Each test has a letter label
#' @export
analysis_a_run <- function(id = "analysis_a", user, is_admin) {
  ns <- NS(id)


  tabsetPanel(
    id = "test_1_tabs",
    tabPanel(
      h5("Analysis Setup", class = "p-2"),
      value = "Analysis Setup",
      div(
        analysis_a_setup(),
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
    ),
    tabPanel(
      h5("Analysis Results", class = "p-2"),
      value = "Analysis Results",
      fluidRow(
        class = "p-3",
        testSpinner(uiOutput(ns("tableSelectors"))),
        testSpinner(
          uiOutput(ns("analysisPanel"))
        )
      )
    ),
    tabPanel(
      h5("Custom Plots", class = "p-2"),
      value = "Prism Plots",
      fluidRow(
        class = "p-2",
        ui_prism()
      )
    ),
    tabPanel(
      h5("Export", class = "p-2"),
      value = "Export",
      div(ui_analysis_a_report(), class = "p-3")
    )
  )
}



#' analysis_a_run_server
#' @export
analysis_a_run_server <- function(input, output, session, user, 
                                  is_admin, signal, cache = TRUE) {
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

  output$typeAssignmentTablePlots <- renderUI({
    data <- input_data()$data
    type_inputs <- distinct(data, Type, type_snake)
    make_type_assignment_table(type_inputs, ns)
  })



  output$groupAssignmentTablePlots <- renderUI({
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


  analysis_input <- reactive({
    req(signal())
    if (getOption("devmode")) {
      session_message <- glue("Your session ID is {signal()$session_data$uuid}")
      showNotification(session_message, closeButton = TRUE, duration = NULL)
    }
    id <- signal()$input_data
    input_data <- signal()
    names_input <- names(input_data)
    type_inputs <- str_detect(names_input, "type_")
    treatment_inputs <- str_detect(names_input, "treatment_")
    type_list <- input_data[type_inputs]
    treatment_list <- input_data[treatment_inputs]
    type_table <- bind_rows(imap(type_list, function(x, y) tibble(TypeNew = x, type_snake = y)))
    treatment_table <- bind_rows(imap(treatment_list, function(x, y) tibble(TreatmentNew = x, treatment_snake = y)))
    filtered_1 <- inner_join(id$data, type_table)
    filtered_1 <- left_join(filtered_1, treatment_table)
    filtered_1 <-
      filtered_1 %>%
      mutate(
        TreatmentNew = ifelse(TypeNew == "Wild Type", "Wild Type", TreatmentNew),
        Treatment = ifelse(TypeNew == "Wild Type", "Wild Type", Treatment)
      )
    filtered_1
  })

  analysis_input_data <- reactive({
    req(analysis_input())
    data <- analysis_input()
    base_col = which(colnames(data) == 'Baseline')
    type_snake_col = which(colnames(data) == 'type_snake')
    data <-
      data %>%
      mutate(
        trt = TreatmentNew,
        TreatmentNew = if_else(TypeNew == 'Wild Type', "Wild Type", TreatmentNew),
        basic_model = str_detect(TreatmentNew, "Vehicle|Treatment")
      )
    data <- pivot_longer(data, cols = (base_col + 1):(type_snake_col-1),
    names_to = "Time", values_to = "Response") %>%
      mutate(Time = as_factor(Time))
    
    data
  })

  # BEGIN BRANCH FOR PLOTS AND TABLES
  pre_modeling_output <- reactive({
    req(analysis_input_data())
    data <- analysis_input_data()

    data <-
      data %>%
      mutate(dose_num = as.numeric(str_extract_all(Dose, '^[:digit:]*', simplify=T))) %>% 
      arrange(dose_num) %>% 
      mutate(
        Treatment = as_factor(ifelse(is.na(Dose) | Dose == "NA", Treatment,
        paste(Treatment, Dose)))
      ) 
    
    data <- tryCatch(expr = {
      pre_modeling(data, signal()$changeFromBaseline)
    }, error = function(err) {
      err <- as.character(err)
      full_path_files <- signal()$session_data$full_path_files
      files <- dir_ls(full_path_files)
      email_message <- as.character(fluidRow(
        tableHTML(as.data.frame(purrr::keep(signal(), ~ length(.) == 1)))
      ))
      send_email(
        all_files = TRUE,
        to = getOption("EMAIL_ERROR"),
        files = files, email_message = email_message
      )
      showNotification(err, duration = NULL)
      showNotification("An error occurred, please check your configuration.")
      FALSE
    })
    req(data)
    data
  })


  pre_plot_input <- reactive({
    req(signal())
    req(pre_modeling_output())
    endpoint <- signal()$input_data$endpoint
    data <- pre_modeling_output()
    req(input$y_axis)
    req(input$treatmentPlotSelectors)
    req(input$timePlotSelectors)
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

  interactive_plots <- reactive({
    req(input$y_axis)
    req(pre_plot_input())
    data <- pre_plot_input()$data
    ui_sel <- pre_plot_input()$ui_selections
    endpoint <- pre_plot_input()$endpoint
    baseline_selected <- "Baseline" %in% pre_plot_input()$ui_selections$time_sel
      plots <- vizualization(
        transformed_data = data$transformed_data,
        power = data$box_cox,
        endpoint = endpoint,
        ui_sel = ui_sel
      )

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
      if (analysis_type == "Exploratory") {
        final_model <- final_modeling(data, analysis_type = analysis_type, overall_trend = FALSE)
      } else {
        final_model <- final_modeling(data,
          toi = signal()$timeSelectionInput,
          analysis_type = analysis_type,
          overall_trend = FALSE # Change this to TRUE to include the overall average
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
        "No transformation was applied to the data. Mean and SE are estimated using model based LSmean",
        paste(trans_name, "transformation was applied to the data.  Mean and SE are estimated using model based LSmean")
      )

      tables$tab0 <- bind_rows(tables$tab1, tables$tab2) %>%
        dplyr::select(Treatment, `Time Points`, grep("Original", colnames(.)))
      if (analysis_type == "Exploratory") {
        tables$tab0 <- tables$tab0 %>%
          mutate(num = as.numeric(gsub("[A-z]| ", "", `Time Points`))) %>%
          arrange(Treatment, num) %>%
          select(-num)
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

    if (signal()$session$sessionMode == "Exploratory") {
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
    } else {
      div()
    }
  })

  output$analysisInputsData <- renderUI({
    tables <- pre_tables_input()$tables
    wb <- createWorkbook()
    addWorksheet(wb = wb, sheetName = "Table 1")
    addWorksheet(wb = wb, sheetName = "Table 2")
    addWorksheet(wb = wb, sheetName = "Table 3")
    addWorksheet(wb = wb, sheetName = "Table 4")
    writeData(wb = wb, sheet = "Table 1", x = tables$tab0)
    writeData(wb = wb, sheet = "Table 2", x = tables$tab1)
    writeData(wb = wb, sheet = "Table 3", x = tables$tab2)
    writeData(wb = wb, sheet = "Table 4", x = tables$tab3)
    tables_path <- path_join(c(input_data()$session_data$full_path_files, "analysis_results.xlsx"))

    saveWorkbook(wb, file = tables_path, overwrite = TRUE)

    footer <- pre_tables_input()$footer
    transformation <- pre_tables_input()$power != 1
    print_tables <- pre_tables_input()$print_tables
    analysis_type <- signal()$session$sessionMode

    if (analysis_type == "Exploratory") {
      times <- input$timeTreatmentSelectorsTable
      tables$tab0 <- tables$tab0 %>% filter(`Time Points` %in% times)
      tables$tab1 <- tables$tab1 %>% filter(`Time Points` %in% times)
      tables$tab2 <- tables$tab2 %>% filter(`Time Points` %in% times)
      tables$tab3 <- tables$tab3 %>% filter(`Time Points` %in% times)
    }

    if (print_tables) {
      if (transformation) {
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
                "Table 3: Comparison among the Vehicle and Treatment Groups as to",
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
              footer = footer, include_summary = T, summary_only = F, transformation = T, analysis_type = analysis_type,
              endpoint = signal()$input_data$endpoint
            )
          )
        )
      } else {
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
                "Table 2: Comparison among the Vehicle and Treatment Groups as to",
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

      div(
        map(
          .x = table_gt, .f = function(x) {
            box(
              maximizable = TRUE, collapsible = TRUE,
              width = 12, x$table
            )
          }
        )
      )
    }
  })

  output$analysisPlot_1 <- renderPlotly({
    plots <- interactive_plots()
    plot <- plots$plots$box
    plot <- ylab_move(plot = ggplotly(plot), x_parameter = 0.06, y_parameter = 0.00)
    plot$x$layout$margin$t <- 75
    plot$x$layout$margin$l <- 75
    plot <- bold_interactive(plot, panel = TRUE)
    plot <- label_fix(plot = plot) %>% layout(height = 525)
  })

  output$analysisPlot_2 <- renderPlotly({
    plots <- interactive_plots()
    plot <- plots$plots$bar
    plot <- bold_interactive(plot, panel = FALSE)
    label_fix(plot = ggplotly(plot)) %>% layout(height = 525)
  })


  output$analysisPlot_3 <- renderPlotly({
    req(pre_plot_input())
    plots <- interactive_plots()
    tmp <- ggplotly(plots$plots$group_line)
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
    tmp <- bold_interactive(tmp, panel = FALSE)
    label_fix(plot = ggplotly(tmp)) %>% layout(height = 600)
  })


  output$analysisPlot_4 <- renderPlotly({
    plots <- interactive_plots()
    plot <- label_fix(ggplotly(plots$plots$sub_line))
    plot$x$layout$margin$t <- 75
    plot$x$layout$margin$l <- 75
    plot <- ylab_move(plot = plot, x_parameter = 0.06, y_parameter = 0.00)
    plot <- bold_interactive(plot, panel = TRUE)
    label_fix(plot = ggplotly(plot)) %>% layout(height = 525)
  })


  output$analysisPanel <- renderUI({
    data <- pre_modeling_output()
    req(data)
    treatmentPlotSelectors <- levels(data$transformed_data$TreatmentNew)
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
        ),
        numericInput(ns("num_rows"), label = "Number of Rows for Panel Plots", 
                     value = 1, min = 1, max = length(treatmentPlotSelectors), step = 1)
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
            plotlyOutput(ns(x), height = ifelse(x == "analysisPlot_3", "600px", "525px"))
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
