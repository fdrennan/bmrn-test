#' analysis_a_session_setup
#' @export
analysis_a_session_setup <- function(id = "analysis_a_session_setup", user, is_admin) {
  ns <- NS(id)
  section_1 <- box(
    width = 12,
    title = h4("Study Information"),
    textInput(ns("name"), "Name"),
    textInput(ns("email"), "Email"),
    tooltip(
      selectizeInput(ns("statistician"),
        div(
          class = "d-flex justify-content-between",
          "Contact Statistician",
          icon("info-circle")
        ),
        choices = c("Cheng Su", "Other"),
      ),
      title = "for support and review of analysis"
    ),
    selectizeInput(
      ns("department"), "Therapeutic Areas",
      choices = c(
        "Cardiovascular", "Central Nervous System",
        "Musculoskeletal",
        "Hematology",
        "Other"
      )
    ),
    selectizeInput(
      ns("program"), "Program (select or type)",
      options = list(create = TRUE),
      choices = c("Exploratory", unique(program_lists$Program)[-1])
    ),
    uiOutput(ns("selectizeInput")),
    textInput(ns("studyId"), "Study ID", "TB21-02"),
    textInput(ns("studyTitle"), "Study Title")
  )

  section_2 <- box(
    width = 12,
    title = h4("Study Description"),
    tooltip(
      selectizeInput(
        ns("sessionMode"), div(
          class = "d-flex justify-content-between",
          "Objective", icon("info-circle")
        ),
        # options = list(create = TRUE),
        choices = c("Exploratory", "Confirmatory"), selected = "Exploratory"
      ),
      title =
        "Exploratory: for early (or first) studies to explore dose and time points\n\nConfirmatory: to confirm treatment effect at a particular time point"
    ),
    textAreaInput(ns("description"),
      "Please give research objectives and experiment details",
      height = "300px"
    ),
    fileInput(
      placeholder = "",
      inputId = ns("upload"),
      label = h6("Upload supporting study documents."),
      multiple = TRUE,
      accept = "*"
    )
  )

  section_3 <- box(
    width = 12,
    title = h4("Upload Study Data"),
    fileInput(
      placeholder = NULL,
      inputId = ns("file"),
      label = NULL,
      accept = ".xlsx", multiple = FALSE
    ),
    downloadLink(
      ns("template"),
      h6("Download Data Template", class = "text-right text-underline")
    )
  )

  fluidRow(
    column(4, offset = 2, section_1),
    column(
      4, section_2,
      section_3,
      div(
        class = "d-flex justify-content-end",
        actionButton(ns("submitForm"), h6(
          "Submit"
        ), class = "btn-primary")
      )
    )
  )
  
}


#' analysis_a_session_setup_server
#' @export
analysis_a_session_setup_server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("name", sv_required())
  iv$add_rule("email", sv_required())
  iv$add_rule("email", sv_email())
  iv$add_rule("description", sv_required())
  iv$add_rule("studyTitle", sv_required())
  iv$enable()

  ns <- session$ns
  output$selectizeInput <- renderUI({
    load("data/program_lists.rda")
    x <- input$program
    print(input$program)
    ns <- session$ns
    print(paste0("NS Server is ", ns("project")))
    project <- unique(filter(program_lists, Program == input$program)$Project)
    selectizeInput(
      {
        print(paste0("NSUI is ", ns("project")))
        ns("project")
      },
      "Project (select or type)",
      options = list(create = TRUE),
      choices = project
    )
  })


  output$template <- downloadHandler(
    contentType='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
    filename = function() {
      "test_example.xlsx"
    },
    content = function(con) {
      file_copy('test_example.xlsx', con)
    }
  )


  out <- eventReactive(input$submitForm, {
    showNotification("Building analysis...", id = "setupnotification")
    if (!iv$is_valid()) {
      showNotification("Please complete all required fields.")
    }
    if (getOption("require_validation")) {
      req(iv$is_valid())
    }
    input <- reactiveValuesToList(input)
    input$uuid <- UUIDgenerate()
    input$project <- input$project %>% to_snake_case()

    rel_path_home <- path_join(c("test_output", input$program, input$project, input$studyId, input$uuid))
    base_dir <- path_abs(Sys.getenv("BASE_DIR"))
    full_path_home <- path_join(c(base_dir, rel_path_home))
    full_path_files <- path_join(c(full_path_home, "files"))

    df <- tibble(
      base_dir = base_dir,
      rel_path_home = rel_path_home,
      full_path_home = full_path_home,
      full_path_files = full_path_files
    )
    
    browser()
    if (length(input$upload$datapath)) {
      copy_files(df, input$upload)
    }

    if (length(input$file$datapath)) {
      input_data <- clean_excel_data(input$file)
      copy_files(df, input$file)
    }

    input$submitForm <- NULL
    input <- as_tibble(purrr::keep(input, ~ length(.) == 1))
    df <- bind_cols(input, df)
    df$timestamp <- with_tz(Sys.time(), "PST")
    con <- connect_table()
    on.exit(dbDisconnect(con))
    if (!dbExistsTable(con, "sessions")) {
      dbCreateTable(con, "sessions", df)
    }
    dbAppendTable(con, "sessions", df)

    change_page("analysisa_run")
    removeNotification(id = "setupnotification")

    write_csv(input_data$data, fs::path_join(c(df$full_path_files, "input_data.csv")))

    list(
      session_data = df,
      input_data = input_data
    )
  })

  out
}


#' copy_files
#' @export copy_files
copy_files <- function(df, upload) {
  full_path_files <- df$full_path_files
  name <- upload$name
  new_path <- paste0(full_path_files, "/", upload$name)
  dir_create(path_dir(new_path), recurse = T)
  walk2(
    upload$datapath, new_path, function(old, new) {
      cli_alert_info(old)
      cli_alert_info(new)
      file_move(old, new)
    }
  )
}
