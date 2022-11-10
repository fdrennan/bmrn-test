#' test_session_setup
#' @export
test_session_setup <- function(id = "test_session_setup") {
  box::use(shiny)
  box::use(bs4Dash)
  box::use(readr)
  load('./data/program_lists.rda')
  ns <- shiny$NS(id)
  section_1 <- bs4Dash$box(
    width = 12,
    title = shiny$h4("Study Information"),
    shiny$textInput(ns("name"), "Name"),
    shiny$textInput(ns("email"), "Email"),
    bs4Dash$tooltip(
      shiny$selectizeInput(ns("statistician"),
        shiny$div(
          class = "d-flex justify-content-between","Contact Statistician",
          shiny$icon("info-circle")
        ),
        choices = c("Cheng Su", "Other")
      ),
      title = "for support and review of analysis"
    ),
    shiny$selectizeInput(
      ns("department"), "Therapeutic Areas",
      choices = c(
        "Cardiovascular", "Central Nervous System",
        "Musculoskeletal",
        "Hematology",
        "Other"
      )
    ),
    shiny$selectizeInput(
      ns("program"), "Program (select or type)",
      options = list(create = TRUE),
      choices = unique(program_lists$Program)
    ),
    shiny$uiOutput(ns("selectizeInput")),
    shiny$textInput(ns("studyTitle"), "Study Title"),
    shiny$textInput(ns("studyId"), "Study ID", "TB21-02")
  )

  section_2 <- bs4Dash$box(
    width = 12,
    title = shiny$h4("Study Description"),
    bs4Dash$tooltip(
      shiny$selectizeInput(
        ns("sessionMode"), shiny$div(
          class = "d-flex justify-content-between",
          "Objective", shiny$icon("info-circle")
        ),
        # options = list(create = TRUE),
        choices = c("Exploratory", "Confirmatory"), selected = "Exploratory"
      ),
      title = "TBD by Monika"
    ),
    shiny$textAreaInput(ns("description"),
      "Please give research objectives and experiment details",
      height = "300px"
    ),
    shiny$fileInput(
      placeholder = "",
      inputId = ns("upload"),
      label = shiny$h6("Upload supporting study documents."),
      multiple = TRUE,
      accept = "*"
    )
  )

  section_3 <- bs4Dash$box(
    width = 12,
    title = shiny$h4("Upload Study Data"),
    shiny$fileInput(
      placeholder = NULL,
      inputId = ns("file"),
      label = NULL,
      accept = ".xlsx", multiple = FALSE
    ),
    shiny$downloadLink(
      ns("template"),
      shiny$h6("Download Data Template", class = "text-right text-underline")
    )
  )

  shiny$fluidRow(
    shiny$column(4, offset = 2, section_1),
    shiny$column(4,
      section_2,
      section_3,
      shiny$div(
        class = "d-flex justify-content-end",
        shiny$actionButton(ns("submitForm"),
                           shiny$h6("Submit"),
                           class = "btn-primary")
      )
    )
  )
}


#' test_session_setup_server
#' @export
test_session_setup_server <- function(id) {
  {
    box::use(shiny)
    box::use(dplyr)
    box::use(shinyvalidate)
    box::use(writexl)
    box::use(readxl)
    box::use(snakecase)
    box::use(uuid)
    box::use(fs)
    box::use(tibble)
    box::use(lubridate)
    box::use(purrr)
    box::use(. / clean_excel_data)
  }
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      iv <- shinyvalidate$InputValidator$new()
      iv$add_rule("name", shinyvalidate$sv_required())
      iv$add_rule("email", shinyvalidate$sv_required())
      iv$add_rule("email", shinyvalidate$sv_email())
      iv$add_rule("description", shinyvalidate$sv_required())
      iv$enable()

      output$selectizeInput <- shiny$renderUI({
        load("data/program_lists.rda")
        x <- input$program
        ns <- session$ns
        project <- unique(dplyr$filter(program_lists, Program == input$program)$Project)
        dplyr$selectizeInput(ns("project"),
          "Project (select or type)",
          options = list(create = TRUE),
          choices = project
        )
      })


      output$template <- dplyr$downloadHandler(
        filename = function() {
          "test_example.xlsx"
        },
        content = function(con) {
          writexl$write_xlsx(
            readxl$read_xlsx("inputs/test_example_baseline_template_v2_trans_replicates_trend_orig_names.xlsx"), con
          )
        }
      )


      out <- shiny$eventReactive(input$submitForm, {
        shiny$showNotification("Building analysis...", id = "setupnotification")
        if (!iv$is_valid()) {
          shiny$showNotification("Please complete all required fields.")
        }
        if (getOption("require_validation")) {
          shiny$req(iv$is_valid())
        }
        input <- shiny$reactiveValuesToList(input)
        input$uuid <- uuid$UUIDgenerate()
        input$project <- input$project %>% snakecase$to_snake_case()

        rel_path_home <- fs$path_join(c("test_output", input$program, input$project, input$studyId, input$uuid))
        base_dir <- fs$path_abs(Sys.getenv("BASE_DIR"))
        full_path_home <- fs$path_join(c(base_dir, rel_path_home))
        full_path_files <- fs$path_join(c(full_path_home, "files"))

        df <- tibble$tibble(
          base_dir = base_dir,
          rel_path_home = rel_path_home,
          full_path_home = full_path_home,
          full_path_files = full_path_files
        )

        if (length(input$upload$datapath)) {
          fs$copy_files(df, input$upload)
        }

        if (length(input$file$datapath)) {
          input_data <- tryCatch(
            {
              clean_excel_data$clean_excel_data(input$file)
            },
            error = function(err) {
              shiny$showModal(shiny$modalDialog(
                shiny$tags$pre(as.character(err))
              ))
              shiny$req(FALSE)
            }
          )
          fs$copy_files(df, input$file)
        }
        box::use(. / connect_table)
        box::use(DBI)
        input$submitForm <- NULL
        input <- tibble$as_tibble(purrr$keep(input, ~ length(.) == 1))
        df <- dplyr$bind_cols(input, df)
        df$timestamp <- lubridate$with_tz(Sys.time(), "PST")
        con <- connect_table$connect_table()
        on.exit(DBI$dbDisconnect(con))
        if (!DBI$dbExistsTable(con, "sessions")) {
          DBI$dbCreateTable(con, "sessions", df)
        }
        DBI$dbAppendTable(con, "sessions", df)
        shiny$removeNotification(id = "setupnotification")
        list(
          session_data = df,
          input_data = input_data
        )
      })

      out
    }
  )
}

#' copy_files
#' @export copy_files
copy_files <- function(df, upload) {
  box::use(fs[dir_create, path_dir])
  box::use(purrr[walk2])
  box::use(cli[cli_alert_info])
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
