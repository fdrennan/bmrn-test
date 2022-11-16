#' ui_analysis_a_report
#' @export
ui_analysis_a_report <- function(id = "analysis_a_report", session_id = "") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$div(
    class = "row",
    shiny$div(class = "col-xl-4 col-lg-4 col-md-4"),
    shiny$div(
      class = "col-xl-4 col-lg-4 col-md-4 d-flex justify-content-center",
      shiny$actionButton(ns("runReport"), "Run Report", class = "btn btn-primary")
    )
  )
}

#' server_analysis_a_report
#' @export
server_analysis_a_report <- function(id = "analysis_a_report", server_input) {
  box::use(shiny)
  box::use(. / connect_table)
  box::use(dplyr)
  box::use(fs)
  box::use(rmarkdown)
  box::use(gt)
  box::use(. / send_email)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      shiny$req(server_input)
      ns <- session$ns
      data <- shiny$reactive({
        con <- connect_table$connect_table()
        data <- dplyr$tbl(con, "sessions")
        # browser()
        data <- dplyr$arrange(data, desc(timestamp))
        data <- dplyr$first(data)
        data <- dplyr$collect(data)
      })

      output$emailUI <- dplyr$renderUI({
        dplyr$req(server_input)
        data <- dplyr$req(data())
        email <- data$email
        dplyr$selectizeInput(ns("repemail"), "Email", selected = email, choices = email, options = list(create = TRUE), multiple = TRUE)
      })

      dplyr$observeEvent(input$runReport, {
        dplyr$showNotification("Making Report")
        data <- dplyr$req(data())
        name <- data$name
        uuid <- data$uuid
        email <- data$email
        studyName <- data$studyName
        statistician <- data$statistician
        studyTitle <- data$studyTitle
        uuid <- data$uuid
        full_path_files <- data$full_path_files
        files <- as.character(fs$dir_ls(full_path_files))
        email_message <-
          as.character(
            gt$html(
              as.character(shiny$div(
                shiny$p(paste0("Dear ", name, ",")),
                shiny$p(
                  paste0("A statistical report has been generated for study ", studyName, " by the TEST 1 Application. ")
                ),
                shiny$p(paste0("The test analysis id is ", uuid)),
                shiny$p(
                  paste0("If you have any questions please contact Cheng Su."),
                ),
                shiny$div("TEST TEAM"),
                shiny$div("Quantitative Science, Data Science"),
                shiny$div("WWRD")
              ))
            )
          )
        tryCatch(
          {
            rmarkdown$render(
              "Test_Report.Rmd",
              params = list(
                uuid = uuid,
                title = studyTitle
              )
            )
          },
          error = function(err) {
            shiny$showNotification(as.character(err), duration = NULL, closeButton = TRUE)
          }
        )

        tryCatch(
          {
            shiny$showNotification("Generating Email", duration = NULL, closeButton = TRUE)
            send_email$send_email(all_files = TRUE, to = email, files = files, email_message = email_message)
            # send_email(all_files = FALSE, to = email, files = files, email_message = email_message)

            if (!getOption("send")) {
              shiny$showNotification("Using development settings, not sending email")
            } else {
              shiny$showNotification("Report Sent",
                duration = NULL, closeButton = TRUE
              )
            }
          },
          error = function(err) {
            shiny$showNotification(as.character(err),
              duration = 10, closeButton = TRUE
            )
            shiny$showNotification("Report failed to email")
          }
        )
      })
    }
  )
}
