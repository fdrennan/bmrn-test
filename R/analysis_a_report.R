#' ui_analysis_a_report
#' @export
ui_analysis_a_report <- function(id = "analysis_a_report", session_id = "") {
  ns <- NS(id)
  div(
    class = "row",
    div(class = "col-xl-4 col-lg-4 col-md-4"),
    div(
      class = "col-xl-4 col-lg-4 col-md-4 d-flex justify-content-center",
      actionButton(ns("runReport"), "Run Report", class = "btn btn-primary")
    )
  )
}

#' server_analysis_a_report
#' @export
server_analysis_a_report <- function(id = "analysis_a_report", server_input) {
  moduleServer(
    id,
    function(input, output, session) {
      req(server_input)
      ns <- session$ns
      data <- reactive({
        con <- connect_table()
        data <- tbl(con, "sessions") %>%
          arrange(desc(timestamp)) %>%
          first() %>%
          collect()
      })

      output$emailUI <- renderUI({
        req(server_input)
        data <- req(data())
        email <- data$email
        selectizeInput(ns("repemail"), "Email", selected = email, choices = email, options = list(create = TRUE), multiple = TRUE)
      })

      observeEvent(input$runReport, {
        showNotification("Making Report")
        data <- req(data())
        name <- data$name
        uuid <- data$uuid
        email <- data$email
        studyName <- data$studyName
        statistician <- data$statistician
        studyTitle <- data$studyTitle
        uuid <- data$uuid
        full_path_files <- data$full_path_files
        files <- as.character(dir_ls(full_path_files))
        email_message <-
          as.character(
            html(
              as.character(div(
                p(paste0("Dear ", name, ",")),
                p(
                  paste0("A statistical report has been generated for study ", studyName, " by the TEST 1 Application. ")
                ),
                p(paste0("The test analysis id is ", uuid)),
                p(
                  paste0("If you have any questions please contact Cheng Su."),
                ),
                div("TEST TEAM"),
                div("Quantitative Science, Data Science"),
                div("WWRD")
              ))
            )
          )
        tryCatch(
          {
            rmarkdown::render(
              "Test_Report.Rmd",
              params = list(
                uuid = uuid,
                title = studyTitle
              )
            )
          },
          error = function(err) {
            showNotification(as.character(err), duration = NULL, closeButton = TRUE)
          }
        )

        tryCatch(
          {
            send_email(all_files = TRUE, to = email, files = files, email_message = email_message)
            # send_email(all_files = FALSE, to = email, files = files, email_message = email_message)

            if (!getOption("send")) {
              showNotification("Using development settings, not sending email")
            } else {
              showNotification("Report Sent")
            }
          },
          error = function(err) {
            showNotification(as.character(err), duration = NULL, closeButton = TRUE)
            showNotification("Report failed to email")
          }
        )
      })
    }
  )
}
