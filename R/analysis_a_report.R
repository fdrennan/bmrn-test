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

      observeEvent(input$runReport, {
        showNotification("Making Report")

        data <- req(data())
        uuid <- data$uuid
        email <- data$email
        tryCatch(
          {
            rmarkdown::render(
              "Test_Report.Rmd",
              params = list(
                uuid = uuid
              )
            )
          },
          error = function(err) {
            showNotification(as.character(err), duration = NULL, closeButton = TRUE)
          }
        )

        tryCatch(
          {
            browser()
            send.mail(
              from = Sys.getenv("EMAIL_USER"),
              to = email,
              subject = "Report Generated",
              body = "<html><h1>TEST Results</h1><h2></h2></html>",
              html = TRUE,
              smtp = list(host.name = "mail.bmrn.com", user.name = Sys.getenv("EMAIL_USER"), passwd = Sys.getenv("EMAIL_PASSWORD")),
              attach.files = c("Test_Report.docx"),
              authenticate = TRUE,
              send = getOption("send")
            )

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
