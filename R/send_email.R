#' @export
send_email <- function(all_files = FALSE,
                       files,
                       from = getOption("EMAIL_USER"),
                       to = getOption("EMAIL_USER"), 
                       email_message = div("No message attached")) {
  if (all_files) {
    files <- c("Test_Report.docx", files)
    zip_name <- "test1output.zip"
    zip(zip_name, files, flags = "-r9Xj")
    file_name <- zip_name
  } else if (is.null(files)) {
    file_name <- NULL
  } else {
    file_name <- "Test_Report.docx"
  }


  mailR::send.mail(
    from = from,
    to = to,
    subject = "Report Generated",
    body = email_message,
    html = TRUE,
    smtp = list(
      host.name = "mail.bmrn.com",
      user.name = getOption("EMAIL_USER"),
      passwd = getOption("EMAIL_PASSWORD")
    ),
    attach.files = file_name,
    authenticate = TRUE,
    send = getOption("send")
  )
}
