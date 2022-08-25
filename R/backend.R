#' @export
ui_backend <- function(id = "backend") {
  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      fluidRow(
        actionButton(ns("refresh"), "Refresh"),
        dataTableOutput(ns("table"))
      ),
      collapsible = TRUE,
      collapsed = FALSE,
      closable = TRUE,
      maximizable = TRUE
    ),
    box(width = 12, uiOutput(ns("sessions"))),
    box(width = 12, dataTableOutput(ns("files")))
  )
}

#' @export
server_backend <- function(id = "backend") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      data <- reactive({
        input$refresh
        showNotification("Refreshed backend")
        con <- connect_table()
        data <- tbl(con, "sessions") %>%
          arrange(desc(timestamp)) %>%
          collect()

        data |>
          select(
            timestamp, description, email, statistician, sessionMode,
            full_path_files, studyId, uuid
          ) |>
          mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
      })

      output$table <- renderDataTable({
        data()
      })

      output$sessions <- renderUI({
        textInput(ns("uuid"), "UUID")
      })

      output$files <- renderDataTable({
        req(input$uuid)
        data <- data() |>
          filter(uuid == input$uuid)
        dir_info(data$full_path_files)
      })
    }
  )
}
