#' administration_ui
#' @export administration_ui
administration_ui <- function(id) {
  ns <- NS(id)
  bsCollapse(
    bsCollapsePanel(
      "Administration",
      div(
        dataTableOutput(ns("downloadStudyInformation"))
      )
    )
  )
}

#' administration_server
#' @export administration_server
administration_server <- function(input, output, session,
                                  user, is_admin) {
  ns <- session$ns
  stored_data <- reactive({
    con <- connect_table()
    sessions <-
      tbl(con, "sessions") %>%
      arrange(desc(timestamp)) %>%
      collect() %>%
      mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
    sessions
  })
  output$downloadStudyInformation <- renderDataTable({
    invalidateLater(30000)
    stored_data() %>%
      select(-rel_path_home, -base_dir, -full_path_files) %>%
      select(uuid, everything())
  })
}
