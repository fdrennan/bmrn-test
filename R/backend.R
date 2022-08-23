#' @export
ui_backend <- function(id='backend') {
  ns <- NS(id)
  fluidRow(
    box(width=12, dataTableOutput(ns('table'))),
    box(sidth=12, textOutput(ns('uuid')))
  )
}

#' @export
server_backend <- function(id='backend') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      data <- reactive({
        con <- connect_table()
        data <- tbl(con, "sessions") %>%
          arrange(desc(timestamp)) %>%
          collect()
        
        data |> 
          select(timestamp, description, email, statistician, sessionMode, studyId, uuid) |> 
          mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01'))
      })
      
      output$table <- renderDataTable({
        data()
      })
      
      output$sessions <- renderUI({
        textInput(ns('uuid'), 'UUID')
      })
    }
  )
}