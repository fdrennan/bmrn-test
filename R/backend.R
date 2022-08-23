#' @export
ui_backend <- function(id='backend') {
  ns <- NS(id)
  fluidRow(
    box(width=12, dataTableOutput(ns('table'))),
    box(sidth=12, uiOutput(ns('sessions'))),
    box(sidth=12, uiOutput(ns('files')))
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
      
      output$files <- renderDataTable({
        req(input$uuid)
        data <- data() |> 
          filter(uuid==input$uuid)
        dir_info(data$full_path_files)
      })
    }
  )
}