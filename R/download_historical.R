#' @export
ui_download_historical <- function(id = "download_historical", container = function(...) {
                                     column(12, ...)
                                   }) {
  box::use(shiny)
  ns <- shiny$NS(id)
  print(ns("ui"))
  container(
    shiny$uiOutput(ns("ui"), container = fluidRow),
    shiny$uiOutput(ns("results"), container = fluidRow)
  )
}

#' @export
server_download_historical <- function(id = "download_historical") {
  box::use(shiny)
  box::use(stringr)
  box::use(purrr)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      print(ns("server"))
      output$ui <- shiny$renderUI({
        shiny$textInput(ns("search"), "Search")
      })

      searchResults <- shiny$reactive({
        shiny$req(input$search)
        sessions_run <- dir_ls("test_output", recurse = T, type = "file")
        sessions_run <- sessions_run[stringr$str_detect(tolower(sessions_run), tolower(input$search))]
      })
      output$results <- shiny$renderUI({
        shiny$req(searchResults())
        shiny$fluidRow(
          shiny$downloadButton(ns("download"), "Download"),
          purrr$map(searchResults(), shiny$tags$pre)
        )
      })


      output$download <- downloadHandler(
        filename = function() {
          "searchResults.zip"
        },
        content = function(file) {
          zip::zip(file, searchResults())
        }
      )
    }
  )
}
