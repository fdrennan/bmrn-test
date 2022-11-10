#' @export
ui_download_historical <- function(id = "download_historical", container = function(...) {
                                     shiny::column(12, ...)
                                   }) {
  box::use(shiny)
  ns <- shiny$NS(id)
  print(ns("ui"))
  container(
    shiny$uiOutput(ns("ui"), container = shiny$fluidRow),
    shiny$uiOutput(ns("results"), container = shiny$fluidRow)
  )
}

#' @export
server_download_historical <- function(id = "download_historical") {
  box::use(shiny)
  box::use(stringr)
  box::use(purrr)
  box::use(fs)
  box::use(zip)
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
        sessions_run <- fs$dir_ls("test_output", recurse = T, type = "file")
        sessions_run <- sessions_run[stringr$str_detect(tolower(sessions_run), tolower(input$search))]
      })
      output$results <- shiny$renderUI({
        shiny$req(searchResults())
        shiny$fluidRow(
          shiny$downloadButton(ns("download"), "Download"),
          purrr$map(searchResults(), shiny$tags$pre)
        )
      })


      output$download <- shiny$downloadHandler(
        filename = function() {
          "searchResults.zip"
        },
        content = function(file) {
          zip$zip(file, searchResults())
        }
      )
    }
  )
}
