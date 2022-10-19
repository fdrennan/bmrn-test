#' ui_home
#' @export
ui_home <- function(id = "home") {
  box::use(shiny)
  ns <- NS(id)
  # print(ns('ui'))
  base <- Sys.getenv("BASE_DOMAIN")
  uiOutput(ns("testLocations"), container = function(...) {
    shiny$fluidRow(..., class = "m-5")
  })
}

#' server_home
#' @export
server_home <- function(id = "home") {
  box::use(shiny)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      print(ns("server"))
      # observe({
      showModal(modalDialog(
        style = "height: 500px;",
        ui_landing(),
        easyClose = TRUE,
        title = NULL,
        footer = div(
          class = "text-right",
          "Contact: fr904103@bmrn.com",
          tags$br(),
          getOption("test_version"),
          tags$br(),
          em("By Quantitative Science, Data Science")
        ),
        size = "xl"
      ))
      output$testLocations <- renderUI({
        box(
          title = h4("TEST 1", class = "display-4"),
          collapsible = FALSE,
          width = 12,
          height = "310px",
          div(
            class = "h-100",
            tags$ul(
              class = "h-75",
              class = "px-3",
              map(c(
                "Single Treatment with Multiple Doses",
                "Multiple Time Points",
                "Controls and Comparators"
              ), ~ tags$li(h4(.)))
            ),
            div(
              class = "text-center",
              actionButton(
                inputId = ns("analysisaGo"), h6("Start", class = "display-6 m-0 p-0"),
                class = "btn btn-primary pb-2"
              )
            )
          )
        )
      })
      observeEvent(input$analysisaGo, {
          change_page("analysisasetup")
        }
      )
    }
  )
}
