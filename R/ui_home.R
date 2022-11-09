#' ui_home
#' @export
ui_home <- function(id = "home") {
  box::use(shiny)
  ns <- shiny$NS(id)
  base <- Sys.getenv("BASE_DOMAIN")
  shiny$uiOutput(ns("testLocations"), container = function(...) {
    shiny$fluidRow(..., class = "m-5")
  })
}

#' server_home
#' @export
server_home <- function(id = "home") {
  box::use(shiny)
  box::use(shiny[tags])
  box::use(./ui_landing)
  box::use(purrr)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # observe({
      ui_landing$server_landing("landing")
      shiny$showModal(shiny$modalDialog(
        style = "height: 500px;",
        ui_landing$ui_landing(ns("landing")),
        easyClose = TRUE,
        title = NULL,
        footer = shiny$div(
          class = "text-right",
          "Contact: fr904103@bmrn.com",
          tags$br(),
          getOption("test_version"),
          tags$br(),
          shiny$tags$em("By Quantitative Science, Data Science")
        ),
        size = "xl"
      ))
      output$testLocations <- shiny$renderUI({
        bs4Dash$box(
          title = shiny$h4("TEST 1", class = "display-4"),
          collapsible = FALSE,
          width = 12,
          height = "310px",
          shiny$div(
            class = "h-100",
            shiny$tags$ul(
              class = "h-75",
              class = "px-3",
              purrr$map(c(
                "Single Treatment with Multiple Doses",
                "Multiple Time Points",
                "Controls and Comparators"
              ), ~ tags$li(shiny$h4(.)))
            ),
            shiny$div(
              class = "text-center",
              shiny$actionButton(
                inputId = ns("analysisaGo"), shiny$h6("Start", class = "display-6 m-0 p-0"),
                class = "btn btn-primary pb-2"
              )
            )
          )
        )
      })
      shiny$observeEvent(input$analysisaGo, {
        box::use(shiny.router[change_page])
        change_page("analysisasetup")
      })
    }
  )
}
