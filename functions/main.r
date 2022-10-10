#' @export
ui <- function(router) {
  function() {{
    box::use(
      shiny[addResourcePath, HTML, tags, div, fluidPage, column, fluidRow, includeCSS, includeScript],
      shinyjs[useShinyjs, extendShinyjs],
      . / reddit,
      . / offcanvas,
      . / button,
      esquisse,
      . / utilities / datatable
    )
    box::use(shiny[tags, actionButton, icon])
  }
  addResourcePath("loaders", "./www/images/loaders")
  tags$body(
    HTML('<meta charset="utf-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1">'),
    style = "max-height: 100vh; overflow-y: auto;",
    useShinyjs(),
    extendShinyjs(
      text = paste0(readLines("www/scripts/fullscreen.js"), collapse = "\n"), functions = "fullScreen"
    ),
    includeCSS("./www/styles.css"),
    includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    includeScript("./www/scripts/enter.js"),
    router$ui
  )  }
}

#' @export
server <- function(router) {
  function(input, output, session) {
    box::use(. / app)
    box::use(. / nfl)
    box::use(. / main)
    router$server(input, output, session)
    app$app_server(id = "app")
    nfl$server_pigskin_analytics("pigskin_analytics")
    main$server_hub("hub")
  }
}


#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  ns <- shiny$NS(id)
  shiny$uiOutput(ns("ui"))
}

#' @export
server_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[div])
  box::use(shinyauthr)
  box::use(tibble)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      user_base <- tibble$tibble(
        user = c("user1", "user2"),
        password = c("pass1", "pass2"),
        permissions = c("admin", "standard"),
        name = c("User One", "User Two")
      )

      output$ui <- shiny$renderUI({
        div(
          div(class = "pull-right", shinyauthr$logoutUI(id = ns("logout"))),
          # add login panel UI function
          shinyauthr$loginUI(id = ns("login"))
        )
      })
    }
  )
}

#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp])
  box::use(. / main[ui, server])
  box::use(. / main[ui_hub, server_hub])
  box::use(shiny.router[make_router, route, page404])
  box::use(. / app)
  box::use(. / nfl)
  box::use(. / main)
  # dataframe that holds usernames, passwords and other user data
  router <- make_router(
    route("", app$app_ui(id = "app")),
    route("hub", main$ui_hub("hub")),
    route("pigskin", nfl$ui_pigskin_analytics("pigskin_analytics")),
    page_404 = page404(message404 = "ABC")
  )

  shinyApp(ui(router), server(router))
}
