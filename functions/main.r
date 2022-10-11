#' @export
ui <- function(router) {
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
  addResourcePath("loaders", "./www/images/loaders")
  addResourcePath("scripts", "./www/scripts")
  tags$body(
    tags$meta(charset = "utf-8"),
    tags$meta(`http-equiv` = "X-UA-Compatible", content = "IE=edge"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    style = "max-height: 100vh; overflow-y: auto;",
    useShinyjs(),
    extendShinyjs("scripts/fullscreen.js", functions = "fullScreen"),
    extendShinyjs("scripts/cookies.js", functions = c("setCookie", "removeCookie", "getCookie")),
    includeCSS("./www/styles.css"),
    includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    includeScript("./www/scripts/enter.js"),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"
    ),
    router$ui
  )
}

#' @export
server <- function(router) {
  function(input, output, session) {
    box::use(. / app)
    box::use(. / hub)
    box::use(. / nfl)
    # box::use(shinymanager[secure_server, check_credentials])

    # credentials <- data.frame(
    #   user = c("", "shinymanager"), # mandatory
    #   password = c("", "12345"), # mandatory
    #   stringsAsFactors = FALSE
    # )

    # res_auth <- secure_server(
    #   keep_token = TRUE,
    #   check_credentials = check_credentials(credentials)
    # )

    router$server(input, output, session)
    app$server_app("app")
    hub$server_hub("hub")
    nfl$server_pigskin_analytics("pigskin_analytics")
  }
}


#' @export
start <- function() {
  box::use(shiny[runApp, shinyApp, tags])
  box::use(. / main[ui, server])
  box::use(. / hub)
  box::use(shiny.router[make_router, route, page404, route_link])
  box::use(. / app)
  box::use(. / nfl)
  # box::use(shinymanager[secure_app, auth_ui, secure_server, check_credentials])
  router <- make_router(
    route("home", app$ui_app(id = "app")),
    route("hub", hub$ui_hub("hub")),
    route("pigskin", nfl$ui_pigskin_analytics("pigskin_analytics")),
    page_404 = page404(message404 = "ABC")
  )

  ui <- ui(router)
  # ui <- secure_app(ui, enable_admin = TRUE)
  shinyApp(ui, server(router))
}


#' @export
main <- function() {
  box::use(. / main)
  box::use(shiny[runApp])
  if (interactive()) {
    options(shiny.host = "127.0.0.1")
    options(shiny.port = 8000)
  }
  runApp(
    main$start(),
    port = getOption("shiny.port"),
    host = getOption("shiny.host")
  )
}
