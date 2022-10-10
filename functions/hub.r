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
          shiny$h1("Hello"),
          div(class = "pull-right", shinyauthr$logoutUI(id = ns("logout"))),
          # add login panel UI function
          shinyauthr$loginUI(id = ns("login"))
        )
      })

      # call login module supplying data frame,
      # user and password cols and reactive trigger
      credentials <- shinyauthr$loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        log_out = shiny$reactive(logout_init())
      )

      # call the logout module with reactive trigger to hide/show
      logout_init <- shinyauthr$logoutServer(
        id = "logout",
        active = shiny$reactive(credentials()$user_auth)
      )

      output$user_table <- shiny$renderTable({
        # use req to only render results when credentials()$user_auth is TRUE
        shiny$req(credentials()$user_auth)
        credentials()$info
      })
    }
  )
}
