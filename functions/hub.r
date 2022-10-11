#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[tags])
  ns <- shiny$NS(id)
  shiny$column(
    12,
    shiny$fluidRow(
      shiny$uiOutput(ns("preinputs")),
      shiny$uiOutput(ns("app"))
    )
  )
}

#' @export
server_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[div, observeEvent])
  box::use(tibble)
  box::use(dplyr)
  box::use(readr[read_csv])
  box::use(shinyjs[js])
  box::use(. / hub)
  box::use(janitor[clean_names])
  box::use(dplyr[group_by, count, glimpse, distinct, pull])

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      dataIn <- shiny$reactive({
        usaa_data <- {
          usaa_data <- read_csv("~/Downloads/bk_download.csv") |>
            dplyr$mutate(Date = as.character(as.Date(Date, origin = "1970-01-01")))
        }

        usaaAccounts <- hub$read_sheets()

        out <- list(usaa_data = usaa_data, usaaAccounts = hub$read_sheets())

        out <- lapply(out, function(x) {
          x <- clean_names(x)
          glimpse(x)
          x
        })

        out
      })

      output$usaaMetadata <- shiny$renderTable({
        dataIn()$usaa_counts
      })


      output$googleDrive <- shiny$renderTable({
        dataIn()$usaa_data
      })


      output$usaaAccounts <- shiny$renderTable({
        dataIn()$usaaAccounts
      })

      output$preinputs <- shiny$renderUI({
        shiny$req(dataIn())
        dataIn <- dataIn()
        # browser()
        daterange <- range(dataIn$usaa_data$date)
        shiny$dateRangeInput(ns("daterange"), "Date range:",
          start = daterange[[1]],
          end = daterange[[2]],
          min = daterange[[1]],
          max = daterange[[2]],
          format = "mm/dd/yy",
          separator = " - "
        )
      })
      output$inputs <- shiny$renderUI({
        shiny$req(dataIn())
        dataIn <- dataIn()
        # browser()
        category <-
          dataIn$usaa_data |>
          group_by(category) |>
          count(sort = TRUE) |>
          distinct(category) |>
          pull(category)

        shiny$fluidRow(
          shiny$column(
            12, shiny$h1("Inputs"),
            class = "hw-100",
            shiny$selectizeInput(
              ns("categories"), "Categories", category, category,
              multiple = TRUE
            )
          )
        )
      })

      output$app <- shiny$renderUI({
        shiny$column(12,
          id = ns("maximize"),
          shiny$uiOutput(ns("inputs")),
          shiny$div(style = "max-height: 40vh; overflow-y: auto;", shiny$tableOutput(ns("usaaMetadata"))),
          shiny$div(style = "max-height: 40vh; overflow-y: auto;", shiny$tableOutput(ns("googleDrive"))),
          shiny$div(style = "max-height: 40vh; overflow-y: auto;", shiny$tableOutput(ns("usaaAccounts")))
        )
      })
    }
  )
}

#' @export
read_sheets <- function() {
  box::use(googlesheets4)
  sheet <- googlesheets4$read_sheet(getOption("billspage"))
}

#' @export
selenium <- function() {
  box::use(RSelenium)



  # remDr <- RSelenium$remoteDriver(
  #   remoteServerAddr = getOption('localhost'),
  #   port = 4445L
  # )
  #
  # open <- remDr$open()
  # on.exit(remDr$close())
  # browseURL('https://docs.google.com/spreadsheets/d/1R0IcmbMR0B8xuNrJb90d5bJErML_TgxP19O-q3HXw1k/edit#gid=0')
  # url <- "https://www.usaa.com/my/checking/"
  # browseURL(url)
  # library(dplyr)
  # library(ggplot2)
  # data <- read.csv('~/Downloads/bk_download.csv')
  # data |>
  #   filter(Date >= Sys.Date() - 30) |>
  #   group_by(Date, Category) |>
  #   summarise(Amount = sum(Amount)) |>
  #   # ungroup() +
  #   # mutate(Amount = cumsum(Amount)) |>
  #   ggplot() +
  #   aes(x = Date, y = Amount) +
  #   geom_col() +
  #   facet_wrap(~ Category, scales = 'free_y')
  #   # facet_wrap(~ Category, scales = 'free_y')

  # remDr$screenshot(display = T)
  # memberId <- remDr$findElement(using = 'xpath', value = "//input[@name='memberId']")
  # remDr$screenshot(display = T)
  # memberId$sendKeysToElement(list('freddy.drennan'))
  # submit <- remDr$findElement(using='xpath', value="//button[@type='submit']")
  # submit$sendKeysToElement(list(key='enter'))
  # memberId$screenshot(display = T)
  # # browser()
}

# selenium()
