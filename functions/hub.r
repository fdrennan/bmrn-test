#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[tags, column])
  ns <- shiny$NS(id)
  shiny$column(
    12,
    shiny$fluidRow(
      class = "p-2",
      shiny$uiOutput(ns("preinputs"), container = function(...) {
        column(12, ...)
      }),
      shiny$uiOutput(ns("app"), container = function(...) {
        column(12, ...)
      })
    )
  )
}

#' @export
server_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[div, observeEvent, column])
  box::use(tibble)
  box::use(dplyr)
  box::use(readr[read_csv])
  box::use(shinyjs[js])
  box::use(. / hub)
  box::use(janitor[clean_names])
  box::use(dplyr[group_by, arrange, mutate, count, summarise, glimpse, distinct, pull])

  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      dataIn <- shiny$reactive({
        out
      })

      output$preinputs <- shiny$renderUI({
        shiny$req(dataIn())
        dataIn <- dataIn()
        daterange <- range(dataIn$usaa_data$date)
        shiny$div(
          shiny$dateRangeInput(ns("daterange"), "Date range:",
            start = Sys.Date() - 3,
            end = daterange[[2]],
            min = daterange[[1]],
            max = daterange[[2]],
            format = "mm/dd/yy",
            separator = " - ", width = "100%"
          ),
          shiny$checkboxInput(ns("selectAll"), "Select All", TRUE)
        )
      })

      dataFiltered <- shiny$reactive({
        shiny$req(dataIn())
        shiny$req(input$daterange)
        dataIn <- dataIn()
        dataIn$usaa_data <- dplyr$filter(dataIn$usaa_data, dplyr$between(as.Date(date), input$daterange[[1]], input$daterange[[2]]))
        dataIn
      })

      output$inputs <- shiny$renderUI({
        shiny$req(dataFiltered())
        dataFiltered <- dataFiltered()

        category <-
          dataFiltered$usaa_data |>
          group_by(category) |>
          count(sort = TRUE) |>
          distinct(category) |>
          pull(category)


        shiny$selectizeInput(
          ns("categories"), "Categories",
          category,
          if (input$selectAll) category else NULL,
          multiple = TRUE, width = "100%"
        )
      })

      dataDoubleFiltered <- shiny$reactive({
        shiny$req(dataFiltered())
        shiny$req(input$categories)
        dataFiltered <- dataFiltered()
        dataFiltered$usaa_data <- dplyr$filter(
          dataFiltered$usaa_data, category %in% input$categories
        )
        saveRDS(dataFiltered, "dataFiltered.rda")
        dataFiltered
      })


      output$trend <- shiny$renderPlot({
        box::use(ggplot2, forcats)
        shiny$req(dataDoubleFiltered())
        usaa_data <- dataDoubleFiltered()$usaa_data
        usaa_data |>
          group_by(date, description) |>
          summarise(amount = sum(amount)) |>
          arrange(amount) |>
          mutate(description = forcats$as_factor(description)) |>
          ggplot2$ggplot() +
          ggplot2$aes(x = date, y = amount) +
          ggplot2$geom_col() +
          ggplot2$facet_wrap(description ~ ., scales = "free_y")
      })



      output$usaa_data <- shiny$renderTable({
        usaa_data <- dataDoubleFiltered()$usaa_data
        usaa_data |>
          group_by(description) |>
          summarise(amount = sum(amount)) |>
          arrange(amount)
      })


      output$googleDrive <- shiny$renderTable({
        dataDoubleFiltered()$googleDrive
      })


      output$app <- shiny$renderUI({
        shiny$fluidRow(
          id = ns("maximize"),
          shiny$uiOutput(ns("inputs"), container = function(...) {
            column(12, ...)
          }),
          shiny$plotOutput(ns("trend")),
          shiny$fluidRow(
            # class='d-flex justify-content-around',
            shiny$column(6, style = "max-height: 40vh; overflow-y: auto;", shiny$tableOutput(ns("usaa_data"))),
            shiny$column(6, style = "max-height: 40vh; overflow-y: auto;", shiny$tableOutput(ns("googleDrive")))
          )
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
