#' @export
ui_hub <- function(id = "hub") {
  box::use(shiny)
  box::use(shiny[tags, column])
  box::use(. / utilities / datatable)
  box::use(plotly)
  ns <- shiny$NS(id)
  shiny$column(
    12,
    shiny$fluidRow(
      class = "p-2",
      shiny$fluidRow(
        shiny$checkboxInput(ns("selectAll"), "Select All", TRUE),
        shiny$actionButton(ns("updateTables"), "Update Tables", class='btn btn-primary')
      ),
      shiny$uiOutput(ns("preinputs"), container = function(...) {
        column(12, ...)
      }),
      shiny$uiOutput(ns("categoryUI")),
      plotly$plotlyOutput(ns("trend")),
      datatable$ui_dt(ns("recurring"), title = "Recurring"),
      datatable$ui_dt(ns("usaa_data"), title = "Usaa Raw Data", container = function(...) {
        column(6, ..., class = "p-2")
      }),
      datatable$ui_dt(ns("googleDrive"), title = "Google Drive", container = function(...) {
        column(6, ..., class = "p-2")
      }),
      datatable$ui_dt(ns("categorySummary"), container = function(...) {
        column(6, ..., class = "p-2")
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
  box::use(purrr[map])
  box::use(fs)
  box::use(plotly)
  box::use(janitor[clean_names])
  box::use(dplyr[
    group_by, arrange, mutate, count,
    summarise, glimpse, distinct, pull, n, ungroup
  ])
  box::use(. / utilities / datatable)
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny$observeEvent(input$updateTables, {
        js$openWindow("https://usaa.com")
        system("xdg-open ~/Downloads")
        system("xdg-open finances")
      })
      dataIn <- shiny$reactive({
        out <- list(
          usaa_data = hub$read_finances(),
          googleDrive = hub$read_sheets()
        )
        map(out, function(x) {
          dplyr$mutate_if(x, is.character, as.factor)
        })
      })

      output$preinputs <- shiny$renderUI({
        shiny$req(dataIn())
        dataIn <- dataIn()
        daterange <- range(dataIn$usaa_data$date)
        account <- unique(dataIn$usaa_data$account)
        names(account) <- fs$path_file(as.character(account))
        shiny$div(
          shiny$selectizeInput(ns("account"), "Account", account, account, multiple = TRUE, width = "100%"),
          shiny$dateRangeInput(ns("daterange"), "Date range:",
            start = Sys.Date() - 3,
            end = daterange[[2]],
            min = daterange[[1]],
            max = daterange[[2]],
            format = "mm/dd/yy",
            separator = " - ", width = "100%"
          )
        )
      })

      dataFiltered <- shiny$reactive({
        shiny$req(dataIn())
        shiny$req(input$daterange)
        dataIn <- dataIn()
        dataIn$usaa_data <- dplyr$filter(
          dataIn$usaa_data,
          dplyr$between(as.Date(date), input$daterange[[1]], input$daterange[[2]])
        )
        dataIn
      })

      output$categoryUI <- shiny$renderUI({
        shiny$req(dataFiltered())
        dataFiltered <- dataFiltered()
        category <- dataFiltered$usaa_data |>
          group_by(category) |>
          summarise(n_values = n()) |>
          ungroup() |>
          arrange(desc(n_values)) |>
          mutate(category_label = paste0(category, ": ", n_values))
        category_label <- category$category_label
        category <- category$category
        names(category) <- category_label
        shiny$selectizeInput(
          ns("category"),
          "Category",
          category,
          {
            if (input$selectAll) category else NULL
          },
          multiple = TRUE,
          width = "100%"
        )
      })

      dataDoubleFiltered <- shiny$reactive({
        shiny$req(dataFiltered())
        shiny$req(input$category)
        shiny$req(input$account)
        dataFiltered <- dataFiltered()
        dataFiltered$usaa_data <- dplyr$filter(
          dataFiltered$usaa_data,
          category %in% input$category,
          account %in% input$account
        )
        dataFiltered$categorySummary <-
          dataFiltered$usaa_data |>
          dplyr$group_by(category) |>
          summarise(amount = sum(amount))

        dataFiltered$recurring <- dataFiltered$usaa_data |>
          group_by(category, description) |>
          mutate(observations = n()) |>
          group_by(category, description, month) |>
          mutate(month_obs = n())
        dataFiltered
      })

      output$trend <- plotly$renderPlotly({
        box::use(ggplot2, forcats)
        shiny$req(dataDoubleFiltered())
        usaa_data <- dataDoubleFiltered()$usaa_data
        out <- usaa_data |>
          group_by(date, account, description, category) |>
          summarise(amount = sum(amount)) |>
          arrange(amount) |>
          mutate(description = forcats$as_factor(description)) |>
          ggplot2$ggplot() +
          ggplot2$aes(x = date, y = amount, fill = description, colour = category) +
          ggplot2$geom_col() +
          ggplot2$facet_wrap(account ~ ., scales = "free_y")
        plotly$ggplotly(out)
      })

      shiny$observe({
        shiny$req(dataDoubleFiltered())
        datatable$server_dt(id = "recurring", dataDoubleFiltered()$recurring)
        datatable$server_dt(id = "usaa_data", dataDoubleFiltered()$usaa_data)
        datatable$server_dt(id = "googleDrive", dataDoubleFiltered()$googleDrive)
        datatable$server_dt(id = "categorySummary", dataDoubleFiltered()$categorySummary)
      })
    }
  )
}

#' @export
read_sheets <- function() {
  box::use(googlesheets4)
  box::use(googledrive)
  googledrive$drive_auth(
    # email='drennanfreddy@gmail.com',
    path='application_default_credentials.json'
    # cache = 'gdrive'
  )
  googlesheets4$gs4_auth(token=googledrive$drive_token())
  sheet <- googlesheets4$read_sheet(getOption("billspage"))
}

#' @export
read_finances <- function() {
  box::use(fs[dir_ls], purrr[map_dfr], readr[read_csv], janitor[clean_names])
  box::use(lubridate[month], dplyr[mutate])
  finances <- fs::dir_ls("finances")
  finances <- map_dfr(finances, function(x) {
    out <- read_csv(x)
    out$account <- x
    out <- out |> clean_names()
  }) |>
    mutate(month = month(date))

  # browser()
  finances
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
