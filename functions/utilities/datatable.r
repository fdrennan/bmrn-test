#' @export
ui_dt <- function(id = "dt", title = NULL, collapsed = TRUE,
                  width = 12) {
  box::use(shiny, DT, shiny[actionButton, icon])
  ns <- shiny$NS(id)
  shiny$column(width,
    id = ns(id),
    actionButton(ns("full"), icon("expand")),
    shiny$div(class = "text-right", shiny$downloadButton(ns("downloadData"), "Download")),
    DT$DTOutput(ns("ui"), width = "100%")
  )
}

#' @export
server_dt <- function(id = "dt", data, title, pageLength = 3) {
  box::use(shiny, DT, esquisse, utils, dplyr, shinyWidgets, readr, writexl)
  box::use(shiny[observeEvent])
  box::use(shinyjs[js])
  shiny$moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$full, {
        js$fullScreen(ns(id))
      })
      output$downloadData <-
        shiny$downloadHandler(
          contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          filename = "output.xlsx",
          content = function(file) {
            writexl$write_xlsx(cleanedData(), file)
          }
        )

      output$ui <- DT$renderDT(
        server = TRUE,
        {
          DT::datatable(data,
            options = list(
              scrollX = TRUE,
              pageLength = pageLength,
              filter = "top"
            ),
            class = "compact",
            caption = NULL,
            filter = c("top"),
            escape = TRUE,
            style = "bootstrap4",
            width = NULL,
            height = NULL,
            elementId = NULL,
            fillContainer = getOption("DT.fillContainer", NULL),
            autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
            selection = "multiple", #  c("multiple", "single", "none"),
            extensions = list(),
            plugins = NULL,
            editable = FALSE
          )
        }
      )

      # data_rv <- shiny$reactiveValues(data = cleanedData(), name = ns("data"))
      # esquisse$esquisse_server(paste0('esquisse', title), data_rv)
    }
  )
}
