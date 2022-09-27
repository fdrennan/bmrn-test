#' @example
#' collapser(id ='asdf'),
#' shiny$div(
#'  id = "asdf", class = "collapse show",
#'  tags$div("Hello", class = "card card-body")
#' )
#' @export
collapser <- function(id = NULL,
                      data_bs_toggle = c("collapse", "offcanvas"),
                      class = "btn", label = shiny::icon('arrow-up')) {
  data_bs_toggle <- match.arg(data_bs_toggle)
  box::use(shiny[tags, tag])
  tag("button", varArgs = list(
    class = class,
    type = "button",
    `data-bs-toggle` = data_bs_toggle,
    `data-bs-target` = paste0("#", id),
    `aria-expanded` = "false",
    `aria-controls` = id,
    label
  ))
}

#' @export
offcanvas <- function(id = "offcanvasScrolling",
                      header = "offcanvas header",
                      body = "offcanvas body",
                      location = c("start", "end", "top", "bottom")) {
  box::use(shiny, shiny[tags])
  box::use(. / app)
  location <- match.arg(location)
  shiny$fluidRow(class='p-1',
    app$collapser(label=shiny$icon('arrow-up'), id = id, data_bs_toggle = "offcanvas"),
    tags$div(
      class = paste(
        paste("offcanvas", paste0(c("offcanvas", location), collapse = "-")), 'bg-dark'
      ),
      `data-bs-scroll` = "true",
      `data-bs-backdrop` = "false",
      tabindex = "-1",
      id = id,
      `aria-labelledby` = paste0(id, "Label"),
      tags$div(
        class = "offcanvas-header",
        tags$h5(class = "offcanvas-title", id = paste0(id, "Label"), header),
        tags$button(class = "btn-close text-reset", `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")
      ),
      tags$div(class = "offcanvas-body", body)
    )
  )
}

#' @export
ui <- function() {
  box::use(shiny, shinyjs, shinycssloaders)
  box::use(shiny[tag, tags, HTML])
  box::use(. / app)
  shiny$addResourcePath("loaders", "./www/images/loaders")
  shiny$fluidPage(id='homepage',
    shinyjs$useShinyjs(),
    tags$head(HTML(
      '<script>
        function openFullscreen() {
          var elem = document.getElementById("homepage");
          if (elem.requestFullscreen) {
            elem.requestFullscreen();
          }
        }
        </script>')
    ),
    shiny$includeCSS("./www/styles.css"),
    shiny$includeScript("node_modules/bootstrap/dist/js/bootstrap.bundle.min.js"),
    shiny$fluidRow(class='bg-light',
      tags$div(
        class = "btn-toolbar",
        role = "toolbar",
        `aria-label` = "Toolbar with button groups",
        shiny$div(
          class = "btn-group me-2", role = "group", `aria-label` = "First group",
          app$offcanvas(
            location = "bottom",
            header = tags$h1("Console"),
            body = tags$h1("Development Information")
          ),
          tag('button', varArgs = list(onclick="openFullscreen();", class='btn', shiny$icon('expand')))
        )
      ),
      # class = "vh-100",
      shiny$div(
        id = "body", class = "col-9 p-3",
        shiny$fluidRow(
        )
      )
    )
  )
}

#' @export
server <- function(input, output, session) {


}

#' @export
start <- function() {
  box::use(shiny)
  box::use(. / app)
  shiny$runApp(
    shiny$shinyApp(app$ui, app$server)
  )
}
