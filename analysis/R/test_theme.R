#' test_theme_base
#' @export
test_theme_base <- function() {
  colors <- list(
    minimal = list(
      black = "black",
      white = "white"
    ),
    rebase = list(
      rebase03 = "#fdf6e3",
      rebase02 = "#eee8d5",
      rebase01 = "#93a1a1",
      rebase00 = "#839496",
      rebase0 = "#657b83",
      rebase1 = "#586e75",
      rebase2 = "#073642",
      rebase3 = "#002b36"
    ),
    main = list(
      background = "#131516",
      well = "#242638",
      code = "#1D1F21",
      link = "#2B79A2"
    )
  )

  fonts <- list(
    primary = "Arial",
    size = list(
      h0 = 40, h1 = 32, h2 = 26, h3 = 22, h4 = 20, h5 = 12, p1 = 13, p2 = 11, em = 11
    )
  )

  base <- list(
    colors = colors,
    fonts = fonts,
    ndexr = list(
      font = fonts$primary,
      primary = colors$rebase$rebase03,
      background = "white",
      text = "black",
      title = colors$rebase$rebase0,
      line = colors$rebase$rebase01,
      well = "white",
      code = "white",
      link = colors$main$link,
      li = "white"
    )
  )

  base
}

#' headers
#' @export
headers <- function() {
  box::use(. / test_theme)
  box::use(shiny)
  box::use(shinyjs)
  box::use(flextable)
  theme <- test_theme$test_theme_base()
  shiny$withTags(
    shiny$tags$head(
      shiny$includeCSS("www/styles.css"),
      shinyjs$extendShinyjs(text = "shinyjs.reset_app = function() {history.go(0)}", functions = "reset_app"),
      shinyjs$useShinyjs(),
      shiny$tags$style(".shiny-output-error:after{contentsr: 'sorry, I guess I broke it.'; visibility: visible}"),
      shiny$tags$style(".shiny-output-error{visibility: hidden}"),
      shiny$tags$style(type = "text/css", "#statistics-current_plot.recalculating { opacity: 1; }"),
      shiny$tags$script(
        src = 'https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p'
      ),
      shiny$tags$script(
        shiny$HTML(
          'Shiny.addCustomMessageHandler("changetitle", function(x) {document.title=x});'
        )
      )
    )
  )
}
