box::use(. / functions / start)
box::use(shiny[runApp])
if (interactive()) {
  options(shiny.host = "127.0.0.1")
  options(shiny.port = 8000)
}
runApp(
  start$start(),
  port = getOption("shiny.port"),
  host = getOption("shiny.host")
)
