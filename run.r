box::use(. / functions / start)
box::use(shiny[runApp])
runApp(
  start$start(),
  port = getOption("shiny.port"),
  host = getOption("shiny.host")
)
