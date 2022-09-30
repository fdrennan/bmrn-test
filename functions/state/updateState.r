#' updateState
#' @export updateState
updateState <- function(input, id) {
  {
    box::use(shiny[is.reactivevalues,showNotification,tags, reactiveValuesToList])
    box::use(.. / connections / sqlite,
             ../ connections /storr)
    box::use(./update.list[update.list])
  }


  if (is.reactivevalues(input)) {
    input <- reactiveValuesToList(input)
  }

  if(!length(input)) return()

  con <- storr$connection_storr()
  state <- con$get(id)
  state <- update.list(state, input)
  con$set(id, state)
  if (getOption('state.verbose')) {
    box::use(jsonlite)
    # print(state)
    showNotification(
      tags$pre(tags$code(
        jsonlite$toJSON(state, pretty=TRUE)
      ))
    )
  }
}
