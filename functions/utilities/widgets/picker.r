#' #' @export
#' pickerInput <- function (inputId, label = NULL, choices, selected = NULL, multiple = FALSE,
#'           options = list(), choicesOpt = NULL, width = NULL, inline = FALSE)
#' {
#'   choices <- shinyWidgets:::choicesWithNames(choices)
#'   selected <- restoreInput(id = inputId, default = selected)
#'   if (!is.null(options) && length(options) > 0)
#'     names(options) <- paste("data", names(options), sep = "-")
#'   if (!is.null(width))
#'     options <- c(options, list(`data-width` = width))
#'   if (!is.null(width) && width %in% c("fit"))
#'     width <- NULL
#'   options <- lapply(options, function(x) {
#'     if (identical(x, TRUE))
#'       "true"
#'     else if (identical(x, FALSE))
#'       "false"
#'     else x
#'   })
#'   maxOptGroup <- options[["data-max-options-group"]]
#'   selectTag <- tag("select", dropNulls(options))
#'   selectTag <- tagAppendAttributes(tag = selectTag, id = inputId,
#'                                    class = "selectpicker form-control")
#'   selectTag <- tagAppendChildren(tag = selectTag, pickerSelectOptions(choices,
#'                                                                       selected, choicesOpt, maxOptGroup))
#'   if (multiple)
#'     selectTag$attribs$multiple <- "multiple"
#'   pickerTag <- tags$div(class = "form-group shiny-input-container",
#'                         class = if (isTRUE(inline))
#'                           "shiny-input-container-inline", style = if (!is.null(width))
#'                             paste0("width: ", validateCssUnit(width), ";"), style = if (isTRUE(inline))
#'                               "display: inline-block;", tags$label(class = "control-label",
#'                                                                    `for` = inputId, label, class = if (is.null(label))
#'                                                                      "shiny-label-null", style = if (isTRUE(inline))
#'                                                                        "display: inline-block;", ), selectTag)
#'   attachShinyWidgetsDep(pickerTag, "picker")
#' }
