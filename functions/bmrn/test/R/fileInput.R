#' shinyInputLabel <- function(inputId, label = NULL) {
#'   tags$label(
#'     label,
#'     class = "control-label",
#'     class = if (is.null(label)) "shiny-label-null",
#'     # `id` attribute is required for `aria-labelledby` used by screen readers:
#'     id = paste0(inputId, "-label"),
#'     `for` = inputId
#'   )
#' }
#'
#'
#' #' fileInput
#' #' @export
#' fileInput <- function(inputId, label, multiple = FALSE, accept = NULL, width = NULL,
#'                       buttonLabel = "Browse...", placeholder = "") {
#'   restoredValue <- restoreInput(id = inputId, default = NULL)
#'   if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
#'     warning("Restored value for ", inputId, " has incorrect format.")
#'     restoredValue <- NULL
#'   }
#'   if (!is.null(restoredValue)) {
#'     restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
#'   }
#'
#'   inputTag <- tags$input(
#'     id = inputId, name = "", type = "file",
#'     `data-restore` = restoredValue
#'   )
#'   if (multiple) {
#'     inputTag$attribs$multiple <- "multiple"
#'   }
#'   if (length(accept) > 0) {
#'     inputTag$attribs$accept <- paste(accept, collapse = ",")
#'   }
#'   div(
#'     class = "", style = css(width = validateCssUnit(width)),
#'     shinyInputLabel(inputId, label), div(
#'       class = "input-group",
#'       withTags(div(
#'         class = "p-2",
#'         inputTag,
#'         input(
#'           type = "text", class = "form-control p-2",
#'           readonly = "readonly"
#'         )
#'       ))
#'     )
#'     # tags$div(
#'     #   id = paste(inputId, "_progress", sep = ""),
#'     #   class = "progress active shiny-file-input-progress p-2",
#'     #   tags$div(class = "progress-bar")
#'     # )
#'   )
#' }
