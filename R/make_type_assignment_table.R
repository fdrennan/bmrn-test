#' make_type_assignment_table
#' @export
make_type_assignment_table <- function(type_inputs, ns) {
  div(
    map2(type_inputs$type_snake, type_inputs$Type, function(typeid, type) {
      selectizeInput(ns(typeid), type, choices = c("Non-Wild Type", "Wild Type"))
    })
  )
}
