
#' clean_excel_data
#' @export
clean_excel_data <- function(file) {
  box::use(readxl)
  box::use(dplyr)
  box::use(snakecase)
  data <-
    readxl$read_excel(
      file$datapath,
      sheet = "Data Entry",
      col_names = TRUE, skip = 2
    )

  data <- dplyr$mutate_if(data, is.numeric, as.character)
  data <- dplyr$rename(data, "Treatment" = `Treatment Group Name`)

  endpoint <- readxl$read_xlsx(
    path = file$datapath,
    range = "B1",
    sheet = "Data Entry",
    col_names = FALSE
  )

  endpoint <- as.character(endpoint)

  BLQ_value <- readxl$read_xlsx(
    path = file$datapath,
    range = "D1",
    sheet = "Data Entry",
    col_names = FALSE
  )

  BLQ_value <- as.numeric(BLQ_value)

  data <-
    dplyr$mutate(
      data,
      type_snake = paste0("type_", snakecase$to_snake_case(Type)),
      treatment_snake = paste0("treatment_", snakecase$to_snake_case(Treatment))
    )

  if (any(data[, 7:ncol(data)] == "BLQ")) {
    data <- dplyr$mutate_all(data, .funs = ~ gsub("BLQ", BLQ_value, .))
  }
  list(data = data, endpoint = endpoint, BLQ_value = BLQ_value)
}
