
#' clean_excel_data
#' @export
clean_excel_data <- function(file) {
  #
  data <- file$datapath %>%
    read_excel(.,
      sheet = "Data Entry",
      col_names = TRUE, skip = 2,
    )

  data <-
    data %>%
    # .[2:nrow(.),] |>
    mutate_if(is.numeric, as.character) %>%
    rename("Treatment" = `Treatment Group Name`)

  endpoint <- readxl::read_xlsx(
    path = file$datapath,
    range = "B1",
    sheet = "Data Entry",
    col_names = FALSE
  ) %>% as.character()

  BLQ_value <- readxl::read_xlsx(
    path = file$datapath,
    range = "D1",
    sheet = "Data Entry",
    col_names = FALSE
  ) %>% as.numeric()

  data <-
    data %>%
    mutate(
      type_snake = paste0("type_", snakecase::to_snake_case(Type)),
      treatment_snake = paste0("treatment_", snakecase::to_snake_case(Treatment))
    ) %>%
    mutate_all(.funs = ~ gsub("BLQ", BLQ_value, .))

  list(data = data, endpoint = endpoint, BLQ_value = BLQ_value)
}
