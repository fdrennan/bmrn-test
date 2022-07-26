#' prism_output
#' @export prism_output
#'
prism_output <- function(tranformed_data, variable) {
  tmp <- tranformed_data %>%
    pivot_wider(
      id_cols = c(Type, Treatment, SubjectID),
      names_from = Time,
      values_from = variable
    )

  max_n <- tmp %>%
    group_by(Type, Treatment) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    slice_max(n, with_ties = FALSE) %>%
    select(n) %>%
    unlist()


  prism <- map_df(.x = 1:length(unique(tmp$Treatment)), .f = ~ {
    out <- tmp %>%
      filter(Treatment == unique(tmp$Treatment)[.x]) %>%
      mutate(SubjectID = paste("SubjectID", row_number()))

    diff <- max_n - nrow(out)
    trt_add <- distinct(.data = out, Type, Treatment)
    if (diff > 0) {
      for (i in 1:diff) {
        out <- out %>%
          bind_rows(data.frame(trt_add,
            SubjectID = ""
          ))
      }
    }
    return(out)
  }) %>%
    as.matrix() %>%
    t()

  prism[is.na(prism)] <- ""
  prism <- cbind(
    c("Timepoint", "", "", 1:sum(grepl("[[:digit:]]", colnames(tmp)))),
    prism
  )

  return(prism)
}

#' save_prism_output
#' @export save_prism_output
#'
save_prism_output <- function(path, tranformed_data, power, cfb) {
  prism_orig <- prism_output(tranformed_data, "Response")

  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Original Scale")

  if (power != 1) {
    addWorksheet(wb, sheetName = "Transformed Scale")
    prism_trans <- prism_output(tranformed_data, "Response_Transformed")
    writeData(wb = wb, sheet = "Transformed Scale", x = prism_trans, colNames = FALSE)
  }
  if (cfb) {
    addWorksheet(wb, sheetName = "Change from Baseline")
    prism_trans <- prism_output(tranformed_data, "Response_Transformed_bc")
    writeData(wb = wb, sheet = "Change from Baseline", x = prism_trans, colNames = FALSE)
  }


  writeData(wb = wb, sheet = "Original Scale", x = prism_orig, colNames = FALSE)

  saveWorkbook(wb, path, overwrite = TRUE)
}
