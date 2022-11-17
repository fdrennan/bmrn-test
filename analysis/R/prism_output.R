#' prism_output
#' @export prism_output
#'
prism_output <- function(tranformed_data, variable) {
  {
    box::use(tidyr)
    box::use(dplyr)
    box::use(purrr)
  }
  tmp <- 
    tidyr$pivot_wider(tranformed_data,
      id_cols = c(Type, Treatment, SubjectID),
      names_from = Time,
      values_from = variable
    )

  max_n <- dplyr$group_by(tmp, Type, Treatment) 
  max_n <- dplyr$summarize(max_n, n = dplyr$n()) 
  max_n <- dplyr$ungroup(max_n)
  max_n <- dplyr$slice_max(max_n, n, with_ties = FALSE) 
  max_n <-  dplyr$select(max_n, n) 
   max_n <- unlist(max_n)


  prism <- purrr$map_df(.x = 1:length(unique(tmp$Treatment)), .f = ~ {
    out <- dplyr$filter(tmp, Treatment == unique(tmp$Treatment)[.x])
    out <- dplyr$mutate(out, 
                        SubjectID = paste("SubjectID", 
                                          dplyr$row_number()))

    diff <- max_n - nrow(out)
    trt_add <- dplyr$distinct(.data = out, Type, Treatment)
    if (diff > 0) {
      for (i in 1:diff) {
        out <-
          dplyr$bind_rows(
            out,
            data.frame(trt_add,
              SubjectID = ""
            )
          )
      }
    }
    return(out)
  })
  prism <- as.matrix(prism)
  prism <- t(prism)

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
  box::use(. / prism_output)
  box::use(openxlsx)
  prism_orig <- prism_output$prism_output(tranformed_data, "Response")

  wb <- openxlsx$createWorkbook()
  openxlsx$addWorksheet(wb, sheetName = "Original Scale")

  if (power != 1) {
    openxlsx$addWorksheet(wb, sheetName = "Transformed Scale")
    prism_trans <- prism_output$prism_output(tranformed_data, "Response_Transformed")
    openxlsx$writeData(wb = wb, sheet = "Transformed Scale", x = prism_trans, colNames = FALSE)
  }
  if (cfb) {
    openxlsx$addWorksheet(wb, sheetName = "Change from Baseline")
    prism_trans <- prism_output$prism_output(tranformed_data, "Response_Transformed_bc")
    openxlsx$writeData(wb = wb, sheet = "Change from Baseline", x = prism_trans, colNames = FALSE)
  }


  openxlsx$writeData(wb = wb, sheet = "Original Scale", x = prism_orig, colNames = FALSE)

  openxlsx$saveWorkbook(wb, path, overwrite = TRUE)
}
