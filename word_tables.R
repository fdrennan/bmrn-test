#' word_tables
#' @export word_tables
#'
word_tables <- function(data, include_summ_stat, transform, summary_only,
                        footer_i = 1, footer_j = 1, footer = "footer", endpoint) {
  data <- data %>%
    mutate(`Time Points` = gsub("Average.*", "Overall Average", `Time Points`)) %>%
    mutate_at(
      .vars = grep("p value", colnames(.), value = TRUE),
      .funs = ~ as.numeric(ifelse(. == "< 0.001", "0.001", .))
    ) %>%
    mutate_at(
      .vars = grep("p value", colnames(.), value = TRUE),
      .funs = ~ ifelse(. < 0.05, paste0(., "*"), .)
    ) %>%
    mutate_at(
      .vars = grep("p value", colnames(.), value = TRUE),
      .funs = ~ ifelse(. == "0.001*", "< 0.001*", .)
    )

  if (summary_only) {
    data <- data %>%
      dplyr::select(
        Treatment, `Time Points`,
        grep("Original", colnames(.), value = T)
      )
    colnames(data) <- gsub("Original Scale ", "", colnames(data))

    data_ft <- flextable(data, col_keys = colnames(data))

    data_ft <- add_header_row(data_ft,
      colwidths = c(2, 3),
      values = c("", endpoint)
    )
  } else {
    if (transform) {
      data <- data %>% select(colnames(.)[!grepl("Orig", colnames(.))])
    }

    if (!transform) {
      data <- data %>% select(colnames(.)[!grepl("Trans", colnames(.))])
    }

    if (!include_summ_stat) {
      data <- data %>% select(colnames(.)[!grepl("Orig|Trans", colnames(.))])
    }

    orig_cols <- colnames(data)
    old_names <- grep("Difference from|p value", orig_cols, value = TRUE)
    groups <- gsub("Difference from ", "", old_names)[grepl("Difference", old_names)]
    new_names <- rep(c("LSMean Diff (95% CI)", "p value"), length(groups))
    names(new_names) <- old_names

    for (i in groups) {
      col <- paste("Difference from", i)
      Est <- map_chr(
        .x = 1:nrow(data),
        .f = ~ {
          ifelse(data[[col]][.x][[1]][1] == "",
            "", strsplit(split = " ", x = data[[col]][.x])[[1]][1]
          )
        }
      )

      CInt <- map_chr(
        .x = 1:nrow(data),
        .f = ~ {
          ifelse(data[[col]][.x] == "", "",
            paste0(
              "(",
              gsub(
                pattern = ".*\\(", replacement = "",
                x = data[[col]][.x]
              )
            )
          )
        }
      )


      data <- cbind.data.frame(data, Estimate = Est, CI = CInt)
      est_col_Name <- paste("Estimate", i)
      colnames(data)[colnames(data) == "Estimate"] <- est_col_Name
      CI_col_Name <- paste("CI", i)
      colnames(data)[colnames(data) == "CI"] <- CI_col_Name
    }

    old_scale <- grep("Original|Transformed", colnames(data), value = T)
    new_scale <- gsub("Original Scale ", "", old_scale)
    new_scale <- gsub("Transformed Scale ", "", new_scale)
    new_scale <- gsub("Back ", "", new_scale)
    new_scale <- gsub("Transformed ", "", new_scale)
    names(new_scale) <- old_scale

    data_ft <- flextable(data, col_keys = orig_cols) %>%
      set_header_labels(data_ft, values = new_scale)

    for (i in groups) {
      col <- paste("Difference from", i)
      est_col_Name <- paste("Estimate", i)
      CI_col_Name <- paste("CI", i)
      data_ft <- data_ft %>% flextable::compose(
        j = col,
        value = as_paragraph(
          as_chunk(x = get(est_col_Name)),
          "\n",
          as_chunk(x = get(CI_col_Name))
        )
      )
    }


    if (include_summ_stat & transform) {
      data_ft <- add_header_row(data_ft,
        colwidths = c(2, 2, 2, rep(2, length(groups))),
        values = c("", "Transformed Scale", "Back Transformed", paste("vs.", groups))
      )
    }


    if (include_summ_stat & !transform) {
      data_ft <- add_header_row(data_ft,
        colwidths = c(2, 3, rep(2, length(groups))),
        values = c("", endpoint, paste("vs.", groups))
      )
    }
    if (!include_summ_stat) {
      data_ft <- add_header_row(data_ft,
        colwidths = c(2, rep(2, length(groups))),
        values = c("", paste("vs.", groups))
      )
    }

    data_ft <- set_header_labels(data_ft, values = new_names)
  }
  data_ft <- align(data_ft, align = "center", part = "all")
  data_ft <- fontsize(data_ft, size = 8, part = "header")
  data_ft <- fontsize(data_ft, size = 8)
  #  if(include_summ_stat){
  #data_ft <- footnote(x = data_ft, i = footer_i, j = footer_j, value = as_paragraph(footer), ref_symbols = "")
  data_ft <- add_footer_lines(data_ft, footer)
  data_ft <- fontsize(data_ft, size = 8, part = "footer")
  # }
  data_ft
}

#' FitFlextableToPage
#' @export FitFlextableToPage
#'
FitFlextableToPage <- function(ft, pgwidth = 8){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}