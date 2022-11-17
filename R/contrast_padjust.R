#' contrast_padjust
#' @export contrast_padjust
#'
contrast_padjust <- function(model, contrast_list, data, variable, analysis_type = "Confirmatory",
                             overall_trend = FALSE) {
  {
    box::use(dplyr)
    box::use(emmeans)
    box::use(. / final_contrasts)
    box::use(furrr)
    box::use(utils)
    box::use(tibble)
    box::use(purrr)
  }
  analysis_type <- ifelse(overall_trend, "Confirmatory", "Exploratory")
  data <- dplyr$rename(data, tmp = variable)
  est <- emmeans$emmeans(
    object = model, ~ TreatmentNew * Time,
    adjust = "none", data = data,
    mode = "auto"
  )

  est_me <- emmeans$emmeans(
    object = model, ~TreatmentNew, adjust = "none", data = data,
    mode = "auto"
  )


  if (analysis_type == "Exploratory") {
    final_contrast <- purrr$map_dfr(.x = LETTERS[1:12], .f = ~ {
      keep <- which(sapply(contrast_list[[.x]], function(i) {
        all(i == floor(i))
      }) == TRUE)
      contrast_list <- lapply(keep, function(i) contrast_list[[.x]][[i]])
      out <- final_contrasts$final_contrasts(model = model, cont_list = contrast_list, est = est, letter = .x)
      if (!is.null(out)) {
        if (nrow(out) == 1) {
          out$contrast <- .x
        }
        if (nrow(out) > 1 & .x != "G") {
          out$contrast <- paste0(.x, 1:nrow(out))
        }
        if (.x == "G") {
          groups <- t(utils$combn(x = sum(grepl("Dose", levels(data$TreatmentNew))), m = 2))
          groups <- data.frame(groups)
          groups <- dplyr$mutate(groups, combn = paste0("G", X1, X2))
          out$contrast <- groups$combn
        }
        tibble$column_to_rownames(out, "contrast")
      }
    })
  } else {
    final_contrast <- purrr$map_dfr(.x = LETTERS[1:12], .f = ~ {
      if (!overall_trend) {
        keep <- which(sapply(contrast_list[[.x]], function(i) {
          all(i == floor(i))
        }) == TRUE)
        cont_list <- lapply(keep, function(i) contrast_list[[.x]][[i]])
      } else {
        cont_list <- contrast_list[[.x]]
      }
      out <- final_contrasts$final_contrasts(model = model, cont_list = cont_list, est = est, letter = .x)
      if (!is.null(out)) {
        num_pairs <- nrow(out) / 2
        if (.x == "G") {
          if (num_pairs == 1) {
            rownames(out) <- c("G12", "G12_st")
          } else {
            out_names <- cbind(
              utils$combn(1:sum(grepl("Dose", levels(data$TreatmentNew))), 2),
              utils$combn(1:sum(grepl("Dose", levels(data$TreatmentNew))), 2)
            )
            out_names <- t(out_names)
            out_names <- data.frame(out_names)
            out_names <- dplyr$arrange(out_names, X1, X2)
            out_names <- dplyr$mutate(out_names, final = dplyr$case_when(
              dplyr$row_number() %% 2 == 1 ~ paste0("G", X1, X2),
              dplyr$row_number() %% 2 == 0 ~ paste0("G", X1, X2, "_st")
            ))
            out_names <- dplyr$select(out_names, final)
            out_names <- unlist(out_names)

            rownames(out) <- out_names
          }
        } else if (num_pairs == 1) {
          rownames(out) <- c(.x, paste0(.x, "_st"))
        } else {
          rownames(out) <- c(
            paste(.x, 1:num_pairs, sep = ""),
            paste(.x, 1:num_pairs, "_st",
              sep = ""
            )
          )
        }
        final_contrast <- dplyr$select(out, -contrast)
      }
    })
  }

  return(list(
    final_contrast = final_contrast,
    emmeans_obj = list(
      AT = est_me,
      ST = est
    )
  ))
}
