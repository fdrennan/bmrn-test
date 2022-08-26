#' contrast_padjust
#' @export contrast_padjust
#'
contrast_padjust <- function(model, contrast_list, data, variable, analysis_type = "Confirmatory", 
                             overall_trend = FALSE) {
  
  analysis_type = ifelse(overall_trend, 'Confirmatory', 'Exploratory')
  data <- data %>% rename(tmp = variable)
  est <- emmeans(
    object = model, ~ TreatmentNew * Time,
    adjust = "none", data = data,
    mode = "auto"
  )

  est_me <- emmeans(
    object = model, ~TreatmentNew, adjust = "none", data = data,
    mode = "auto"
  )

  if (getOption('run_parallel')) {
    mapping_fn <- future_map_dfr
  } else {
    mapping_fn <- map_dfr
  }
  
  if (analysis_type == "Exploratory") {
    
  
    final_contrast <- mapping_fn(.x = LETTERS[1:9], .f = ~ {
      keep <- which(sapply(contrast_list[[.x]], function(i) {
        all(i == floor(i))
      }) == TRUE)
      contrast_list <- lapply(keep, function(i) contrast_list[[.x]][[i]])
      out <- final_contrasts(model = model, cont_list = contrast_list, est = est, letter = .x)
      if (!is.null(out)) {
        if (nrow(out) == 1) {
          out$contrast <- .x
        }
        if (nrow(out) > 1 & .x != "G") {
          out$contrast <- paste0(.x, 1:nrow(out))
        }
        if (.x == "G") {
          groups <- t(combn(x = sum(grepl("Dose", levels(data$TreatmentNew))), m = 2)) %>%
            data.frame() %>%
            mutate(combn = paste0("G", X1, X2))
          out$contrast <- groups$combn
        }
        out %>% tibble::column_to_rownames("contrast")
      }
    })
  } else {
    final_contrast <- mapping_fn(.x = LETTERS[1:9], .f = ~ {
      if(!overall_trend){
      keep <- which(sapply(contrast_list[[.x]], function(i) {
        all(i == floor(i))
      }) == TRUE)
      cont_list <- lapply(keep, function(i) contrast_list[[.x]][[i]])
      }else{
        cont_list = contrast_list[[.x]]
      }
      out <- final_contrasts(model = model, cont_list = cont_list, est = est, letter = .x)
      if (!is.null(out)) {
        num_pairs <- nrow(out) / 2
        if (.x == "G") {
          if (num_pairs == 1) {
            rownames(out) <- c("G12", "G12_st")
          } else {
            rownames(out) <- cbind(
              combn(1:sum(grepl("Dose", levels(data$TreatmentNew))), 2),
              combn(1:sum(grepl("Dose", levels(data$TreatmentNew))), 2)
            ) %>%
              t() %>%
              data.frame() %>%
              arrange(X1, X2) %>%
              mutate(final = case_when(
                row_number() %% 2 == 1 ~ paste0("G", X1, X2),
                row_number() %% 2 == 0 ~ paste0("G", X1, X2, "_st")
              )) %>%
              select(final) %>%
              unlist()
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
        final_contrast <- out %>% select(-contrast)
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
