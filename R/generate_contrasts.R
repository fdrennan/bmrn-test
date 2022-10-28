generate_contrasts <- function(model, toi, data, time_order, analysis_type = "confirm") {
  data <- mutate(data,
    Time = factor(Time, levels = time_order),
    TreatmentNew = factor(TreatmentNew)
  ) # Need to figure out a way sort these

  num_times <- length(levels(data$Time))
  num_groups <- length(levels(data$TreatmentNew))
  num_doses <- length(grep("Dose", levels(data$TreatmentNew)))
  toi_num <- which(levels(data$Time) == toi)
  # Contrasts for the effect of a single group averaged (final_AE) and for the single group
  # effect a single time (final_SE)

  coef_name <- expand_grid(
    Time = levels(data$Time),
    TreatmentNew = levels(data$TreatmentNew)
  ) %>%
    mutate(final_name = paste(Time, TreatmentNew, sep = ":")) %>%
    select(final_name) %>%
    unlist()

  AE <- diag(1, nrow = num_groups) / num_times
  final_SE <- matrix(0, ncol = num_groups * num_times, nrow = num_groups)
  final_SE[, ((toi_num - 1) * num_groups + 1):(toi_num * num_groups)] <- diag(1, nrow = num_groups)
  rownames(AE) <- levels(data$TreatmentNew)
  rownames(final_SE) <- paste0(levels(data$TreatmentNew), "_Time", toi)
  final_AE <- AE
  for (i in 2:num_times) {
    final_AE <- cbind(final_AE, AE)
  }

  contrast_map <- data.frame(
    Label = LETTERS[1:12],
    Group_1 = c(
      "Wild Type",
      "Positive Control",
      "Wild Type",
      "Vehicle",
      "Wild Type",
      "Positive Control",
      "Dose",
      "Negative Control",
      "Negative Control",
      "Other Comparator",
      "Wild Type",
      "Other Comparator"
    ),
    Group_2 = c(
      "Vehicle",
      "Vehicle",
      "Dose",
      "Dose",
      "Positive Control",
      "Dose",
      "Dose",
      "Vehicle",
      "Dose",
      "Vehicle",
      "Other Comparator",
      "Dose"
    )
  )

  # contrast_map <-
  #   contrast_map %>%
  #   filter(Group_1 %in% unique(data$TreatmentNew),
  #          Group_2 %in% unique(data$TreatmentNew))
  # #
  final_list <- list()
  for (i in 1:nrow(contrast_map)) {
    a <- contrast_map$Group_1[i]
    b <- contrast_map$Group_2[i]
    if ((length(grep(
      paste0(a, "|", b),
      levels(data$TreatmentNew)
    )) < num_doses + 1 &
      !all(c(a, b) %in% levels(data$TreatmentNew))) & a != b) {
      coi_list <- list()
    } else {
      if (all(c(a, b) == "Dose") == FALSE) {
        # contrast of interest (coi)
        c1_SE <- final_SE[grep(a, rownames(final_SE)), ]
        c1_AE <- final_AE[grep(a, rownames(final_AE)), ]

        c2_SE <- matrix(final_SE[grep(b, rownames(final_SE)), ], ncol = num_groups * num_times)
        c2_AE <- matrix(final_AE[grep(b, rownames(final_AE)), ], ncol = num_groups * num_times)

        c1_SE_rep <- t(matrix(rep(c1_SE, nrow(c2_SE)), ncol = nrow(c2_SE)))
        c1_AE_rep <- t(matrix(rep(c1_AE, nrow(c2_AE)), ncol = nrow(c2_AE)))


        coi <- rbind(c1_AE_rep, c1_SE_rep) - rbind(c2_AE, c2_SE)
        colnames(coi) <- coef_name
        coi_list <- map(.x = 1:nrow(coi), .f = ~ {
          coi[.x, ]
        })
      } else {
        dose_names <- grep(a, rownames(final_AE), value = TRUE)

        grid <- combn(dose_names, 2) %>%
          t() %>%
          data.frame() %>%
          rename("Group_1" = "X1", "Group_2" = "X2")
        plan(multisession)
        coi_tmp <- future_map_dfr(.x = 1:nrow(grid), .f = ~ {
          a <- grid$Group_1[.x]
          b <- grid$Group_2[.x]

          c1_SE <- final_SE[grep(a, rownames(final_SE)), ]
          c1_AE <- final_AE[grep(a, rownames(final_AE)), ]

          c2_SE <- final_SE[grep(b, rownames(final_SE)), ]
          c2_AE <- final_AE[grep(b, rownames(final_AE)), ]

          coi <- data.frame(rbind(c1_AE, c1_SE) - rbind(c2_AE, c2_SE))
          colnames(coi) <- coef_name
          return(coi)
        }, .progress = TRUE, .options = furrr_options(seed = TRUE))

        coi_list <- map(1:nrow(coi_tmp), .f = ~ {
          unlist(coi_tmp[.x, ])
        })
      }
    }

    final_list <- rlist::list.append(final_list, coi_list)
    names(final_list)[i] <- contrast_map$Label[i]
  }


  if (analysis_type == "exp") {
    for (letter in LETTERS[1:12]) {
      if (length(final_list[[letter]]) > 0) {
        keep <- which(unlist(lapply(final_list[[letter]], function(i) {
          all(i %in% c(-1, 0, 1))
        })))
        tmp <- lapply(keep, function(i) {
          final_list[[letter]][[i]]
        })
        final_list[[letter]] <- tmp
      }
    }
  }

  return(final_list)
}
