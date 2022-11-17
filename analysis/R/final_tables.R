#' table_1
#' @export
table_1 <- function(final_contrast, os_together, toi) {
  {
    box::use(dplyr)
  }
  if (!all(!grepl("_st", rownames(final_contrast)))) {
    table_1 <- data.frame(
      TreatmentNew = c(
        rep("Wild Type", 2),
        rep("Other Comparator", 2),
        rep("Positive Control", 2),
        rep("Negative Control", 2)
      ),
      Endpoint = c(
        "Average", "Specific Time",
        "Average", "Specific Time",
        "Average", "Specific Time"
      ),
      `Difference from Vehicle` = c(
        "A", "A_st", "B", "B_st",
        "H", "H_st", "I", "I_st"
      ),
      `Difference from Positive Control` = c(
        "E", "E_st", NA, NA,
        NA, NA, NA, NA
      ),
      `Difference from Other Comparator` = c(
        "K", "K_st", NA, NA,
        NA, NA, NA, NA
      )
    )
  } else {
    table_1 <- data.frame(
      TreatmentNew = c("Wild Type", "Other Comparator", "Positive Control", "Negative Control"),
      Endpoint = rep("Specific Time", 4),
      `Difference from Vehicle` = c("A", "B", "H", "I"),
      `Difference from Positive Control` = c("E", NA, NA, NA),
      `Difference from Other Comparator` = c("K", NA, NA, NA)
    )
  }
  tmp <-
    dplyr$mutate(final_contrast, Difference = paste0(estimate, " (", lower.CL, ", ", upper.CL, ")"))
  tmp <- dplyr$select(tmp, Difference, p.value)
  tmp <- dplyr$mutate(tmp, contrast = row.names(tmp))

  for (i in grep("Difference", colnames(table_1), value = TRUE)) {
    table_1 <- dplyr$rename(table_1, "contrast" = i)
    table_1 <- dplyr$left_join(table_1, tmp)
    table_1 <- dplyr$select(table_1, -contrast)

    colnames(table_1)[(ncol(table_1) - 1):ncol(table_1)] <- c(i, paste0(
      "p value.",
      gsub("Difference.", "", i)
    ))
  }

  tab1 <- dplyr$inner_join(os_together, table_1)
  tab1 <- dplyr$mutate(tab1, Endpoint = ifelse(grepl("Average", Endpoint),
    "Average Over All Times",
    toi
  ))
  tab1 <- dplyr$rename(tab1, "Time Points" = Endpoint)

  tab1[is.na(tab1)] <- ""
  return(tab1)
}

#' table_2
#' @export
table_2 <- function(final_contrast, os_together, toi) {
  {
    box::use(dplyr)
  }
  # Ensure that the levels are ordered for this table
  # Vehicle versus the doses should be looked at first
  if (all(!grepl("_st", rownames(final_contrast)))) {
    rownames(final_contrast) <- paste0(rownames(final_contrast), "_st")
  }
  os_table_2 <- dplyr$filter(os_together, grepl("Dose|Vehicle", os_together$TreatmentNew))
  os_table_2 <- dplyr$mutate(os_table_2,
    TreatmentNew = droplevels.factor(TreatmentNew),
    TreatmentNew = factor(TreatmentNew,
      levels = c("Vehicle", grep("Dose", levels(TreatmentNew),
        value = TRUE
      ))
    )
  )
  os_table_2 <- dplyr$arrange(os_table_2, TreatmentNew)
  table_2 <- dplyr$select(os_table_2, TreatmentNew, Endpoint)
  for (i in grep("Vehicle|Dose", levels(table_2$TreatmentNew)[-1], value = TRUE)) {
    num <- as.numeric(gsub("[A-z]| ", "", i))
    if (any(table_2$Endpoint == "Average")) {
      contrast_veh <- c(paste0("D", num), paste0("D", num, "_st"))

      contrast_dose <- dplyr$filter(table_2, grepl("Dose", TreatmentNew))
      contrast_dose <- dplyr$mutate(contrast_dose,
        X = as.numeric(gsub("[A-z]| ", "", TreatmentNew)),
        contrast_dose = dplyr$case_when(
          X < num & Endpoint == "Average" ~
            paste0("G", X, num),
          X < num & Endpoint == "Specific Time" ~
            paste0("G", X, num, "_st")
        )
      )
      contrast_dose <- dplyr$select(contrast_dose, contrast_dose)
      contrast_dose <- unlist(contrast_dose)
    } else {
      contrast_veh <- paste0("D", num)
      contrast_dose <-
        dplyr$filter(table_2, grepl("Dose", TreatmentNew))
      contrast_dose <- dplyr$mutate(contrast_dose,
        X = as.numeric(gsub("[A-z]| ", "", TreatmentNew)),
        contrast_dose = dplyr$case_when(
          X < num & Endpoint == "Specific Time" ~
            paste0("G", X, num)
        )
      )
      contrast_dose <- dplyr$select(contrast_dose, contrast_dose)
      contrast_dose <- unlist(contrast_dose)
    }
    table_2[[paste0("Difference from ", i)]] <- c(contrast_veh, contrast_dose)
  }

  summary_stat <-
    dplyr$mutate(final_contrast, Difference = paste0(estimate, " (", lower.CL, ", ", upper.CL, ")"))
  summary_stat <- dplyr$select(summary_stat, Difference, p.value)
  summary_stat <- dplyr$mutate(summary_stat, contrast = row.names(summary_stat))


  for (i in grep("Difference", colnames(table_2), value = TRUE)) {
    table_2 <-
      dplyr$rename(table_2, "contrast" = i)
    table_2 <- dplyr$left_join(table_2, summary_stat)
    table_2 <- dplyr$select(table_2, -contrast)

    colnames(table_2)[(ncol(table_2) - 1):ncol(table_2)] <- c(i, paste0(
      "p value.",
      gsub("Difference.", "", i)
    ))
  }

  tab2 <- dplyr$inner_join(os_together, table_2)
  tab2 <- dplyr$mutate(tab2, Endpoint = ifelse(grepl("Average", Endpoint),
    "Average Over Time",
    toi
  ))
  tab2 <- dplyr$rename(tab2, "Time Points" = Endpoint)
  tab2[is.na(tab2)] <- ""

  return(tab2)
}

#' table_3
#' @export
table_3 <- function(final_contrast, os_together, toi, include_summ_stat = T) {
  {
    box::use(dplyr)
  }
  # Needs to be generalized to more than 2 groups
  if (!all(!grepl("_st", rownames(final_contrast)))) {
    os_table_3 <-
      dplyr$select(os_together, 1:2)
    os_table_3 <- dplyr$filter(os_table_3, grepl("Dose", os_together$TreatmentNew))
    os_table_3 <- dplyr$mutate(os_table_3,
      TreatmentNew = droplevels.factor(TreatmentNew),
      Dose = as.numeric(gsub(pattern = "[A-z]| ", "", TreatmentNew)),
      `Difference from Wild Type` = ifelse(Endpoint == "Average", paste0("C", Dose),
        paste0("C", Dose, "_st")
      ),
      `Difference from Positive Control` = ifelse(Endpoint == "Average", paste0("F", Dose),
        paste0("F", Dose, "_st")
      ),
      `Difference from Negative Control` = ifelse(Endpoint == "Average", paste0("I", Dose),
        paste0("I", Dose, "_st")
      ),
      `Difference from Other Comparator` = ifelse(Endpoint == "Average", paste0("L", Dose),
        paste0("L", Dose, "_st")
      )
    )
    os_table_3 <- dplyr$arrange(os_table_3, TreatmentNew)
  } else {
    os_table_3 <- dplyr$select(os_together, 1:2)
    os_table_3 <- dplyr$filter(os_table_3, grepl("Dose", os_together$TreatmentNew))
    os_table_3 <- dplyr$mutate(os_table_3,
      TreatmentNew = droplevels.factor(TreatmentNew),
      Dose = as.numeric(gsub(pattern = "[A-z]| ", "", TreatmentNew)),
      `Difference from Wild Type` = paste0("C", Dose),
      `Difference from Positive Control` = paste0("F", Dose),
      `Difference from Negative Control` = paste0("I", Dose),
      `Difference from Other Comparator` = paste0("L", Dose)
    )
    os_table_3 <- dplyr$arrange(os_table_3, TreatmentNew)
  }

  tmp <- dplyr$mutate(final_contrast, Difference = paste0(-1 * estimate, " (", -1 * upper.CL, ", ", -1 * lower.CL, ")"))
  tmp <- dplyr$select(tmp, Difference, p.value)
  tmp <- dplyr$mutate(tmp, contrast = row.names(tmp))

  for (i in grep("Difference", colnames(os_table_3), value = TRUE)) {
    os_table_3 <-
      dplyr$rename(os_table_3, "contrast" = i)
    os_table_3 <- dplyr$left_join(os_table_3, tmp)
    os_table_3 <- dplyr$select(os_table_3, -contrast)

    colnames(os_table_3)[(ncol(os_table_3) - 1):ncol(os_table_3)] <- c(i, paste0(
      "p.value.",
      gsub("Difference.", "", i)
    ))
  }


  if (include_summ_stat) {
    tab3 <- dplyr$inner_join(os_together, os_table_3)
    tab3 <- dplyr$mutate(tab3, Endpoint = ifelse(grepl("Average", Endpoint),
      "Average Over Time",
      toi
    ))
    tab3 <- dplyr$rename(tab3, "Time Points" = Endpoint)
    tab3 <- dplyr$select(tab3, -Dose)
    tab3[is.na(tab3)] <- ""
  } else {
    tab3 <- dplyr$select(os_table_3, -Dose)
    tab3 <- dplyr$mutate(tab3, Endpoint = ifelse(grepl("Average", Endpoint),
      "Average Over Time",
      toi
    ))
    tab3 <- dplyr$rename(tab3, "Time Points" = Endpoint)
    tab3[is.na(tab3)] <- ""
  }
  return(tab3)
}
