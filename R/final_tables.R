#' table_1
#' @export
table_1 <- function(final_contrast, os_together, toi) {
  if (!all(!grepl("_st", rownames(final_contrast)))) {
    table_1 <- data.frame(
      TreatmentNew = c(
        rep("Wild Type", 2), rep("Positive Control", 2),
        rep("Negative Control", 2)
      ),
      Endpoint = c(
        "Average", "Specific Time",
        "Average", "Specific Time",
        "Average", "Specific Time"
      ),
      `Difference from Vehicle` = c(
        "A", "A_st", "B", "B_st",
        "H", "H_st"
      ),
      `Difference from Positive Control` = c(
        "E", "E_st", NA, NA,
        NA, NA
      )
    )
  } else {
    table_1 <- data.frame(
      TreatmentNew = c("Wild Type", "Positive Control", "Negative Control"),
      Endpoint = rep("Specific Time", 3),
      `Difference from Vehicle` = c("A", "B", "H"),
      `Difference from Positive Control` = c("E", NA, NA)
    )
  }
  tmp <- final_contrast %>%
    mutate(Difference = paste0(estimate, " (", lower.CL, ", ", upper.CL, ")")) %>%
    select(Difference, p.value) %>%
    mutate(contrast = row.names(.))

  #
  for (i in grep("Difference", colnames(table_1), value = TRUE)) {
    table_1 <- table_1 %>%
      rename("contrast" = i) %>%
      mutate(contrast) %>%
      left_join(tmp) %>%
      select(-contrast)

    colnames(table_1)[(ncol(table_1) - 1):ncol(table_1)] <- c(i, paste0(
      "p value.",
      gsub("Difference.", "", i)
    ))
  }

  tab1 <- inner_join(os_together, table_1) %>%
    mutate(Endpoint = ifelse(grepl("Average", Endpoint),
      "Average Over All Times",
      toi
    )) %>%
    rename("Time Points" = Endpoint)

  tab1[is.na(tab1)] <- ""
  return(tab1)
}

#' table_2
#' @export
table_2 <- function(final_contrast, os_together, toi) {
  # Ensure that the levels are ordered for this table
  # Vehicle versus the doses should be looked at first
  if (all(!grepl("_st", rownames(final_contrast)))) {
    rownames(final_contrast) <- paste0(rownames(final_contrast), "_st")
  }
  os_table_2 <- os_together %>%
    filter(grepl("Dose|Vehicle", os_together$TreatmentNew)) %>%
    mutate(
      TreatmentNew = droplevels.factor(TreatmentNew),
      TreatmentNew = factor(TreatmentNew,
        levels = c("Vehicle", grep("Dose", levels(TreatmentNew),
          value = TRUE
        ))
      )
    ) %>%
    arrange(TreatmentNew)

  table_2 <- os_table_2 %>%
    select(TreatmentNew, Endpoint)
  for (i in grep("Vehicle|Dose", levels(table_2$TreatmentNew)[-1], value = TRUE)) {
    num <- as.numeric(gsub("[A-z]| ", "", i))
    if (any(table_2$Endpoint == "Average")) {
      contrast_veh <- c(paste0("D", num), paste0("D", num, "_st"))

      contrast_dose <- table_2 %>%
        filter(grepl("Dose", TreatmentNew)) %>%
        mutate(
          X = as.numeric(gsub("[A-z]| ", "", TreatmentNew)),
          contrast_dose = case_when(
            X < num & Endpoint == "Average" ~
              paste0("G", X, num),
            X < num & Endpoint == "Specific Time" ~
              paste0("G", X, num, "_st")
          )
        ) %>%
        select(contrast_dose) %>%
        unlist()
    } else {
      contrast_veh <- paste0("D", num)
      contrast_dose <- table_2 %>%
        filter(grepl("Dose", TreatmentNew)) %>%
        mutate(
          X = as.numeric(gsub("[A-z]| ", "", TreatmentNew)),
          contrast_dose = case_when(
            X < num & Endpoint == "Specific Time" ~
              paste0("G", X, num)
          )
        ) %>%
        select(contrast_dose) %>%
        unlist()
    }
    table_2[[paste0("Difference from ", i)]] <- c(contrast_veh, contrast_dose)
  }

  summary_stat <- final_contrast %>%
    mutate(Difference = paste0(estimate, " (", lower.CL, ", ", upper.CL, ")")) %>%
    select(Difference, p.value) %>%
    mutate(contrast = row.names(.))


  for (i in grep("Difference", colnames(table_2), value = TRUE)) {
    table_2 <- table_2 %>%
      rename("contrast" = i) %>%
      left_join(summary_stat) %>%
      select(-contrast)

    colnames(table_2)[(ncol(table_2) - 1):ncol(table_2)] <- c(i, paste0(
      "p value.",
      gsub("Difference.", "", i)
    ))
  }

  tab2 <- inner_join(os_together, table_2) %>%
    mutate(Endpoint = ifelse(grepl("Average", Endpoint),
      "Average Over Time",
      toi
    )) %>%
    rename("Time Points" = Endpoint)
  tab2[is.na(tab2)] <- ""

  return(tab2)
}

#' table_3
#' @export
table_3 <- function(final_contrast, os_together, toi) {
  # Table 3
  # Needs to be generalized to more than 2 groups
  if (!all(!grepl("_st", rownames(final_contrast)))) {
    os_table_3 <- os_together %>%
      select(1:2) %>%
      filter(grepl("Dose", os_together$TreatmentNew)) %>%
      mutate(
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
        )
      ) %>%
      arrange(TreatmentNew)
  } else {
    os_table_3 <- os_together %>%
      select(1:2) %>%
      filter(grepl("Dose", os_together$TreatmentNew)) %>%
      mutate(
        TreatmentNew = droplevels.factor(TreatmentNew),
        Dose = as.numeric(gsub(pattern = "[A-z]| ", "", TreatmentNew)),
        `Difference from Wild Type` = paste0("C", Dose),
        `Difference from Positive Control` = paste0("F", Dose),
        `Difference from Negative Control` = paste0("I", Dose)
      ) %>%
      arrange(TreatmentNew)
  }

  tmp <- final_contrast %>%
    mutate(Difference = paste0(-1 * estimate, " (", -1 * upper.CL, ", ", -1 * lower.CL, ")")) %>%
    select(Difference, p.value) %>%
    mutate(contrast = row.names(.))

  for (i in grep("Difference", colnames(os_table_3), value = TRUE)) {
    os_table_3 <- os_table_3 %>%
      rename("contrast" = i) %>%
      left_join(tmp) %>%
      select(-contrast)

    colnames(os_table_3)[(ncol(os_table_3) - 1):ncol(os_table_3)] <- c(i, paste0(
      "p.value.",
      gsub("Difference.", "", i)
    ))
  }
  tab3 <- os_table_3 %>%
    select(-Dose) %>%
    mutate(Endpoint = ifelse(grepl("Average", Endpoint),
      "Average Over Time",
      toi
    )) %>%
    rename("Time Points" = Endpoint)
  tab3[is.na(tab3)] <- ""
  return(tab3)
}
