#!/usr/bin/env Rscript

##############################################
# Helper functions for AutoPlate Step 1) Input
##############################################

# Read CSVs + append name
read_plus <- function(name, file) {
  read.csv(file) %>%
    dplyr::mutate(filename = name)
}
init_cols <- function(assay_df) {
  assay_df$filename <- gsub(".csv","",assay_df$filename)
  assay_df$types <- ""
  assay_df$subject <- ""
  assay_df$dilution <- ""
  assay_df$bleed <- ""
  assay_df$inoculate <- ""
  assay_df$primary <- ""
  assay_df$study <- ""
  assay_df$neutralisation <- as.numeric("")
  assay_df$exclude <- FALSE
  return(assay_df)
}
init_types <- function(assay_df) {
  assay_df <- assay_df %>%
    dplyr::mutate(types = case_when(
      wcol == 1 & wrow %in% c("A", "B", "C", "D", "E") ~ "v",
      wcol == 1 & wrow %in% c("F", "G", "H") ~ "c",
      wcol %in% seq(2, 11) ~ "x",
      wcol == 12 ~ "m",
    ))
  return(assay_df)
}
init_subject <- function(assay_df, wcol1, wcol2, subject) {
  plates_not_numbered <- all(is.na(as.numeric(assay_df$plate_number)))
  if (plates_not_numbered) {
    assay_df <- assay_df %>% dplyr::mutate(rank = dense_rank(plate_number))
    assay_df$plate_number <- assay_df$rank
  }
  assay_df$subject <- ifelse(
    assay_df$wcol == wcol1 | assay_df$wcol == wcol2, paste("Mouse", (as.numeric(assay_df$plate_number) - 1) * 5 + subject), assay_df$subject
  )
  if (plates_not_numbered) {
    assay_df$plate_number <- assay_df$filename
    assay_df$rank <- NA
  }
  return(assay_df)
}
init_neut <- function(assay_df) {
  plates <- unique(assay_df$plate_number)
  for (plate_n in plates) {
    plate_df <- assay_df[assay_df$plate_number == plate_n, ]
    # max & min levels of cell infection
    neu0 <- mean(plate_df[plate_df$types == "v", ]$rlu)
    neu100 <- mean(plate_df[plate_df$types == "c", ]$rlu)
    # express rlu as neutralisation percentage between neu0 and new100
    plate_df$neutralisation <- 100 * ((plate_df$rlu - neu0) / (neu100 - neu0))
    # update main assay dataframe with neutralisations
    assay_df[rownames(plate_df), ] <- plate_df
  }
  return(assay_df)
}
update_dilutions <- function(assay_df, dilutions) {
  assay_df %>%
    dplyr::mutate(dilution = case_when(
      wrow == "A" & types == "x" ~ dilutions[1, 1],
      wrow == "B" & types == "x" ~ dilutions[2, 1],
      wrow == "C" & types == "x" ~ dilutions[3, 1],
      wrow == "D" & types == "x" ~ dilutions[4, 1],
      wrow == "E" & types == "x" ~ dilutions[5, 1],
      wrow == "F" & types == "x" ~ dilutions[6, 1],
      wrow == "G" & types == "x" ~ dilutions[7, 1],
      wrow == "H" & types == "x" ~ dilutions[8, 1],
      wrow == "A" & types == "m" ~ dilutions[1, 2],
      wrow == "B" & types == "m" ~ dilutions[2, 2],
      wrow == "C" & types == "m" ~ dilutions[3, 2],
      wrow == "D" & types == "m" ~ dilutions[4, 2],
      wrow == "E" & types == "m" ~ dilutions[5, 2],
      wrow == "F" & types == "m" ~ dilutions[6, 2],
      wrow == "G" & types == "m" ~ dilutions[7, 2],
      wrow == "H" & types == "m" ~ dilutions[8, 2]
    ))
}
update_subjects <- function(assay_df, updated_plate_df, plate_n) {
  updated_subjects <- updated_plate_df[1, ]
  for (i in seq(1, length(updated_subjects))) {
    assay_df <- assay_df %>%
      dplyr::mutate(subject = ifelse((plate_number == plate_n) & (wcol == i), updated_subjects[i], subject))
  }
  return(assay_df)
}
update_types <- function(assay_df, updated_plate_df, plate_n) {
  updated_types <- tail(updated_plate_df, -1)
  for (col in seq(1, length(updated_types))) {
    full_col <- updated_types[, col]
    for (i in seq(1, length(full_col))) {
      row <- row.names(updated_types)[i]
      updated_type <- updated_types[row, col]
      current_type <- dplyr::filter(assay_df, (plate_number == plate_n) & (wcol == col) & (wrow == row))["types"]
      if (current_type != updated_type) {
        print(paste0("For plate ", plate_n, ", well ", row, col, ", updating type ", current_type, " -> ", updated_type))
        assay_df <- assay_df %>%
          dplyr::mutate(types = ifelse((plate_number == plate_n) & (wcol == col) & (wrow == row), updated_types[row, col], types))
      }
    }
  }
  return(assay_df)
}
create_feature_dropdown <- function(new_feature, input, values) {
  req(input$plate_data)
  assay_df <- isolate(values[["assay_df"]])
  selectInput(new_feature, "Select existing feature", names(assay_df))
}
create_feature_table <- function(new_feature, input, values) {
  req(input[[new_feature]])
  assay_df <- isolate(values[["assay_df"]])
  feature_levels <- levels(as.factor(unlist(assay_df[[input[[new_feature]]]])))
  feature_levels <- feature_levels[order(nchar(feature_levels), feature_levels)]
  new_feature_df <- data.frame(matrix(unlist(feature_levels), nrow = length(feature_levels), byrow = T))
  names(new_feature_df) <- input[[new_feature]]
  new_feature_df[[new_feature]] <- as.character(NA)
  rhandsontable(new_feature_df, stretchH = "all", rowHeaders = NULL)
}
update_feature <- function(new_feature, input, values) {
  new_feature_table <- paste0(new_feature, "_table")
  req(new_feature_table)
  existing_feature <- input[[new_feature]]
  assay_df <- values[["assay_df"]]
  mappings_table <- hot_to_r(isolate(input[[new_feature_table]]))
  assay_df[[new_feature]] <- mappings_table[match(assay_df[[existing_feature]], mappings_table[[existing_feature]]), 2]
  values[["assay_df"]] <- assay_df
}
assay_to_plate_df <- function(assay_df, plate_n) {
  plate_df <- isolate(assay_df[assay_df$plate_number == plate_n, ]) %>%
    dplyr::select(wrow, wcol, types) %>%
    tidyr::spread(key = wcol, value = types) %>%
    dplyr::rename(Well = wrow)
  # Re-order cols
  plate_df <- plate_df[c("Well", sort(as.numeric(names(plate_df))))]
  # Get the subjects
  subjects <- isolate(assay_df[assay_df$plate_number == plate_n, ]) %>%
    dplyr::filter(wrow == "A") %>%
    dplyr::select(subject)
  subject <- c("Subject", subjects$subject)
  # Reformat the row & col names + add a subject row
  names(subject) <- names(plate_df)
  plate_df <- rbind(subject, plate_df)
  row.names(plate_df) <- plate_df$Well
  plate_df[1] <- NULL
  return(plate_df)
}
