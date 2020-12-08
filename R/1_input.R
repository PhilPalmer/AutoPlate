#!/usr/bin/env Rscript

##############################################
# Helper functions for AutoPlate Step 1) Input
##############################################

#' @title Read plus
#'
#' @description Read CSV file appending the filename as an additional column in the dataframe
#' @param filename character, file name to be added to the dataframe
#' @param filepath character, path to the CSV file
#' @return dataframe, generated from the input CSV containing the filenames
#' @keywords read input CSV files
#' @export
#' @examples
#' read_plus()
read_plus <- function(filename, filepath) {
  read.csv(filepath) %>%
    dplyr::mutate(filename = filename)
}

#' @title Init cols
#'
#' @description Initialise columns for assay dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @return dataframe, containing the initialised columns
#' @keywords assay
#' @export
#' @examples
#' init_cols()
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

#' @title Init types
#'
#' @description Initialise types (c,m,v,x) for assay dataframe based on template 96-well plate format
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @return dataframe, containing the initialised type column
#' @keywords assay
#' @export
#' @examples
#' init_types()
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

#' @title Init subject
#'
#' @description Initialise subject (mouse number) for assay dataframe based on input parameters
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param wcols vector, list of column numbers to update for each plate
#' @param subject character, name to set subject to for specified columns
#' @return dataframe, containing the initialised subject column
#' @keywords assay
#' @export
#' @examples
#' init_subject()
init_subject <- function(assay_df, wcols, subject) {
  plates_not_numbered <- all(is.na(as.numeric(assay_df$plate_number)))
  if (plates_not_numbered) {
    assay_df <- assay_df %>% dplyr::mutate(rank = dense_rank(plate_number))
    assay_df$plate_number <- assay_df$rank
  }
  if (subject != "Antibody") {
    assay_df$subject <- ifelse(
      assay_df$wcol %in% wcols, paste("Mouse", (as.numeric(assay_df$plate_number) - 1) * 5 + subject), assay_df$subject
    )
  } else {
    assay_df$subject <- ifelse( assay_df$wcol %in% wcols, subject, assay_df$subject )
  }
  if (plates_not_numbered) {
    assay_df$plate_number <- assay_df$filename
    assay_df$rank <- NA
  }
  return(assay_df)
}

#' @title Calculate neutralisation
#'
#' @description Calculate neutralisation using the RLU values and normalisation with the virus and cell only controls
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @return dataframe, containing the initialised neutralisation column
#' @keywords assay
#' @export
#' @examples
#' calc_neut()
calc_neut <- function(assay_df) {
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

#' @title Update dilutions
#'
#' @description Update dilutions in main assay dataframe based on the input dilutions dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param dilutions dataframe, containing two columns for serum and control dilutions
#' @return dataframe, containing the updated dilutions
#' @keywords assay
#' @export
#' @examples
#' update_dilutions()
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

#' @title Update subjects
#'
#' @description Update subjects in main assay dataframe for a single plate based on the input plate dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param updated_plate_df dataframe, containing 96-well plate data
#' @param plate_n integer, plate number to update
#' @return dataframe, containing the updated subjects
#' @keywords assay
#' @export
#' @examples
#' update_subjects()
update_subjects <- function(assay_df, updated_plate_df, plate_n) {
  updated_subjects <- updated_plate_df[1, ]
  for (i in seq(1, length(updated_subjects))) {
    assay_df <- assay_df %>%
      dplyr::mutate(subject = ifelse((plate_number == plate_n) & (wcol == i), updated_subjects[i], subject))
  }
  return(assay_df)
}

#' @title Update types
#'
#' @description Update types in main assay dataframe for a single plate based on the input plate dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param updated_plate_df dataframe, containing 96-well plate data
#' @param plate_n integer, plate number to update
#' @return dataframe, containing the updated types
#' @keywords assay
#' @export
#' @examples
#' update_types()
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

#' @title Create feature dropdown
#'
#' @description Create feature dropdown that can be used to select an existing column from a dataframe
#' @param new_feature character, name of feature to update eg "bleed"
#' @param input object, R shiny server input parameter/object containing `plate_data` dataframe and `new_feature` character
#' @param values object, containing `assay_df` biological assay data from plate reader
#' @return dropdown of existing columns that can be used to define the `new_feature`
#' @keywords assay
#' @export
#' @examples
#' create_feature_dropdown()
create_feature_dropdown <- function(new_feature, input, values) {
  req(input$plate_data)
  assay_df <- isolate(values[["assay_df"]])
  selectInput(new_feature, "Select existing feature", names(assay_df))
}

#' @title Create feature table
#'
#' @description Create feature table that can be used to display existing levels for a selected column in a dataframe
#' @param new_feature character, name of feature to update eg "bleed"
#' @param input object, R shiny server input parameter/object containing `new_feature` character
#' @param values object, containing `assay_df` biological assay data from plate reader
#' @return rhandsontable interactive table of levles for selected feature/table in dataframe
#' @keywords assay
#' @export
#' @examples
#' create_feature_table()
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

#' @title Update feature
#'
#' @description For a given feature update the values in the main assay dataframe reactive values based on the input from the feature table
#' @param new_feature character, name of feature to update eg "bleed"
#' @param input object, R shiny server input parameter/object containing new_feature` character and `new_feature_table` dataframe
#' @param values object, containing `assay_df` biological assay data from plate reader
#' @return null, automatically updates the main assay dataframe within the reactive values
#' @keywords assay
#' @export
#' @examples
#' update_feature()
update_feature <- function(new_feature, input, values) {
  new_feature_table <- paste0(new_feature, "_table")
  req(new_feature_table)
  existing_feature <- input[[new_feature]]
  assay_df <- values[["assay_df"]]
  mappings_table <- hot_to_r(isolate(input[[new_feature_table]]))
  assay_df[[new_feature]] <- mappings_table[match(assay_df[[existing_feature]], mappings_table[[existing_feature]]), 2]
  values[["assay_df"]] <- assay_df
}

#' @title Assay to plate dataframe
#'
#' @description Convert full assay dataframe to 96-well plate format for a specified plate number
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param plate_n integer, plate number to generate dataframe for
#' @return dataframe, plate dataframe in 96-well plate format
#' @keywords assay
#' @export
#' @examples
#' assay_to_plate_df()
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
