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
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#' @export
read_plus <- function(filename, filepath) {
  read.csv(filepath) %>%
    dplyr::mutate(filename = filename)
}

#' @title ELLA to assay dataframe
#'
#' @description Read ELLA data from an Excel file and convert to assay dataframe format appending the filename as an additional column
#' @param filename character, file name to be added to the dataframe
#' @param filepath character, path to the CSV file
#' @return dataframe, generated from the input Excel file containing the filenames
#' @keywords read input Excel files
#' @importFrom magrittr %>%
#' @export
ella_to_assay_df <- function(filename, filepath) {
  raw <- as.data.frame(readxl::read_excel(filepath))
  if (all(is.na(raw[1,]))) raw <- as.data.frame(readxl::read_excel(filepath, skip=2))
  rownames(raw) <- raw[,1]
  raw <- raw[,2:ncol(raw)]
  assay_df <- expand.grid(names(raw), rownames(raw))
  # Rename columns to well column and well row
  names(assay_df) <- c("wcol", "wrow")
  # Relative Light Units
  assay_df$rlu <- as.vector(t(raw))
  # When the well exceeds 3.5 the machine retuns ( + ) convert to 3.51
  assay_df$rlu[assay_df$rlu == "( + )"] <- 3.51
  assay_df$rlu <- as.numeric(assay_df$rlu)
  assay_df %>% dplyr::mutate(filename = filename)
}

#' @title Initialise assay dataframe
#'
#' @description Read input files spectified from a dataframe and convert to assay dataframe format appending the filename as an additional column
#' @param luminescence_files dataframe, containing columns "name" and "datapath" for input file names and path to the file respectively
#' @param assay_type character, type of assay eg "pMN" or "ELLA" (default = "pMN")
#' @return dataframe, generated from the files containing the filenames
#' @keywords read input files assay dataframe
#' @importFrom magrittr %>%
#' @export
init_assay_df <- function(luminescence_files, assay_type="pMN") {
  if (tolower(assay_type) == "ella") {
    assay_df <-
        apply(luminescence_files, 1, function(df) ella_to_assay_df(df["name"], df["datapath"])) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(plate_number = gsub(pattern = ".*n([0-9]+).xls", '\\1', tolower(filename)))
  } else {
    assay_df <-
        apply(luminescence_files, 1, function(df) read_plus(df["name"], df["datapath"])) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(plate_number = gsub(pattern = ".*n([0-9]+).csv", '\\1', tolower(filename))) %>%
        tidyr::separate(col = WellPosition, into = c("wrow", "wcol"), sep = ":")
  }
  return(assay_df)
}

#' @title Init cols
#'
#' @description Initialise columns for assay dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @return dataframe, containing the initialised columns
#' @keywords assay
#' @export
init_cols <- function(assay_df) {
  assay_df$filename <- gsub(".csv","",assay_df$filename)
  assay_df$types <- ""
  assay_df$sample_id <- ""
  assay_df$dilution <- ""
  assay_df$bleed <- ""
  assay_df$treatment <- ""
  assay_df$virus <- ""
  assay_df$experiment_id <- ""
  assay_df$neutralisation <- as.numeric("")
  assay_df$exclude <- FALSE
  return(assay_df)
}

#' @title Init types
#'
#' @description Initialise types (c,m,v,x) for assay dataframe based on template 96-well plate format
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param assay_type character, type of assay eg "pMN" or "ELLA" (default = "pMN")
#' @return dataframe, containing the initialised type column
#' @keywords assay
#' @export
init_types <- function(assay_df, assay_type="pMN") {
  if (tolower(assay_type) == "ella") {
    assay_df <- assay_df %>%
      dplyr::mutate(types = dplyr::case_when(
        wcol %in% 1:10 ~ "x",
        wcol == 11 ~ "v",
        wcol == 12 ~ "c"
      ))
  } else {
    assay_df <- assay_df %>%
      dplyr::mutate(types = dplyr::case_when(
        wcol == 1 & wrow %in% c("A", "B", "C", "D", "E") ~ "v",
        wcol == 1 & wrow %in% c("F", "G", "H") ~ "c",
        wcol %in% 2:11 ~ "x",
        wcol == 12 ~ "m"
      ))
  }
  return(assay_df)
}

#' @title Init sample ID
#'
#' @description Initialise sample_id (mouse number) for assay dataframe based on input parameters
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param wells vector, list of well column numbers and row letters to update for each plate
#' @param sample_id character, name to set sample_id to for specified columns
#' @param n_samples integer, total number of samples per plate (default = 5)
#' @return dataframe, containing the initialised sample_id column
#' @keywords assay
#' @importFrom magrittr %>%
#' @export
init_sample <- function(assay_df, wells, sample_id, n_samples=5) {
  plates_not_numbered <- all(is.na(as.numeric(assay_df$plate_number)))
  if (plates_not_numbered) {
    assay_df <- assay_df %>% dplyr::mutate(rank = dplyr::dense_rank(plate_number))
    assay_df$plate_number <- assay_df$rank
  }
  if (sample_id != "Antibody") {
    assay_df$sample_id <- ifelse(
      assay_df$wcol %in% wells | (assay_df$wrow %in% wells & !assay_df$wcol %in% c(11,12)), paste("Mouse", (as.numeric(assay_df$plate_number) - 1) * n_samples + sample_id), assay_df$sample_id
    )
  } else {
    assay_df$sample_id <- ifelse( assay_df$wcol %in% wells, sample_id, assay_df$sample_id )
  }
  if (plates_not_numbered) {
    assay_df$plate_number <- assay_df$filename
    assay_df$rank <- NA
  }
  return(assay_df)
}

#' @title Init samples
#'
#' @description Initialise all sample_ids (mouse numbers) for assay dataframe based on assay type
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param assay_type character, type of assay eg "pMN" or "ELLA"
#' @return dataframe, containing the initialised samples in sample_id column
#' @keywords assay
#' @export
init_samples <- function(assay_df, assay_type) {
  if (tolower(assay_type) == "ella") {
    assay_df <- init_sample(assay_df = assay_df, wells = c("A","B"), sample_id = 1, n_samples=4)
    assay_df <- init_sample(assay_df = assay_df, wells = c("C","D"), sample_id = 2, n_samples=4)
    assay_df <- init_sample(assay_df = assay_df, wells = c("E","F"), sample_id = 3, n_samples=4)
    assay_df <- init_sample(assay_df = assay_df, wells = c("G","H"), sample_id = 4, n_samples=4)
  } else {
    assay_df <- init_sample(assay_df = assay_df, wells = c(2,3), sample_id = 1)
    assay_df <- init_sample(assay_df = assay_df, wells = c(4,5), sample_id = 2)
    assay_df <- init_sample(assay_df = assay_df, wells = c(6,7), sample_id = 3)
    assay_df <- init_sample(assay_df = assay_df, wells = c(8,9), sample_id = 4)
    assay_df <- init_sample(assay_df = assay_df, wells = c(10,11), sample_id = 5)
    assay_df <- init_sample(assay_df = assay_df, wells = c(12), sample_id = "Antibody")
    assay_df$sample_id <- ifelse(assay_df$wcol == 12, "Antibody", assay_df$sample_id)
  }
  return(assay_df)
}

#' @title Calculate neutralisation
#'
#' @description Calculate neutralisation using the RLU values and normalisation with the virus and cell only controls
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @return dataframe, containing the initialised neutralisation column
#' @keywords assay
#' @importFrom stats median
#' @export
calc_neut <- function(assay_df) {
  plates <- unique(assay_df$plate_number)
  for (plate_n in plates) {
    plate_df <- assay_df[assay_df$plate_number == plate_n, ]
    # max & min levels of cell infection
    neu0 <- median(plate_df[plate_df$types == "v", ]$rlu)
    neu100 <- median(plate_df[plate_df$types == "c", ]$rlu)
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
#' @param assay_type character, type of assay eg "pMN" or "ELLA" (default = "pMN")
#' @return dataframe, containing the updated dilutions
#' @keywords assay
#' @importFrom magrittr %>%
#' @export
update_dilutions <- function(assay_df, dilutions, assay_type="pMN") {
  if (tolower(assay_type) == "ella") {
    assay_df %>% dplyr::mutate(dilution = dplyr::case_when(
      wcol == "1" & types == "x" ~ dilutions[1, 1],
      wcol == "2" & types == "x" ~ dilutions[2, 1],
      wcol == "3" & types == "x" ~ dilutions[3, 1],
      wcol == "4" & types == "x" ~ dilutions[4, 1],
      wcol == "5" & types == "x" ~ dilutions[5, 1],
      wcol == "6" & types == "x" ~ dilutions[6, 1],
      wcol == "7" & types == "x" ~ dilutions[7, 1],
      wcol == "8" & types == "x" ~ dilutions[8, 1],
      wcol == "9" & types == "x" ~ dilutions[9, 1],
      wcol == "10" & types == "x" ~ dilutions[10, 1]
    ))
  } else {
    assay_df %>% dplyr::mutate(dilution = dplyr::case_when(
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
}

#' @title Update sample_ids
#'
#' @description Update sample_ids in main assay dataframe for a single plate based on the input plate dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param updated_plate_df dataframe, containing 96-well plate data
#' @param plate_n integer, plate number to update
#' @return dataframe, containing the updated sample_ids
#' @keywords assay
#' @importFrom magrittr %>%
#' @export
update_sample_ids <- function(assay_df, updated_plate_df, plate_n) {
  updated_sample_ids <- updated_plate_df[1, ]
  for (i in seq(1, length(updated_sample_ids))) {
    assay_df <- assay_df %>%
      dplyr::mutate(sample_id = ifelse((plate_number == plate_n) & (wcol == i), updated_sample_ids[i], sample_id))
  }
  return(assay_df)
}

#' @title Update types
#'
#' @description Update types in main assay dataframe for a single plate based on the input plate dataframe
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param feature character, containing column to update in assay dataframe
#' @param plate_n integer, plate number to update
#' @param changes vector, containg sub vectors each corresponding to a change, which contain 0-index based: c(row, col, old_val, new_val)
#' @return dataframe, containing the updated feature
#' @keywords assay
#' @export
update_feature_plate <- function(assay_df, feature, plate_n, changes) {
  for (change in changes) {
    row = LETTERS[ change[[1]] + 1 ]
    col = change[[2]] + 1
    old_value = change[[3]]
    new_value = change[[4]]
    if (plate_n == "all") {
      plates_text <- "For all plates "
      condition <- TRUE
    } else {
      plates_text <- paste("For plate", plate_n)
      condition <- assay_df$plate_number == plate_n
    }
    print(paste0(plates_text, ", well ", row, col, ", updating ", feature, " from '", old_value, "' -> '", new_value, "'"))
    assay_df[[feature]][condition & assay_df$wcol == col & assay_df$wrow == row] = new_value
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
#' @noRd
create_feature_dropdown <- function(new_feature, input, values) {
  req(values[["plate_data"]])
  assay_df <- isolate(values[["assay_df"]])
  message <- paste("Enter the",new_feature,"for each:")
  features <- c("well", names(assay_df))
  selected <- if (new_feature == "treatment") "sample_id" else features[1]
  selectInput(new_feature, message, features, selected)
}

#' @title Create feature table
#'
#' @description Create feature table that can be used to display existing levels for a selected column in a dataframe
#' @param new_feature character, name of feature to update eg "bleed"
#' @param input object, R shiny server input parameter/object containing `new_feature` character
#' @param values object, containing `assay_df` biological assay data from plate reader
#' @return rhandsontable interactive table of levles for selected feature/table in dataframe
#' @keywords assay
#' @noRd
create_feature_table <- function(new_feature, input, values) {
  req(input[[new_feature]])
  assay_df <- values[["assay_df"]]
  if (tolower(input[[new_feature]]) == "well") {
    new_feature_df <- setNames(data.frame(matrix(ncol = 2, nrow = 1)), c("Wells to update", new_feature))
    new_feature_df["Wells to update"] <- "all"
  } else {
    feature_levels <- levels(as.factor(unlist(assay_df[[input[[new_feature]]]])))
    feature_levels <- feature_levels[order(nchar(feature_levels), feature_levels)]
    new_feature_df <- data.frame(matrix(unlist(feature_levels), nrow = length(feature_levels), byrow = T))
    names(new_feature_df) <- input[[new_feature]]
  }
  new_feature_df[[new_feature]] <- as.character(NA)
  rhandsontable::rhandsontable(new_feature_df, stretchH = "all", rowHeaders = NULL)
}

#' @title Update feature
#'
#' @description For a given feature update the values in the main assay dataframe reactive values based on the input from the feature table
#' @param new_feature character, name of feature to update eg "bleed"
#' @param input object, R shiny server input parameter/object containing new_feature` character and `new_feature_table` dataframe
#' @param values object, containing `assay_df` biological assay data from plate reader
#' @return null, automatically updates the main assay dataframe within the reactive values
#' @keywords assay
#' @noRd
update_feature <- function(new_feature, input, values) {
  tryCatch({
    existing_feature <- input[[new_feature]]
    assay_df <- values[["assay_df"]]
    if (tolower(existing_feature) == "well") {
      new_feature_text <- paste0(new_feature, "_text")
      assay_df[[new_feature]] <- input[[new_feature_text]]
    } else {
      new_feature_table <- paste0(new_feature, "_table")
      mappings_table <- rhandsontable::hot_to_r(isolate(input[[new_feature_table]]))
      assay_df[[new_feature]] <- mappings_table[match(assay_df[[existing_feature]], mappings_table[[existing_feature]]), 2]
    }
    values[["assay_df"]] <- assay_df
  }, error = function(error_message) {
    print(error_message)
  })
}

#' @title Assay to plate dataframe
#'
#' @description Convert full assay dataframe to 96-well plate format for a specified plate number
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param plate_n integer, plate number to generate dataframe for
#' @param feature feature to generate the plate data format for eg `types`
#' @return dataframe, plate dataframe in 96-well plate format
#' @keywords assay
#' @export
assay_to_plate_df <- function(assay_df, plate_n, feature) {
  # TODO: update sample names?
  # TODO: update documentation
  plate_df <- isolate(assay_df[assay_df$plate_number == plate_n, ])
  plate_df <- as.data.frame(matrix(plate_df[[feature]], byrow=T, ncol=12, nrow=8))
  row.names(plate_df) <- LETTERS[1:length(row.names(plate_df))]
  return(plate_df)
}
