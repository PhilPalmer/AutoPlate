#!/usr/bin/env Rscript

#' @title Read plus
#'
#' @description Read CSV file appending the filename as an additional column in the dataframe
#' @param name File name to be added to the dataframe
#' @param file Path to the CSV file
#' @return A dataframe generated from the input CSV containing the filenames
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
#' @param assay_df Dataframe containing biological assay data from plate reader
#' @return A dataframe containing the initialised columns
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