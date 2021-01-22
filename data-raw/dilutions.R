#!/usr/bin/env Rscript

dilutions_filepath <- "data-raw/dilutions.csv"
dilutions <- utils::read.csv(dilutions_filepath,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
usethis::use_data(dilutions, overwrite = TRUE)
