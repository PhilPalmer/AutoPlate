#!/usr/bin/env Rscript

dilutions_pmn_filepath <- "data-raw/dilutions_pmn.csv"
dilutions_pmn <- utils::read.csv(dilutions_pmn_filepath,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  row.names = 1
)
usethis::use_data(dilutions_pmn, overwrite = TRUE)

dilutions_ella_filepath <- "data-raw/dilutions_ella.csv"
dilutions_ella <- utils::read.csv(dilutions_ella_filepath,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  row.names = 1
)
usethis::use_data(dilutions_ella, overwrite = TRUE)
