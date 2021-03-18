#!/usr/bin/env Rscript

pmn_platelist_filepath <- "data-raw/example_data_pmn_platelist_H1N1.csv"
example_data_pmn_platelist_H1N1 <- utils::read.csv(pmn_platelist_filepath,
  header=TRUE,
  stringsAsFactors=FALSE,
  check.names=FALSE
)
usethis::use_data(example_data_pmn_platelist_H1N1, overwrite = TRUE)

pmn_plate_filepath <- "data-raw/example_data_pmn_plate_n1.csv"
example_data_pmn_plate_n1 <- utils::read.csv(pmn_plate_filepath,
  header=TRUE,
  stringsAsFactors=FALSE,
  check.names=FALSE
)
usethis::use_data(example_data_pmn_plate_n1, overwrite = TRUE)
