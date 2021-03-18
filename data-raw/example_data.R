#!/usr/bin/env Rscript

pmn_platelist_filepath <- "data-raw/pmn_platelist_H1N1_example_data.csv"
pmn_platelist_H1N1_example_data <- utils::read.csv(pmn_platelist_filepath, 
  header=TRUE, 
  stringsAsFactors=FALSE, 
  check.names=FALSE
)
usethis::use_data(pmn_platelist_H1N1_example_data, overwrite = TRUE)

