#!/usr/bin/env Rscript

utils::globalVariables(c('dilutions_pmn', 'dilutions_ella', 'example_data_column_descriptions'))

#' Default dilutions pMN data
#'
#' A dataset containing the default pMN serum and control dilutions
#'
#' @docType data
#' @keywords datasets
#' @name dilutions_pmn
#' @usage data(dilutions_pmn)
#' @format A data frame with 8 rows and 2 variables
NULL

#' Default dilutions ELLA data
#'
#' A dataset containing the default ELLA serum dilutions
#'
#' @docType data
#' @keywords datasets
#' @name dilutions_ella
#' @usage data(dilutions_ella)
#' @format A data frame with 10 rows and 1 variable
NULL

#' pMN Platelist H1N1 Example Data
#'
#' A dataset containing example pMN platelist data for influenza H1N1
#'
#' @docType data
#' @keywords datasets
#' @name example_data_pmn_platelist_H1N1
#' @usage data(example_data_pmn_platelist_H1N1)
#' @format A data frame with 1152 rows and 14 variables, see more info on the variables [here](https://philpalmer.github.io/AutoPlate/articles/web_app.html#columns-1)
NULL

#' pMN Plate 1 Example Data
#'
#' A dataset containing an example pMN input plate data
#'
#' @docType data
#' @keywords datasets
#' @name example_data_pmn_plate_n1
#' @usage data(example_data_pmn_plate_n1)
#' @format A data frame containing 96 rows and 2 variables, see more info on the variables [here](https://philpalmer.github.io/AutoPlate/articles/web_app.html#columns-1)
NULL

#' Example data column descriptions
#'
#' A dataset containing an example data column descriptions
#'
#' @docType data
#' @keywords datasets
#' @name example_data_column_descriptions
#' @usage data(example_data_column_descriptions)
#' @format A data frame containing 13 rows and 4 variables
NULL