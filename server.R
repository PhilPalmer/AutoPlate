library(shiny)
library(shinydashboard)
library(rhandsontable)

source("helpers/make_table.R")

function(input, output, sessions) {
    
    # Define variables
    dilutions_file <- "data/dilutions.csv"
    dilutions <- read.csv(dilutions_file, 
                          header           = TRUE, 
                          stringsAsFactors = FALSE, 
                          check.names      = FALSE)
    dilution_values <- reactiveValues()
    metadata_values <- reactiveValues()
    # Create empty df for metadata
    groups <- paste("Group",LETTERS[seq( from = 1, to = 12)])
    groups <- sort(c(paste0(groups,":1"),paste0(groups,":2")))
    metadata <- data.frame(matrix(0, nrow = 8, ncol = 24))
    names(metadata) <- groups
    
    # Make tables
    make_table(input,output,dilutions,"dilutions",dilution_values)
    make_table(input,output,metadata,"metadata",metadata_values)
}