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
    groups <- paste("Group",LETTERS[seq( from = 1, to = 6)])
    groups <- sort(c(paste0(groups,":1"),paste0(groups,":2")))
    metadata <- data.frame(matrix(0, nrow = 8, ncol = 12))
    names(metadata) <- groups
    rownames(metadata) <- c(paste0("V",1:5),paste0("C",1:3))
    
    # Make tables
    make_table(input,output,dilutions,"dilutions",dilution_values)
    make_table(input,output,metadata,"metadata",metadata_values)
}