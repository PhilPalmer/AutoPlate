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
    values <- reactiveValues()
    
    # Make tables
    make_table(input,output,dilutions,"dilutions")
    
}