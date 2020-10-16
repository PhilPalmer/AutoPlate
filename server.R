library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyr)
library(dplyr)

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
    
    # Make tables
    luminescence_df <- reactive({
        # input$luminescence_files will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$luminescence_files)
        luminescence_files <- input$luminescence_files$datapath
        luminescence_file_names <- input$luminescence_files$name
        luminescence_df <- read.csv(luminescence_files[1])
        luminescence_df <- luminescence_df %>% 
            tidyr::separate(col = WellPosition, into = c("WellCol", "WellRow"), sep = ":") %>%
            dplyr::select(WellCol, WellRow, RLU) %>%
            tidyr::spread(key = WellRow, value = RLU) %>%
            dplyr::rename(Well = WellCol)
        # Re-order cols
        luminescence_df <- luminescence_df[c("Well",sort(as.numeric(names(luminescence_df))))]
        return(luminescence_df)
    })
    make_table(input,output,dilutions,"dilutions",dilution_values,TRUE)
    make_table(input,output,luminescence_df(),"metadata",metadata_values,NULL)
}