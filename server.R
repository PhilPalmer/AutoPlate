library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyr)
library(dplyr)

source("helpers/make_table.R")

function(input, output, sessions) {
    
    output$contents <- renderTable({
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
            tidyr::spread(key = WellCol, value = RLU) %>% 
            dplyr::arrange(as.numeric(WellRow)) %>% 
            dplyr::rename(Well = WellRow)
        return(luminescence_df)
    })

    
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