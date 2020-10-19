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
    
    # Create a tab for each uploaded plate
    output$plate_tabs = renderUI({
        if (is.null(input$luminescence_files)) {
            n_luminescence_files = 1
        } else {
            n_luminescence_files = length(input$luminescence_files$name)
        }
        plate_tabs = lapply(paste('Plate', 1: n_luminescence_files), tabPanel)
        do.call(tabsetPanel, c(plate_tabs,id = "plate_tabs"))
    })
    
    # Read the luminescence raw data based on the selection of the current plate tab
    luminescence_raw_df <- reactive({
        # input$luminescence_files will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$luminescence_files)
        luminescence_files <- input$luminescence_files$datapath
        luminescence_file_names <- input$luminescence_files$name
        plate_n <- sub("^\\S+\\s+", '', input$plate_tabs)
        luminescence_raw_df <- read.csv(luminescence_files[[as.numeric(plate_n)]])
        return(luminescence_raw_df)
    })
    # Process the luminescence rawdata -> 96 well plate format
    luminescence_df <- reactive({
        luminescence_df <- luminescence_raw_df() %>% 
            tidyr::separate(col = WellPosition, into = c("WellCol", "WellRow"), sep = ":") %>%
            dplyr::select(WellCol, WellRow, RLU) %>%
            tidyr::spread(key = WellRow, value = RLU) %>%
            dplyr::rename(Well = WellCol)
        # Re-order cols
        luminescence_df <- luminescence_df[c("Well",sort(as.numeric(names(luminescence_df))))]
        return(luminescence_df)
    })
    
    # Make tables
    # TODO: refactor/generalize the `make_table()` func (& others?) so that it can be used for the dilutions & metadata tables
    make_table(input,output,dilutions,"dilutions",dilution_values,TRUE)
    output$metadata <- renderRHandsontable({
            rhandsontable(luminescence_df(), stretchH = "all", rowHeaders = NULL)
    })
}