library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyr)
library(dplyr)

source("helpers/make_table.R")
# Define helper func to read CSVs + append name
read_plus <- function(name,file) {
    read.csv(file) %>% 
        dplyr::mutate(filename = name)
}

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
    
    # Create main dataframe
    assay_df <- reactive({
        req(input$luminescence_files)
        luminescence_files <- input$luminescence_files
        assay_df <-
            apply(luminescence_files, 1, function(df) read_plus(df['name'],df['datapath'])) %>% 
            dplyr::bind_rows() %>%
            dplyr::mutate(plate_number = gsub(pattern=".*n([0-9]+).csv","\\1",filename))
        # TODO: extract date
        return(assay_df)
    })
    
    # Convert the luminescence rawdata -> 96 well plate format for the current plate tab
    luminescence_df <- reactive({
        plate_number <- sub("^\\S+\\s+", '', input$plate_tabs)
        assay_df <- assay_df()
        luminescence_df <- assay_df[assay_df$plate_number == plate_number, ] %>%
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
            rhandsontable(luminescence_df(), stretchH = "all", rowHeaders = NULL, useTypes = FALSE)
    })
    
    # Create dropdown for bleed
    output$bleed <- renderUI({
        selectInput("bleed", "Select existing feature", names(assay_df()))
    })
    
    # Create bleed table
    output$bleed_table <- renderRHandsontable({
        req(input$bleed)
        assay_df <- assay_df()
        feature_levels <- levels(as.factor(assay_df[[input$bleed]]))
        bleed_df <- data.frame(matrix(unlist(feature_levels), nrow=length(feature_levels), byrow=T))
        names(bleed_df) <- input$bleed
        bleed_df$bleed <- as.character(NA)
        rhandsontable(bleed_df, stretchH = "all", rowHeaders = NULL)
    })
}