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
    
    # Create main dataframe for assay data
    assay_df <- reactive({
        req(input$luminescence_files, input$plate_tabs)
        luminescence_files <- input$luminescence_files
        plate_n <- sub("^\\S+\\s+", '', input$plate_tabs)
        assay_df <-
            apply(luminescence_files, 1, function(df) read_plus(df['name'],df['datapath'])) %>% 
            dplyr::bind_rows() %>%
            dplyr::mutate(plate_number = gsub(pattern=".*n([0-9]+).csv","\\1",filename)) %>%
            tidyr::separate(col = WellPosition, into = c("WellCol", "WellRow"), sep = ":")
        assay_df$types <- NA
        assay_df$subject <- NA
        assay_df <- assay_df %>% 
            # Populate main assay df with types using the default plate layout
            dplyr::mutate(types = case_when(
                WellRow == 1 & WellCol %in% c("A","B","C","D","E") ~ "v",
                WellRow == 1 & WellCol %in% c("F","G","H") ~ "c",
                WellRow %in% seq(2,11) ~ "x",
                WellRow == 12 ~ "m",
            )) %>% 
            # Populate main assay df with default subject info
            dplyr::mutate(subject = case_when(
                WellRow %in% c(2,3) ~ "Mouse 1",
                WellRow %in% c(4,5) ~ "Mouse 2",
                WellRow %in% c(6,7) ~ "Mouse 3",
                WellRow %in% c(8,9) ~ "Mouse 4",
                WellRow %in% c(10,11) ~ "Mouse 5",
                WellRow == 12 ~ "Antibody"
            ))
        # Update main assay dataframe with types
        # TODO: make changes persistent when switching plates
        if (!is.null(input$metadata)) {
            updated_luminescence_df <- hot_to_r(input$metadata)
            updated_subjects <- updated_luminescence_df[1,]
            for (i in seq(1,length(updated_subjects))) {
                assay_df <- assay_df %>% 
                    dplyr::mutate(subject = ifelse( (plate_number == plate_n) & (WellRow == i), updated_subjects[i], subject))
            }
        }
        # TODO: extract date
        return(assay_df)
    })
    
    # Convert the luminescence rawdata -> 96 well plate format for the current plate tab
    luminescence_df <- reactive({
        plate_number <- sub("^\\S+\\s+", '', input$plate_tabs)
        assay_df <- assay_df()
        luminescence_df <- assay_df[assay_df$plate_number == plate_number, ] %>%
            dplyr::select(WellCol, WellRow, types) %>%
            tidyr::spread(key = WellRow, value = types) %>%
            dplyr::rename(Well = WellCol)
        # Re-order cols
        luminescence_df <- luminescence_df[c("Well",sort(as.numeric(names(luminescence_df))))]
        # Get the subjects
        subjects <- assay_df[assay_df$plate_number == plate_number, ] %>%
            dplyr::filter(WellCol == "A") %>% 
            dplyr::select(subject)
        subject <- c("Subject", subjects$subject)
        # Reformat the row & col names + add a subject row
        names(subject) <- names(luminescence_df)
        luminescence_df <- rbind(subject, luminescence_df)
        row.names(luminescence_df) <- luminescence_df$Well
        luminescence_df[1] <- NULL
        return(luminescence_df)
    })
    
    # Make tables
    # TODO: refactor/generalize the `make_table()` func (& others?) so that it can be used for the dilutions & metadata tables
    make_table(input,output,dilutions,"dilutions",dilution_values,TRUE)
    output$metadata <- renderRHandsontable({
            rhandsontable(luminescence_df(), stretchH = "all", useTypes = TRUE)
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