library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyr)
library(dplyr)

##########
# 1) Input
##########
source("helpers/make_table.R")
# Define helper func to read CSVs + append name
read_plus <- function(name,file) {
    read.csv(file) %>% 
        dplyr::mutate(filename = name)
}
update_dilutions <- function(assay_df,dilutions) {
    assay_df %>% 
        dplyr::mutate(dilution = case_when(
            wrow == "A" & types == "x" ~ dilutions[1,1],
            wrow == "B" & types == "x" ~ dilutions[2,1],
            wrow == "C" & types == "x" ~ dilutions[3,1],
            wrow == "D" & types == "x" ~ dilutions[4,1],
            wrow == "E" & types == "x" ~ dilutions[5,1],
            wrow == "F" & types == "x" ~ dilutions[6,1],
            wrow == "G" & types == "x" ~ dilutions[7,1],
            wrow == "H" & types == "x" ~ dilutions[8,1],
            wrow == "A" & types == "m" ~ dilutions[1,2],
            wrow == "B" & types == "m" ~ dilutions[2,2],
            wrow == "C" & types == "m" ~ dilutions[3,2],
            wrow == "D" & types == "m" ~ dilutions[4,2],
            wrow == "E" & types == "m" ~ dilutions[5,2],
            wrow == "F" & types == "m" ~ dilutions[6,2],
            wrow == "G" & types == "m" ~ dilutions[7,2],
            wrow == "H" & types == "m" ~ dilutions[8,2]
        ))
}
create_feature_dropdown <- function(new_feature,input,values) {
    req(input$plate_data)
    assay_df <- isolate(values[["assay_df"]])
    selectInput(new_feature, "Select existing feature", names(assay_df))
}
create_feature_table <- function(new_feature,input,values) {
    req(input[[new_feature]])
    assay_df <- isolate(values[["assay_df"]])
    feature_levels <- levels(as.factor(unlist(assay_df[[input[[new_feature]]]])))
    new_feature_df <- data.frame(matrix(unlist(feature_levels), nrow=length(feature_levels), byrow=T))
    names(new_feature_df) <- input[[new_feature]]
    new_feature_df[[new_feature]] <- as.character(NA)
    rhandsontable(new_feature_df, stretchH = "all", rowHeaders = NULL)
}
update_feature <- function(new_feature,input,values) {
    new_feature_table <- paste0(new_feature, "_table")
    req(new_feature_table)
    existing_feature <- input[[new_feature]]
    assay_df <- values[["assay_df"]]
    mappings_table <- hot_to_r(isolate(input[[new_feature_table]]))
    assay_df[[new_feature]] <- mappings_table[match(assay_df[[existing_feature]], mappings_table[[existing_feature]]),2]
    values[["assay_df"]] <- assay_df
}

#######
# 2) QC
#######
plot_heatmap <- function(values,plate_number,feature,col,fmt.cell) {
    assay_df <- isolate(values[["assay_df"]])
    plate_df <- isolate(assay_df[assay_df$plate_number == plate_number, ])
    vals <- matrix(plate_df[[feature]],byrow=T,ncol=12,nrow=8)
    plot(vals, col=col, fmt.cell=fmt.cell, main=feature)
}

function(input, output, sessions) {

    ##########
    # 1) Input
    ##########
    
    # Define variables
    dilutions_file <- "data/dilutions.csv"
    dilutions <- read.csv(dilutions_file, 
                          header           = TRUE, 
                          stringsAsFactors = FALSE, 
                          check.names      = FALSE)
    dilution_values <- reactiveValues()
    values <- reactiveValues()
    
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
        if (is.null(input$plate_data)){
            assay_df <-
                apply(luminescence_files, 1, function(df) read_plus(df['name'],df['datapath'])) %>% 
                dplyr::bind_rows() %>%
                dplyr::mutate(plate_number = gsub(pattern=".*n([0-9]+).csv","\\1",filename)) %>%
                tidyr::separate(col = WellPosition, into = c("wrow", "wcol"), sep = ":")
            assay_df$types <- NA
            assay_df$subject <- NA
            assay_df$dilution <- NA
            assay_df$bleed <- NA
            assay_df$inoculate <- NA
            assay_df$primary <- NA
            assay_df$study <- NA
            assay_df <- assay_df %>% 
                # Populate main assay df with types using the default plate layout
                dplyr::mutate(types = case_when(
                    wcol == 1 & wrow %in% c("A","B","C","D","E") ~ "v",
                    wcol == 1 & wrow %in% c("F","G","H") ~ "c",
                    wcol %in% seq(2,11) ~ "x",
                    wcol == 12 ~ "m",
                )) %>% 
                # Populate main assay df with default subject info
                dplyr::mutate(subject = case_when(
                    wcol %in% c(2,3) ~ "Mouse 1",
                    wcol %in% c(4,5) ~ "Mouse 2",
                    wcol %in% c(6,7) ~ "Mouse 3",
                    wcol %in% c(8,9) ~ "Mouse 4",
                    wcol %in% c(10,11) ~ "Mouse 5",
                    wcol == 12 ~ "Antibody"
                ))
            # Populate main assay df with concentration/dilution info
            assay_df <- update_dilutions(assay_df,dilutions)
            values[["assay_df"]] <- assay_df
        }
        if (!is.null(input$plate_data)) {
            assay_df <- values[["assay_df"]]
            updated_plate_df <- hot_to_r(input$plate_data)
            # Update main assay dataframe with subject
            updated_subjects <- updated_plate_df[1,]
            for (i in seq(1,length(updated_subjects))) {
                assay_df <- assay_df %>% 
                    dplyr::mutate(subject = ifelse( (plate_number == plate_n) & (wcol == i), updated_subjects[i], subject))
            }
            # Update main assay dataframe with types
            updated_types <- tail(updated_plate_df,-1)
            for (col in seq(1,length(updated_types))) {
                full_col = updated_types[,col]
                for (i in seq(1,length(full_col))) {
                    row = row.names(updated_types)[i]
                    assay_df <- assay_df %>% 
                        dplyr::mutate(types = ifelse( (plate_number == plate_n) & (wcol == col) & (wrow == row), updated_types[row,col], types))
                }
            }
            values[["assay_df"]] <- assay_df
        }
        # Update main assay dataframe with new dilutions
        if (!is.null(input$dilutions)) {
            assay_df <- values[["assay_df"]]
            dilutions <- hot_to_r(input$dilutions)
            assay_df <- update_dilutions(assay_df,dilutions)
            values[["assay_df"]] <- assay_df
        }        
        # TODO: extract date
        return(assay_df)
    })
    
    # Convert the luminescence rawdata -> (96) well plate format for the current plate tab
    plate_df <- reactive({
        req(input$luminescence_files)
        plate_number <- sub("^\\S+\\s+", '', input$plate_tabs)
        assay_df <- assay_df()
        plate_df <- isolate(assay_df[assay_df$plate_number == plate_number, ]) %>%
            dplyr::select(wrow, wcol, types) %>%
            tidyr::spread(key = wcol, value = types) %>%
            dplyr::rename(Well = wrow)
        # Re-order cols
        plate_df <- plate_df[c("Well",sort(as.numeric(names(plate_df))))]
        # Get the subjects
        subjects <- isolate(assay_df[assay_df$plate_number == plate_number, ]) %>%
            dplyr::filter(wrow == "A") %>% 
            dplyr::select(subject)
        subject <- c("Subject", subjects$subject)
        # Reformat the row & col names + add a subject row
        names(subject) <- names(plate_df)
        plate_df <- rbind(subject, plate_df)
        row.names(plate_df) <- plate_df$Well
        plate_df[1] <- NULL
        return(plate_df)
    })
    
    # Make tables
    # TODO: refactor/generalize the `make_table()` func (& others?) so that it can be used for the dilutions & plate_data tables
    make_table(input,output,dilutions,"dilutions",dilution_values,TRUE)
    output$plate_data <- renderRHandsontable({
        rhandsontable(plate_df(), stretchH = "all", useTypes = TRUE)
    })
    
    # Create dropdown for features: bleed, inoculate, primary & study
    output$bleed     <- renderUI(create_feature_dropdown("bleed",input,values))
    output$inoculate <- renderUI(create_feature_dropdown("inoculate",input,values))
    output$primary   <- renderUI(create_feature_dropdown("primary",input,values))
    output$study     <- renderUI(create_feature_dropdown("study",input,values))
    
    # Create table for features: bleed, inoculate, primary & study
    output$bleed_table     <- renderRHandsontable(create_feature_table("bleed",input,values))
    output$inoculate_table <- renderRHandsontable(create_feature_table("inoculate",input,values))
    output$primary_table   <- renderRHandsontable(create_feature_table("primary",input,values))
    output$study_table     <- renderRHandsontable(create_feature_table("study",input,values))
    
    # Update the main assay df with user input for features: bleed, inoculate, primary & study
    observeEvent(input$go_bleed,update_feature("bleed",input,values))
    observeEvent(input$go_inoculate,update_feature("inoculate",input,values))
    observeEvent(input$go_primary,update_feature("primary",input,values))
    observeEvent(input$go_study,update_feature("study",input,values))

    # Download/export data to CSV 
    output$downloadData <- downloadHandler(
        # TODO: generate more unique name for file based on experiment ID etc.
        filename = function() {
            paste("pmn_platelist", ".csv", sep = "")
        },
        content = function(file) {
            write.table(apply(assay_df(),2,as.character),  
                file      = file,
                append    = FALSE, 
                quote     = FALSE, 
                sep       = ",",
                row.names = F,
                col.names = T)
        }
    )

    #######
    # 2) QC
    #######
    output$types <- renderPlot(plot_heatmap(values,1,"types",rainbow,"%.5s"))

}