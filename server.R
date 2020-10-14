library(shiny)
library(shinydashboard)
library(rhandsontable)

function(input, output, sessions) {
    
    dilutions_file <- "data/dilutions.csv"
    DF <- read.csv(dilutions_file, 
                        header           = TRUE, 
                        stringsAsFactors = FALSE, 
                        check.names      = FALSE)
    outdir <- getwd()
    outfilename <- "new_df"
    values <- reactiveValues()
    
    ## Handsontable
    observe({
        if (!is.null(input$hot)) {
            values[["previous"]] <- isolate(values[["DF"]])
            DF = hot_to_r(input$hot)
        } else {
            if (is.null(values[["DF"]]))
                DF <- DF
            else
                DF <- values[["DF"]]
        }
        values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
            rhandsontable(DF, stretchH = "all")
    })
    
}