#!/usr/bin/env Rscript

##############################################
# The server-side of the AutoPlate application
##############################################

##' Server main function
##'
##' @param input,output,session Internal parameters for {shiny}.
##'
##' @noRd
server <- function(input, output, session) {

  ##################
  # Define variables
  ##################
  values <- reactiveValues()
  report_filepath <- "report.Rmd"
  dilutions_filepath <- "data/dilutions.csv"
  dilutions <- read.csv(dilutions_filepath,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  #########
  # 0) Home
  #########

  output$steps <- renderText({
    HTML(paste0(
      "<li><b>Input</b> - upload the raw plate readouts for your 96 well-plates and specify what each well contained</li>",
      "<li><b>Quality control</b> - visualise the data you entered in step 1 and check that the controls have worked for each plate/well</li>",
      "<li><b>Results</b> - analyse the data and generate downloadable plots such as a Dose Response Curve</li>"
    ))
  })
  output$autoplate_version <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "1.0.0",
      width = 12,
      subtitle = "GitHub Release Version",
      icon = shiny::icon("box-open"),
      color = "light-blue"
    )
  })
  output$new_issue <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "New issue",
      width = 12,
      subtitle = "Go to GitHub issues",
      href = "https://github.com/PhilPalmer/AutoPlate/issues",
      icon = shiny::icon("github"),
      color = "maroon"
    )
  })
  output$email <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = "Email",
      width = 12,
      subtitle = "pp502@cam.ac.uk",
      icon = shiny::icon("envelope"),
      color = "green"
    )
  })

  ##########
  # 1) Input
  ##########

  # Helper func to create tooltips
  create_tooltip <- function(text) {
    HTML(paste0("<i class='fa fa-question-circle' title='", text, "'</i>"))
  }
  # Create tooltip icons
  output$tooltip_input_files <- renderText({
    create_tooltip("Raw plate readout CSV files specifying all of the wells and their luminescence values")
  })
  output$tooltip_dilutions <- renderText({
    create_tooltip("Dilutions will be used to set the corresponding rows in the 96-well plate")
  })
  output$tooltip_plates <- renderText({
    create_tooltip("Specify the samples (eg \"Mouse 1\") and type (eg \"x\") for each well
        c = cell only control
        m = monoclonal antibody (posotive control)
        v = virus (or pseudotype) only control
        x = serum sample")
  })
  output$tooltip_features <- renderText({
    create_tooltip("Set the values for new features such as the \"virus\" based on existing features such as the \"sample_id\" (i.e. mouse number)")
  })
  output$tooltip_exclude <- renderText({
    create_tooltip("You may wish to exclude certain wells/plates if they have failed the control for example")
  })
  output$tooltip_download_data <- renderText({
    create_tooltip("Export the full assay dataframe as a CSV")
  })
  output$tooltip_download_report <- renderText({
    create_tooltip("Export all QC and results plots as a shareable HTML file")
  })

  # Create messages to display to user
  output$message_input_files <- renderUI({
    if (is.null(values[["luminescence_files"]])) {
      box(HTML(paste0(
        "<h4>Start here!</h4><p>Upload your CSV files first to use the app, which meet the following criteria:<p>
            <ul>
                <li>Each file must contain the following columns: 
                    \"ID,SequenceID,WellPosition,ScanPosition,Tag,RLU,RLU(RQ),Timestamp(ms)\"
                <li>(Recommended) the file names end with the plate number, such as \"n1.csv\" for plate 1, 
                    for example: \"Luminescence Quick Read 2020.01.01 10_10_10 n1.csv\"
            </ul>
        </p>
        <h5>OR</h5>
        <button id=\"example_data\" type=\"button\" class=\"btn btn-default action-button\">
          Try with <a href=\"https://github.com/PhilPalmer/AutoPlate/blob/r-package/data/pmn_platelist_H1N1_example_data.csv\" target=\"_blank\" >example data</a>
        </button>
        "
        # TODO: update example data link to `main` branch once merged
      )), width = 12, background = "yellow")
    }
  })
  output$message_drm_string <- renderText({
    HTML("<p>Dose Response Model: specify the model for the dose response curve as per the <a href='https://www.rdocumentation.org/packages/drc/versions/2.5-12/topics/drm'>DRM function</a></p>")
  })

  # Use example data on click of button
  observeEvent(input$example_data, {
    values[["luminescence_files"]] <- structure(list(name = "example_H1N1_data_pmn_platelist.csv", 
      size = NA, type = "text/csv", datapath = "data/pmn_platelist_H1N1_example_data.csv"), 
      class = "data.frame", row.names = c(NA, -1L))
  })
  # Reload everything if the user uploads a new dataset
  observeEvent(input$luminescence_files, {
    values[["luminescence_files"]] <- input$luminescence_files
    values[["plate_data"]] <- NULL
    values[["assay_df"]] <- assay_df()
    # TODO: fix and uncomment this - add check to see if plate_data already exists?
    # values[["plate_data"]] <- plate_df()
  })

  # Create dropdown to select feature for plate data table
  output$plate_feature <- renderUI({
    req(values[["assay_df"]])
    assay_df <- isolate(values[["assay_df"]])
    selectInput("plate_feature", "Select feature", names(assay_df), "types")
  })

  # Create a tab for each uploaded plate
  output$plate_tabs <- renderUI({
    if (is.null(values[["luminescence_files"]])) {
      plates <- "NA - Please upload your CSV file(s) to display plates"
    } else {
      assay_df <- values[["assay_df"]]
      plates <- sort(unique(isolate(assay_df$plate_number)))
    }
    plate_tabs <- lapply(paste("Plate", plates), tabPanel)
    do.call(tabsetPanel, c(plate_tabs, id = "plate_tabs"))
  })

  # Create main dataframe for assay data
  assay_df <- reactive({
    req(values[["luminescence_files"]], input$plate_tabs)
    # Define variables
    luminescence_files <- values[["luminescence_files"]]
    header <- colnames(read.csv(luminescence_files$datapath[1], nrows = 1, header = TRUE))
    cols <- c("types", "dilution", "bleed")
    # Get the current plate number from the plate tab
    plate_n <- sub("^\\S+\\s+", "", input$plate_tabs)
    # Initialise plate number
    if (is.null(values[["plate_n"]])) values[["plate_n"]] <- plate_n
    # Record the plate number - we'll switch back to this later to prevent the plate tab changing for the user when they update the plate data 
    if (plate_n != values[["plate_n"]]) values[["plate_n"]] <- plate_n
    if (is.null(values[["plate_data"]]) & all(cols %in% header)) {
      assay_df <- read.csv(luminescence_files$datapath[1], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      # Update deprecated colnames if present
      oldnames <- c("subject", "inoculate", "primary", "study")
      newnames <- c("sample_id", "treatment", "virus", "experiment_id")
      if (all(oldnames %in% header)) {
        assay_df <- assay_df %>% dplyr::rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)
      }
      values[["assay_df"]] <- assay_df
    }
    # Initialise the main assay dataframe from the users uploaded luminescence files
    if (is.null(values[["plate_data"]]) & !all(cols %in% header)) {
      assay_df <-
        apply(luminescence_files, 1, function(df) read_plus(df["name"], df["datapath"])) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(plate_number = gsub(pattern = ".*n([0-9]+).csv", '\\1', tolower(filename))) %>%
        tidyr::separate(col = WellPosition, into = c("wrow", "wcol"), sep = ":")
      # Initialise new columns
      assay_df <- init_cols(assay_df)
      # Rename columns
      assay_df <- rename(assay_df, rlu = RLU, machine_id = ID, rlu.rq = RLU.RQ., timestamp = Timestamp.ms., sequence_id = SequenceID, scan_position = ScanPosition, tag = Tag)
      # Populate main assay df with types using the default plate layout
      assay_df <- init_types(assay_df)
      # Populate main assay df with default sample_id info
      assay_df <- init_sample_id(assay_df = assay_df, wcols = c(2,3), sample_id = 1)
      assay_df <- init_sample_id(assay_df = assay_df, wcols = c(4,5), sample_id = 2)
      assay_df <- init_sample_id(assay_df = assay_df, wcols = c(6,7), sample_id = 3)
      assay_df <- init_sample_id(assay_df = assay_df, wcols = c(8,9), sample_id = 4)
      assay_df <- init_sample_id(assay_df = assay_df, wcols = c(10,11), sample_id = 5)
      assay_df <- init_sample_id(assay_df = assay_df, wcols = c(12), sample_id = "Antibody")
      assay_df$sample_id <- ifelse(assay_df$wcol == 12, "Antibody", assay_df$sample_id)
      # Populate main assay df with concentration/dilution info
      assay_df <- update_dilutions(assay_df, dilutions)
      # Calculate normalised luminescence values
      assay_df <- calc_neut(assay_df)
      values[["assay_df"]] <- assay_df
    }
    # Update main assay dataframe with new dilutions
    if (!is.null(input$dilutions)) {
      assay_df <- values[["assay_df"]]
      dilutions <- hot_to_r(input$dilutions)
      assay_df <- update_dilutions(assay_df, dilutions)
      values[["assay_df"]] <- assay_df
    }
    # TODO: extract date
    return(assay_df)
  })

  # Update the sample_id and types of the main assay dataframe based on user input
  observeEvent(input$plate_data, {
    req(input$plate_tabs)
    values[["plate_data"]] <- input$plate_data
    plate_n <- values[["plate_n"]]
    assay_df <- values[["assay_df"]]
    # Check if the `changes$changes` is `NULL` to prevent bug #3
    # See more info here: https://github.com/PhilPalmer/AutoPlate/issues/3
    if (!(is.null(input$plate_data$changes$changes))) {
      # Catch errors because when plate 1 is not uploaded loading input$plate_data will initially throw an error
      tryCatch(
        {
          # updated_plate_df <- hot_to_r(input$plate_data)
          # # Update main assay dataframe with sample_id
          # assay_df <- update_sample_ids(assay_df, updated_plate_df, plate_n)
          # Update main assay dataframe with types
          # assay_df <- update_types(assay_df, updated_plate_df, plate_n)
          # # Update the neutralisation values
          # assay_df <- calc_neut(assay_df)
          # updateTabsetPanel(session, "plate_tabs", selected = paste("Plate", values[["plate_n"]]))
          # values[["assay_df"]] <- assay_df
        },
        error = function(error_message) {
          print(error_message)
        }
      )
    }
  })

  # Convert the luminescence rawdata -> (96) well plate format for the current plate tab
  plate_df <- reactive({
    req(values[["luminescence_files"]])
    plate_n <- values[["plate_n"]]
    assay_df <- assay_df()
    plate_df <- assay_to_plate_df(assay_df, plate_n, input$plate_feature)
    return(plate_df)
  })
  # Render plate data table
  output$plate_data <- renderRHandsontable({
    rhandsontable(plate_df(), stretchH = "all", useTypes = TRUE)
  })

  # Make dilutions table
  observe({
    if (!is.null(input[["dilutions"]])) {
      values[["previous"]] <- isolate(values[["dilutions"]])
      dilutions <- hot_to_r(input[["dilutions"]])
    } else {
      if (is.null(values[["dilutions"]])) {
        dilutions <- dilutions
      } else {
        dilutions <- values[["dilutions"]]
      }
    }
    values[["dilutions"]] <- dilutions
  })
  # Render dilutions table
  output$dilutions <- renderRHandsontable({
    dilutions <- values[["dilutions"]]
    row.names(dilutions) <- LETTERS[1:dim(dilutions)[1]]
    if (!is.null(dilutions)) {
      rhandsontable(dilutions, stretchH = "all")
    }
  })

  # Create dropdown for features: bleed, treatment, virus & experiment_id
  output$bleed <- renderUI(create_feature_dropdown("bleed", input, values))
  output$treatment <- renderUI(create_feature_dropdown("treatment", input, values))
  output$virus <- renderUI(create_feature_dropdown("virus", input, values))
  output$experiment_id <- renderUI(create_feature_dropdown("experiment_id", input, values))

  # Create table for features: bleed, treatment, virus & experiment_id
  output$bleed_table <- renderRHandsontable(create_feature_table("bleed", input, values))
  output$treatment_table <- renderRHandsontable(create_feature_table("treatment", input, values))
  output$virus_table <- renderRHandsontable(create_feature_table("virus", input, values))
  output$experiment_id_table <- renderRHandsontable(create_feature_table("experiment_id", input, values))

  # Update the main assay df with user input for features: bleed, treatment, virus & experiment_id
  observeEvent(input$go_bleed, update_feature("bleed", input, values))
  observeEvent(input$go_treatment, update_feature("treatment", input, values))
  observeEvent(input$go_virus, update_feature("virus", input, values))
  observeEvent(input$go_experiment_id, update_feature("experiment_id", input, values))

  #######
  # 2) QC
  #######

  # Download/export data to CSV
  output$download_data <- downloadHandler(
    # TODO: generate more unique name for file based on experiment ID etc.
    filename = function() {
      paste("pmn_platelist", ".csv", sep = "")
    },
    content = function(file) {
      assay_df <- assay_df()
      assay_df <- assay_df[order(assay_df$plate_number),]
      write.table(apply(assay_df, 2, as.character),
        file = file,
        append = FALSE,
        quote = FALSE,
        sep = ",",
        row.names = F,
        col.names = T
      )
    }
  )

  # Update main assay dataframe with excluded plates
  observeEvent(input$exclude_wells, {
    req(values[["luminescence_files"]])
    assay_df <- values[["assay_df"]]
    assay_df$exclude <- FALSE
    assay_df <- exclude_wells(assay_df, input$exclude_wells)
    values[["assay_df"]] <- assay_df
  })

  # Calculate average viral and cell luminescence
  output$av_lum <- renderTable({
    assay_df <- isolate(values[["assay_df"]])
    av_lum_df <- init_av_lum_df(assay_df)
    return(av_lum_df)
  })

  # Generate plots for each plate on change of the QC tabs
  observeEvent(input$tabset_qc, {
    feature <- gsub(" ", "_", tolower(input$tabset_qc), fixed = TRUE)
    assay_df <- isolate(values[["assay_df"]])
    plates <- sort(unique(assay_df$plate_number))
    values[["heatmap_input"]] <- list("feature" = feature, "plates" = plates)
    lapply(values[["heatmap_input"]]$plates, function(i) {
      output[[paste("plot", i, sep = "")]] <- renderPlot({
        assay_df <- isolate(values[["assay_df"]])
        plot_heatmap(i, assay_df, values[["heatmap_input"]]$feature, input$tabset_qc)
      })
    })
  })
  # Create divs to display heatmaps
  output$heatmaps <- renderUI({
    feature <- tolower(input$tabset_qc)
    if (feature != "average luminescence values") {
      plot_output_list <- lapply(values[["heatmap_input"]]$plates, function(i) {
        plotname <- paste("plot", i, sep = "")
        plotOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    }
  })

  # Generate types boxplot
   output$types_boxplot <- renderPlotly({
    req(values[["luminescence_files"]])
    assay_df <- values[["assay_df"]]
    types_boxplot <- init_types_boxplot(assay_df)
    plotly::ggplotly(types_boxplot) %>% layout(boxmode = "group")
  })

  ############
  # 3) Results
  ############

  # Download HTML report
  output$download_report <- downloadHandler(
    # TODO: generate more unique name for file based on experiment ID etc.
    filename = function() {
      paste("pmn_report", ".html", sep = "")
    },
    content = function(file) {
      shiny::withProgress(
        message = paste0("Generating Report Data"),
        value = 0,
        {
          shiny::incProgress(1 / 10)
          Sys.sleep(3)
          shiny::incProgress(5 / 10)
          write.table(apply(assay_df(), 2, as.character),
            file = file.path(tempdir(), "pmn_platelist.csv"),
            append = FALSE,
            quote = FALSE,
            sep = ",",
            row.names = F,
            col.names = T
          )
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy(report_filepath, tempReport, overwrite = TRUE)
          # Set up parameters to pass to Rmd document
          params <- list(drm_model = input$drm_string)
          # Write dataframe to file
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )

  # Generate raw R code output to display
  output$data_exploration_code <- renderUI({
    prism_code_block(code = data_exploration_code("all"), language = "r")
  })
  output$drc_code <- renderUI({
    prism_code_block(code = drc_code("all",input$drm_string,input$virus_drc), language = "r")
  })
  output$ic50_boxplot_code <- renderUI({
    prism_code_block(code = ic50_boxplot_code("all",input$drm_string,input$ic50_is_boxplot,input$virus_ic50), language = "r")
  })
  output$cv_boxplot_code <- renderUI({
    prism_code_block(code = cv_boxplot_code("all"), language = "r")
  })

  # Download plots
  output$download_data_exploration <- downloadHandler(
    filename = "data_exploration.svg",
    content = function(file) ggsave(file, plot = values[["data_exploration"]], width = 10)
  )
  output$download_drc <- downloadHandler(
    filename = "drc.svg",
    content = function(file) ggsave(file, plot = values[["drc"]], width = 10)
  )
  output$download_ic50 <- downloadHandler(
    filename = "ic50_boxplot.svg",
    content = function(file) ggsave(file, plot = values[["ic50_boxplot"]], width = 10)
  )
  output$download_cv_boxplot <- downloadHandler(
    filename = "cv_boxplot.svg",
    content = function(file) ggsave(file, plot = values[["cv_boxplot"]], width = 10)
  )

  # Create dropdown to select virus for DRC & IC50 plots
  # TODO: refactor create_feature_dropdown?
  output$virus_drc <- renderUI({
    req(values[["plate_data"]])
    assay_df <- isolate(values[["assay_df"]])
    selectInput("virus_drc", "Select virus to plot", unique(assay_df$virus))
  })
  output$virus_ic50 <- renderUI({
    req(values[["plate_data"]])
    assay_df <- isolate(values[["assay_df"]])
    selectInput("virus_ic50", "Select virus to plot", unique(assay_df$virus))
  })

  # Generate and render results plots
  output$data_exploration <- renderPlotly({
    req(values[["luminescence_files"]])
    data <- values[["assay_df"]]
    eval(parse(text=data_exploration_code("plot")))
    values[["data_exploration"]] <- data_exploration_plot
  })
  output$drc <- renderPlotly({
    req(values[["luminescence_files"]])
    data <- values[["assay_df"]]
    # Catch errors to prevent https://github.com/PhilPalmer/AutoPlate/issues/13
    tryCatch({
        eval(parse(text=drc_code("plot",input$drm_string,input$virus_drc)))
        values[["drc"]] <- drc_plot
      }, error = function(error_message) {
        print(error_message)
    })
  })
  output$ic50_boxplot <- renderPlotly({
    req(values[["luminescence_files"]])
    data <- values[["assay_df"]]
    # Catch errors to prevent https://github.com/PhilPalmer/AutoPlate/issues/13
    tryCatch({
      eval(parse(text=ic50_boxplot_code("plot",input$drm_string,input$ic50_is_boxplot,input$virus_ic50)))
      values[["ic50_boxplot"]] <- ic50_boxplot
      }, error = function(error_message) {
        print(error_message)
    })
  })
  output$cv_boxplot <- renderPlotly({
    req(values[["luminescence_files"]])
    data <- values[["assay_df"]]
    eval(parse(text=cv_boxplot_code("plot"))) 
    values[["cv_boxplot"]] <- cv_boxplot
    cv_boxplotly
  })
}
