#!/usr/bin/env Rscript

##############################################
# The server-side of the AutoPlate application
##############################################

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  ##################
  # Define variables
  ##################
  values <- reactiveValues()
  report_filepath <- "inst/app/www/report.Rmd"
  data(dilutions_pmn)
  data(dilutions_ella)

  #########
  # 0) Home
  #########

  # Create dropdown to select assay type
  output$assay_type <- renderUI({
    selectInput("assay_type", "Select the type of assay you wish to analyse", c("pMN", "ELLA"), "pMN")
  })
  observeEvent(input$assay_type, values[["assay_type"]] <- input$assay_type)

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
    data("example_data_column_descriptions")
    feature_description <- example_data_column_descriptions[example_data_column_descriptions$column_name==input$plate_feature,]$column_description
    create_tooltip(feature_description)
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
      HTML(paste0(
        "<button id=\"example_data\" type=\"button\" class=\"btn btn-default action-button\">
          Or try with example data!
        </button>
        "
      ))
    }
  })
  output$message_drm_string <- renderText({
    HTML("<p>Dose Response Model: specify the model for the dose response curve as per the <a href='https://www.rdocumentation.org/packages/drc/versions/2.5-12/topics/drm'>DRM function</a></p>")
  })

  # Use example data on click of button
  observeEvent(input$example_data, {
    values[["luminescence_files"]] <- structure(list(name = "example_data_pmn_platelist_H1N1.csv", 
      size = NA, type = "text/csv", datapath = "data-raw/example_data_pmn_platelist_H1N1.csv"), 
      class = "data.frame", row.names = c(NA, -1L))
  })
  # Reload everything if the user uploads a new dataset
  observeEvent(input$luminescence_files, {
    values[["luminescence_files"]] <- input$luminescence_files
    values[["plate_data"]] <- NULL
    values[["assay_df"]] <- assay_df()
    values[["plate_data"]] <- plate_df()
  })

  # Create dropdown to select feature for plate data table
  output$plate_feature <- renderUI({
    req(values[["assay_df"]])
    assay_df <- isolate(values[["assay_df"]])
    selectInput("plate_feature", "Select feature", names(assay_df), "types")
  })

  # Create a tab for each uploaded plate
  # The selected plate will be set to the first plate when the data is first uploaded
  output$plate_tabs <- renderUI({
    if (is.null(values[["luminescence_files"]])) {
      plates <- "NA - Please upload your CSV file(s) to display plates"
      values[["selected_plate"]] <- plates[1]
    } else {
      assay_df <- values[["assay_df"]]
      values[["plate_count"]] <- isolate(values[["plate_count"]]) + 1
      plates <- unique(isolate(assay_df$plate_number))
      if (length(plates) > 0) plates <- plates[order(nchar(plates), plates)]
      plates <- c("Template", paste("Plate", plates))
      if (startsWith(toString(isolate(values[["selected_plate"]])),"NA ")) {
        values[["selected_plate"]] <- plates[2]
      } else { values[["selected_plate"]] <- input$plate_tabs }
    }
    plate_tabs <- lapply(plates, tabPanel)
    do.call(tabsetPanel, c(plate_tabs, id = "plate_tabs", selected = values[["selected_plate"]]))
  })

  # Create template dataframe
  template_df <- reactive({
    req(values[["assay_df"]])
    if (is.null(values[["template_df"]])) {
      template_df <- values[["assay_df"]]
      # Get just the first plate
      template_df <- template_df[template_df$plate_number == unique(sort(template_df$plate_number))[1],]
      # Initialise columns
      cols <- setdiff(colnames(template_df), c("wcol", "wrow"))
      template_df[cols] <- NA
      template_df$plate_number <- "template"
      # TODO: Initialise types/samples/concentrations?
      # Save template_df reactive values
      values[["template_df"]] <- template_df
    } else {
      # Load existing template_df
      template_df <- values[["template_df"]]
    }
    return(template_df)
  })

  # Create main dataframe for assay data
  assay_df <- reactive({
    req(values[["luminescence_files"]], input$plate_tabs)
    # Define variables
    luminescence_files <- values[["luminescence_files"]]
    cols <- c("types", "bleed")
    if (tolower(values[["assay_type"]]) == "ella") {
      header <- c()
    } else {
      header <- colnames(utils::read.csv(luminescence_files$datapath[1], nrows = 1, header = TRUE))
    }
    # Get the current plate number from the plate tab
    plate_n <- sub("^\\S+\\s+", "", input$plate_tabs)
    # Initialise plate number
    if (is.null(values[["plate_n"]])) values[["plate_n"]] <- plate_n
    # Record the plate number & feature - we'll switch back to these later to prevent the plate tab changing for the user when they update the plate data 
    if (plate_n != values[["plate_n"]]) values[["plate_n"]] <- plate_n
    feature <- input$plate_feature
    if (is.null(feature)) values[["plate_feature"]] <- "types"
    if (feature != values[["plate_feature"]] && !is.null(feature)) values[["plate_feature"]] <- feature
    if (is.null(values[["plate_data"]]) & all(cols %in% header)) {
      assay_df <- utils::read.csv(luminescence_files$datapath[1], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      # Update deprecated colnames if present
      oldnames <- c("subject","mouse","inoculate","inoc","primary","study","dil","neu","plate")
      newnames <- c("sample_id","sample_id","treatment","treatment","virus","experiment_id","dilution","neutralisation","plate_number")
      assay_df <- replace_names(assay_df, oldnames, newnames)
      assay_df <- init_cols(assay_df)
      values[["assay_df"]] <- assay_df
    }
    # Initialise the main assay dataframe from the users uploaded luminescence files
    if (is.null(values[["plate_data"]]) & !all(cols %in% header)) {
      assay_df <- init_assay_df(values[["luminescence_files"]], values[["assay_type"]])
      # Initialise new columns
      assay_df <- init_cols(assay_df)
      # Rename columns
      assay_df <- dplyr::rename_all(assay_df, dplyr::recode, RLU="rlu", ID="machine_id", RLU.RQ.="rlu.rq", Timestamp.ms.="timestamp", SequenceID="sequence_id", ScanPosition="scan_position", Tag="tag")
      # Populate main assay df with types using the default plate layout
      assay_df <- init_types(assay_df, values[["assay_type"]])
      # Populate main assay df with default sample_id info
      assay_df <- init_samples(assay_df, values[["assay_type"]])
      # Populate main assay df with concentration/dilution info
      assay_df <- update_dilutions(assay_df, dilutions(), values[["assay_type"]])
      # Calculate normalised luminescence values
      assay_df <- calc_neut(assay_df)
      # Ensure the assay dataframe types are correct
      assay_df <- update_col_types(assay_df, example_data_column_descriptions)
      values[["assay_df"]] <- assay_df
    }
    # TODO: extract date
    assay_df <- values[["assay_df"]]
    return(assay_df)
  })

  # Update main assay dataframe with new dilutions
  observeEvent(input$dilutions, {
    req(values[["luminescence_files"]])
    if (!is.null(input$dilutions)) {
      assay_df <- values[["assay_df"]]
      dilutions <- rhandsontable::hot_to_r(input$dilutions)
      assay_df <- update_dilutions(assay_df, dilutions, values[["assay_type"]])
      values[["assay_df"]] <- assay_df
    }
  })

  # Update the sample_id and types of the main assay dataframe based on user input
  observeEvent(input$plate_data, {
    req(input$plate_tabs)
    values[["plate_data"]] <- input$plate_data
    plate_n <- values[["plate_n"]]
    assay_df <- values[["assay_df"]]
    template_df <- values[["template_df"]]
    changes <- input$plate_data$changes$changes
    # Check if the `changes$changes` is `NULL` to prevent bug #3
    # See more info here: https://github.com/PhilPalmer/AutoPlate/issues/3
    if (!(is.null(input$plate_data$changes$changes))) {
      # Catch errors because when plate 1 is not uploaded loading input$plate_data will initially throw an error
      tryCatch(
        {
          # Update main assay dataframe with updated values for selected feature
          if (plate_n == "Template") {
            # Update all plates
            assay_df <- update_feature_plate(assay_df, input$plate_feature, "all", changes)
            # Update the template
            template_df <- update_feature_plate(template_df, input$plate_feature, tolower(plate_n), changes)
            values[["template_df"]] <- template_df
          } else {
            assay_df <- update_feature_plate(assay_df, input$plate_feature, plate_n, changes)
          }
          # Update the neutralisation values
          assay_df <- calc_neut(assay_df)
          # Update the plate tab and feature dropdown to the previous value 
          # Otherwise it would be changed back to the default when updating the main assay dataframe 
          updateTabsetPanel(session, "plate_tabs", selected = paste("Plate", values[["plate_n"]]))
          updateSelectInput(session, "plate_feature", selected = values[["plate_feature"]])
          assay_df <- update_col_types(assay_df, example_data_column_descriptions)
          values[["assay_df"]] <- assay_df
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
    feature <- values[["plate_feature"]]
    if (plate_n == "Template") {
      template_df <- template_df()
      plate_df <- assay_to_plate_df(template_df, tolower(plate_n), feature)
    } else {
      plate_df <- assay_to_plate_df(assay_df, plate_n, feature)
    }
    if (feature == "types") {
      plate_df[] <- lapply(plate_df, factor, levels = c("","c","m","v","x"))
    } else if (feature == "exclude") {
      plate_df[] <- lapply(plate_df, as.logical)
    } else if (feature == "dilution") {
      plate_df[] <- lapply(plate_df, as.integer)
    }
    else {
      plate_df[] <- lapply(plate_df, as.character)
    }
    return(plate_df)
  })
  # Render plate data table
  output$plate_data <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(plate_df(), stretchH = "all", useTypes = TRUE)
  })

  # Make dilutions table
  dilutions <- reactive({
    if (is.null(values[["dilutions"]])) {
      dilutions <- if (tolower(values[["assay_type"]]) == "ella") dilutions_ella else dilutions_pmn
    } else {
      dilutions <- values[["dilutions"]]
    }
    return(dilutions)
  })
  # If user changes the assay type update the dilutions
  observeEvent(input$assay_type, {
    values[["dilutions"]] <- NULL
    values[["dilutions"]] <- dilutions()
  })
  # Update dilutions based on user input
  observeEvent(input$dilutions, {
    if (!is.null(input[["dilutions"]])) {
      # Catch errors to prevent https://github.com/PhilPalmer/AutoPlate/issues/28
      tryCatch({
          dilutions <- rhandsontable::hot_to_r(input[["dilutions"]])
        }, error = function(error_message) {
          print(error_message)
      })
    }
  })
  # Render dilutions table
  output$dilutions <- rhandsontable::renderRHandsontable({
    values[["dilutions"]] <- dilutions()
    if (!is.null(dilutions)) {
      rhandsontable::rhandsontable(values[["dilutions"]], stretchH = "all")
    }
  })

  # Display RLU values for the currently selected plate
  output$plate_preview <- renderPlot({
    req(values[["luminescence_files"]])
    plot_heatmap(values[["plate_n"]], isolate(values[["assay_df"]]), "rlu", "RLU")
  })

  # Create dropdown for features: bleed, treatment, virus & experiment_id
  output$bleed <- renderUI(create_feature_dropdown("bleed", input, values))
  output$treatment <- renderUI(create_feature_dropdown("treatment", input, values))
  output$virus <- renderUI(create_feature_dropdown("virus", input, values))
  output$experiment_id <- renderUI(create_feature_dropdown("experiment_id", input, values))

  # Create table for features: bleed, treatment, virus & experiment_id
  output$bleed_table <- rhandsontable::renderRHandsontable(create_feature_table("bleed", input, values))
  output$treatment_table <- rhandsontable::renderRHandsontable(create_feature_table("treatment", input, values))
  output$virus_table <- rhandsontable::renderRHandsontable(create_feature_table("virus", input, values))
  output$experiment_id_table <- rhandsontable::renderRHandsontable(create_feature_table("experiment_id", input, values))

  # Update the main assay df with user input for features: bleed, treatment, virus & experiment_id
  observeEvent(input$go_bleed, update_feature("bleed", input, values))
  observeEvent(input$go_treatment, update_feature("treatment", input, values))
  observeEvent(input$go_virus, update_feature("virus", input, values))
  observeEvent(input$go_experiment_id, update_feature("experiment_id", input, values))

  # Creature table to display which features have been entered
  output$features_table <- formattable::renderFormattable({
    req(values[["luminescence_files"]])
    assay_df <- values[["assay_df"]]
    features <- c("types", "sample_id", "dilution", "virus", "treatment", "bleed", "experiment_id")
    features_df <- setNames(data.frame(matrix(ncol = 2, nrow = length(features))), c("Feature", "Entered"))
    features_df$Feature <- features
    for (feature in features) {
      empty <- all(unique(as.character(assay_df[[feature]])) %in% c(NA, ""))
      features_df[features_df$Feature == feature,]$Entered <- if(empty) FALSE else TRUE
    }
    formattable::formattable(features_df, list(
      Entered = formattable::formatter("span",
        style = x ~ formattable::style(color = ifelse(x, "green", "red")),
        x ~ formattable::icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
    ))
  })

  #######
  # 2) QC
  #######

  # Download/export all data to CSV
  output$download_data <- downloadHandler(
    # TODO: generate more unique name for file based on experiment ID etc.
    filename = function() {
      paste0("pmn_platelist", ".csv")
    },
    content = function(file) {
      assay_df <- assay_df()
      assay_df <- assay_df[order(assay_df$plate_number),]
      utils::write.table(apply(assay_df, 2, as.character),
        file = file,
        append = FALSE,
        quote = TRUE,
        sep = ",",
        row.names = F,
        col.names = T
      )
    }
  )
  # Download/export data for PRISM to CSV
  output$download_prism <- downloadHandler(
    filename = function() {
      paste0("plate_neutralisations", ".csv")
    },
    content = function(file) {
      assay_df <- assay_df()
      assay_df <- assay_df[order(assay_df$plate_number),]
      prism_df <- assay_to_prism_df(assay_df)
      utils::write.table(prism_df,
        file = file,
        append = FALSE,
        quote = TRUE,
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
  heatmap_listen <- reactive({
    list(input$tabset_qc,input$tab)
  })
  observeEvent(heatmap_listen(), {
    if (input$tab == "qc") {
      feature <- gsub(" ", "_", tolower(input$tabset_qc), fixed = TRUE)
      assay_df <- isolate(values[["assay_df"]])
      plates <- unique(assay_df$plate_number)
      if (length(plates) > 0) plates <- plates[order(nchar(plates), plates)]
      values[["heatmap_input"]] <- list("feature" = feature, "plates" = plates)
      lapply(values[["heatmap_input"]]$plates, function(i) {
        output[[paste("plot", i, sep = "")]] <- renderPlot({
          assay_df <- isolate(values[["assay_df"]])
          if (!(feature %in% c("average luminescence values","types_boxplot"))) {
            plot_heatmap(i, assay_df, values[["heatmap_input"]]$feature, input$tabset_qc)
          }
        })
      })
    }
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
   output$types_boxplot <- plotly::renderPlotly({
    req(values[["luminescence_files"]])
    assay_df <- values[["assay_df"]]
    types_boxplot <- init_types_boxplot(assay_df)
    plotly::ggplotly(types_boxplot) %>% plotly::layout(boxmode = "group")
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
          utils::write.table(apply(assay_df(), 2, as.character),
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
    content = function(file) ggplot2::ggsave(file, plot = values[["data_exploration"]], width = 10, height = 8)
  )
  output$download_drc <- downloadHandler(
    filename = "drc.svg",
    content = function(file) ggplot2::ggsave(file, plot = values[["drc"]], width = 10, height = 8)
  )
  output$download_ic50 <- downloadHandler(
    filename = "ic50_boxplot.svg",
    content = function(file) ggplot2::ggsave(file, plot = values[["ic50_boxplot"]], width = 10, height = 8)
  )
  output$download_ied <- downloadHandler(
    filename = "ied.csv",
    content = function(file) {
      req(values[["luminescence_files"]])
      assay_df <- values[["assay_df"]]
      assay_df$dilution <- as.numeric(as.character(assay_df$dilution))
      tryCatch({
        all_ieds = data.frame()
        for (virus_to_keep in unique(assay_df$virus)) {
          data <- dplyr::filter(assay_df, types %in% c("x", "m"), exclude == FALSE, virus == virus_to_keep)
          model <- eval(parse(text=paste0("drc::drm(",input$drm_string,")")))
          plot_type <- if(input$ic50_is_boxplot) "boxplot" else "jitter"
          ied <- plot_ic50_boxplot(data, model, plot_type)$ied
          all_ieds <- dplyr::bind_rows(all_ieds,ied)
        }
        all_ieds <- all_ieds[match(row.names(all_ieds)[order(nchar(row.names(all_ieds)), row.names(all_ieds))], row.names(all_ieds)),]
      }, error = function(error_message) {
        print(error_message)
      })
      utils::write.table(all_ieds, file=file, append=FALSE, quote=FALSE, sep=",", row.names=F, col.names=T)
    }
  )
  output$download_cv_boxplot <- downloadHandler(
    filename = "cv_boxplot.svg",
    content = function(file) ggplot2::ggsave(file, plot = values[["cv_boxplot"]], width = 10, height = 8)
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
  output$data_exploration <- plotly::renderPlotly({
    if (input$tab == "results") {
      req(values[["luminescence_files"]])
      data <- values[["assay_df"]]
      tryCatch({
        data$dilution <- as.numeric(as.character(data$dilution))
        data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
        values[["data_exploration"]] <- plot_data_exploration(data, text_size=strtoi(input$plot_text_size))
        m <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)
        plotly::ggplotly(values[["data_exploration"]])  %>% plotly::layout(autosize = F, width = 1000, height = 800, margin = m)
      }, error = function(error_message) {
        shiny::validate(toString(error_message))
      })
    }
  })
  output$drc <- plotly::renderPlotly({
    if (input$tab == "results") {
      req(values[["luminescence_files"]])
      data <- values[["assay_df"]]
      data$dilution <- as.numeric(as.character(data$dilution))
      # Catch errors to prevent https://github.com/PhilPalmer/AutoPlate/issues/13
      tryCatch({
        virus_to_plot <- input$virus_drc
        data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE, virus == virus_to_plot)
        model <- eval(parse(text=paste0("drc::drm(",input$drm_string,")")))
        values[["drc"]] <- plot_drc(data, model, text_size=strtoi(input$plot_text_size))
        drc_plotly <- plotly::ggplotly(values[["drc"]])
        m <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)
        drc_plotly <- drc_plotly %>% plotly::layout(autosize = F, width = 1000, height = 800, margin = m)
        drc_plotly 
      }, error = function(error_message) {
        shiny::validate(toString(error_message))
      })
    }
  })
  output$ic50_boxplot <- plotly::renderPlotly({
    if (input$tab == "results") {
      req(values[["luminescence_files"]])
      data <- values[["assay_df"]]
      data$dilution <- as.numeric(as.character(data$dilution))
      # Catch errors to prevent https://github.com/PhilPalmer/AutoPlate/issues/13
      tryCatch({
        virus_to_plot <- input$virus_ic50
        data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE, virus == virus_to_plot)
        model <- eval(parse(text=paste0("drc::drm(",input$drm_string,")")))
        plot_type <- if(input$ic50_is_boxplot) "boxplot" else "jitter"
        ic50_boxplot <- plot_ic50_boxplot(data, model, plot_type, text_size=strtoi(input$plot_text_size))
        ic50_boxplotly <- plotly::ggplotly(ic50_boxplot$ic50_boxplot)
        values[["ic50_boxplot"]] <- ic50_boxplot$ic50_boxplot
        m <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)
        ic50_boxplotly <- ic50_boxplotly %>% plotly::layout(autosize = F, width = 1000, height = 800, margin = m)
        ic50_boxplotly 
        }, error = function(error_message) {
          shiny::validate(toString(error_message))
      })
    }
  })
  output$cv_boxplot <- plotly::renderPlotly({
    if (input$tab == "results") {
      req(values[["luminescence_files"]])
      data <- values[["assay_df"]]
      tryCatch({
        data <- dplyr::filter(data, types %in% c("c", "v"), exclude == FALSE)  %>%
          dplyr::mutate(types = ifelse( (types == "c"), "cell", types)) %>%
          dplyr::mutate(types = ifelse( (types == "v"), "virus", types))
        values[["cv_boxplot"]] <- plot_cv_boxplot(data, text_size=strtoi(input$plot_text_size))
        cv_boxplotly <- plotly::ggplotly(values[["cv_boxplot"]]) 
        m <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)
        cv_boxplotly <- cv_boxplotly %>% plotly::layout(boxmode = "group", autosize = F, width = 1000, height = 800, margin = m)
        cv_boxplotly
      }, error = function(error_message) {
        shiny::validate(toString(error_message))
      })
    }
  })
}