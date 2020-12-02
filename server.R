library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyr)
library(dplyr)
library(plot.matrix)
library(viridis)
library(ggplot2)
library(drc)
library(rmarkdown)
library(knitr)
library(svglite)
library(plotly)

# Import helper scripts
source("helpers/1_input.R")
source("helpers/2_qc.R")
source("helpers/3_results.R")

# Define helper func
create_tooltip <- function(text) {
  HTML(paste0("<i class='fa fa-question-circle' title='", text, "'</i>"))
}

function(input, output, session) {

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

  # Define variables
  dilutions_file <- "data/dilutions.csv"
  dilutions <- read.csv(dilutions_file,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  values <- reactiveValues()

  # Create tooltip icons
  output$tooltip_input_files <- renderText({
    create_tooltip("Raw plate readout CSV files specifying all of the wells and their luminescence values")
  })
  output$tooltip_dilutions <- renderText({
    create_tooltip("Dilutions will be used to set the corresponding rows in the 96-well plate")
  })
  output$tooltip_plates <- renderText({
    create_tooltip("Specify the subject (eg \"Mouse 1\") and type (eg \"x\") for each well
        c = cell only control
        m = monoclonal antibody (posotive control)
        v = virus (or pseudotype) only control
        x = serum sample")
  })
  output$tooltip_features <- renderText({
    create_tooltip("Set the values for new features such as the \"primary\" based on existing features such as the \"subject\" (i.e. mouse number)")
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
    if (is.null(input$luminescence_files)) {
      box(HTML(paste0(
        "<h4>Start here!</h4><p>Upload your CSV files first to use the app, which meet the following criteria:<p>
            <ul>
                <li>Each file must contain the following columns: 
                    \"ID,SequenceID,WellPosition,ScanPosition,Tag,RLU,RLU(RQ),Timestamp(ms)\"
                <li>(Recommended) the file names end with the plate number, such as \"n1.csv\" for plate 1, 
                    for example: \"Luminescence Quick Read 2020.01.01 10_10_10 n1.csv\"
            </ul>
        </p>"
      )), width = 12, background = "yellow")
    }
  })
  output$message_drm_string <- renderText({
    HTML("<p>Dose Response Model: specify the model for the dose response curve as per the <a href='https://www.rdocumentation.org/packages/drc/versions/2.5-12/topics/drm'>DRM function</a></p>")
  })
  # Create a tab for each uploaded plate
  output$plate_tabs <- renderUI({
    if (is.null(input$luminescence_files)) {
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
    req(input$luminescence_files, input$plate_tabs)
    # Define variables
    luminescence_files <- input$luminescence_files
    header <- colnames(read.csv(luminescence_files$datapath[1], nrows = 1, header = TRUE))
    cols <- c("types", "subject", "dilution", "bleed", "inoculate", "primary", "study")
    # Get the current plate number from the plate tab
    plate_n <- sub("^\\S+\\s+", "", input$plate_tabs)
    # Initialise plate number
    if (is.null(values[["plate_n"]])) values[["plate_n"]] <- plate_n
    # Record the plate number - we'll switch back to this later to prevent the plate tab changing for the user when they update the plate data 
    if (plate_n != values[["plate_n"]]) values[["plate_n"]] <- plate_n
    if (is.null(input$plate_data) & all(cols %in% header)) {
      assay_df <- read.csv(luminescence_files$datapath[1], header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      values[["assay_df"]] <- assay_df
    }
    # Initialise the plate data frame
    if (is.null(input$plate_data) & !all(cols %in% header)) {
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
      # Populate main assay df with default subject info
      assay_df <- init_subject(assay_df = assay_df, wcol1 = 2, wcol2 = 3, subject = 1)
      assay_df <- init_subject(assay_df = assay_df, wcol1 = 4, wcol2 = 5, subject = 2)
      assay_df <- init_subject(assay_df = assay_df, wcol1 = 6, wcol2 = 7, subject = 3)
      assay_df <- init_subject(assay_df = assay_df, wcol1 = 8, wcol2 = 9, subject = 4)
      assay_df <- init_subject(assay_df = assay_df, wcol1 = 10, wcol2 = 11, subject = 5)
      assay_df$subject <- ifelse(assay_df$wcol == 12, "Antibody", assay_df$subject)
      # Populate main assay df with concentration/dilution info
      assay_df <- update_dilutions(assay_df, dilutions)
      # Calculate normalised luminescence values
      assay_df <- init_neut(assay_df)
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

  # Update the subject and types of the main assay dataframe based on user input
  observeEvent(input$plate_data, {
    req(input$plate_tabs)
    plate_n <- values[["plate_n"]]
    assay_df <- values[["assay_df"]]
    # Check if the `changes$changes` is `NULL` to prevent bug #3
    # See more info here: https://github.com/PhilPalmer/AutoPlate/issues/3
    if (!(is.null(input$plate_data$changes$changes))) {
      # Catch errors because when plate 1 is not uploaded loading input$plate_data will initially throw an error
      tryCatch(
        {
          updated_plate_df <- hot_to_r(input$plate_data)
          # Update main assay dataframe with subject
          assay_df <- update_subjects(assay_df, updated_plate_df, plate_n)
          # Update main assay dataframe with types
          updated_types <- tail(updated_plate_df, -1)
          for (col in seq(1, length(updated_types))) {
            full_col <- updated_types[, col]
            for (i in seq(1, length(full_col))) {
              row <- row.names(updated_types)[i]
              updated_type <- updated_types[row, col]
              current_type <- dplyr::filter(assay_df, (plate_number == plate_n) & (wcol == col) & (wrow == row))["types"]
              if (current_type != updated_type) {
                print(paste0("For plate ", plate_n, ", well ", row, col, ", updating type ", current_type, " -> ", updated_type))
                assay_df <- assay_df %>%
                  dplyr::mutate(types = ifelse((plate_number == plate_n) & (wcol == col) & (wrow == row), updated_types[row, col], types))
              }
            }
          }
          updateTabsetPanel(session, "plate_tabs", selected = paste("Plate", values[["plate_n"]]))
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
    req(input$luminescence_files)
    plate_number <- values[["plate_n"]]
    assay_df <- assay_df()
    plate_df <- isolate(assay_df[assay_df$plate_number == plate_number, ]) %>%
      dplyr::select(wrow, wcol, types) %>%
      tidyr::spread(key = wcol, value = types) %>%
      dplyr::rename(Well = wrow)
    # Re-order cols
    plate_df <- plate_df[c("Well", sort(as.numeric(names(plate_df))))]
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
  output[["dilutions"]] <- renderRHandsontable({
    dilutions <- values[["dilutions"]]
    if (!is.null(dilutions)) {
      rhandsontable(dilutions, stretchH = "all", rowHeaders = TRUE)
    }
  })
  # Make plate data table
  output$plate_data <- renderRHandsontable({
    rhandsontable(plate_df(), stretchH = "all", useTypes = TRUE)
  })

  # Create dropdown for features: bleed, inoculate, primary & study
  output$bleed <- renderUI(create_feature_dropdown("bleed", input, values))
  output$inoculate <- renderUI(create_feature_dropdown("inoculate", input, values))
  output$primary <- renderUI(create_feature_dropdown("primary", input, values))
  output$study <- renderUI(create_feature_dropdown("study", input, values))

  # Create table for features: bleed, inoculate, primary & study
  output$bleed_table <- renderRHandsontable(create_feature_table("bleed", input, values))
  output$inoculate_table <- renderRHandsontable(create_feature_table("inoculate", input, values))
  output$primary_table <- renderRHandsontable(create_feature_table("primary", input, values))
  output$study_table <- renderRHandsontable(create_feature_table("study", input, values))

  # Update the main assay df with user input for features: bleed, inoculate, primary & study
  observeEvent(input$go_bleed, update_feature("bleed", input, values))
  observeEvent(input$go_inoculate, update_feature("inoculate", input, values))
  observeEvent(input$go_primary, update_feature("primary", input, values))
  observeEvent(input$go_study, update_feature("study", input, values))

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
    req(input$luminescence_files)
    assay_df <- values[["assay_df"]]
    assay_df$exclude <- FALSE
    assay_df <- exclude_wells(assay_df, input$exclude_wells)
    values[["assay_df"]] <- assay_df
  })
  # Calculate average viral and cell luminescence
  output$av_lum <- renderTable({
    assay_df <- isolate(values[["assay_df"]])
    # TODO: add plates to reactive values so that it can be accessed globally
    plates <- sort(unique(assay_df$plate_number))
    av_cell_lum <- lapply(plates, function(plate_n) mean(dplyr::filter(assay_df, (plate_number == plate_n) & (types == "c"))$rlu))
    av_viral_lum <- lapply(plates, function(plate_n) mean(dplyr::filter(assay_df, (plate_number == plate_n) & (types == "v"))$rlu))
    no_cell_wells <- lapply(plates, function(plate_n) sum(dplyr::filter(assay_df, (plate_number == plate_n))$types == "c"))
    no_viral_wells <- lapply(plates, function(plate_n) sum(dplyr::filter(assay_df, (plate_number == plate_n))$types == "v"))
    av_lum_df <- do.call(rbind, Map(data.frame,
      plate_number = plates,
      average_cell_luminescence = av_cell_lum,
      average_viral_luminescence = av_viral_lum,
      number_of_cell_wells = no_cell_wells,
      number_of_virus_wells = no_viral_wells
    ))
    return(av_lum_df)
  })
  # Generate plots for each plate on change of the QC tabs
  observeEvent(input$tabset_qc, {
    feature <- tolower(input$tabset_qc)
    assay_df <- isolate(values[["assay_df"]])
    plates <- sort(unique(assay_df$plate_number))
    values[["heatmap_input"]] <- list("feature" = feature, "plates" = plates)
    lapply(values[["heatmap_input"]]$plates, function(i) {
      output[[paste("plot", i, sep = "")]] <- renderPlot({
        plot_heatmap(i, values, values[["heatmap_input"]]$feature, input$tabset_qc)
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
    req(input$luminescence_files)
    assay_df <- values[["assay_df"]]
    assay_df <- assay_df %>%
      dplyr::filter(exclude == FALSE) %>%
      dplyr::mutate(types = ifelse((types == "c"), "cell", types)) %>%
      dplyr::mutate(types = ifelse((types == "m"), "monoclonal antibody", types)) %>%
      dplyr::mutate(types = ifelse((types == "v"), "virus", types)) %>%
      dplyr::mutate(types = ifelse((types == "x"), "serum sample", types))
    assay_df$plate_number <- as.factor(assay_df$plate_number)
    values[["types_boxplot"]] <- ggplot2::ggplot(assay_df, aes(x = plate_number, y = rlu, colour = types)) +
      geom_boxplot() +
      geom_point(position=position_dodge(0.75)) +
      ylab("Raw luminescence value") +
      xlab("Plate number") +
      theme_classic() +
      ggtitle(paste(unique(assay_df$study), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$primary)))
    plotly::ggplotly(ggplot2::last_plot()) %>% layout(boxmode = "group")
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
          file.copy("report.Rmd", tempReport, overwrite = TRUE)
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
    prism_code_block(code = print_data_exploration_code(), language = "r")
  })
  output$drc_code <- renderUI({
    prism_code_block(code = print_drc_code(input$drm_string), language = "r")
  })
  output$ic50_boxplot_code <- renderUI({
    prism_code_block(code = print_ic50_boxplot_code(input$drm_string), language = "r")
  })
  output$cv_boxplot_code <- renderUI({
    prism_code_block(code = print_cv_boxplot_code(), language = "r")
  })
  # Download plots
  output$download_data_exploration <- downloadHandler(
    filename = "data_exploration.svg",
    content = function(file) ggsave(file, plot = values[["data_exploration"]])
  )
  output$download_drc <- downloadHandler(
    filename = "drc.svg",
    content = function(file) ggsave(file, plot = values[["drc"]])
  )
  output$download_ic50 <- downloadHandler(
    filename = "ic50.svg",
    content = function(file) ggsave(file, plot = values[["ic50"]])
  )
  output$download_cv_boxplot <- downloadHandler(
    filename = "cv_boxplot.svg",
    content = function(file) ggsave(file, plot = values[["cv_boxplot"]])
  )
  # Generate plots to display
  output$data_exploration <- renderPlotly({
    req(input$luminescence_files)
    assay_df <- values[["assay_df"]]
    assay_df <- dplyr::filter(assay_df, types %in% c("x", "m"), exclude == FALSE)
    values[["data_exploration"]] <- ggplot2::ggplot(assay_df, aes(x = dilution, y = neutralisation, colour = inoculate)) +
      geom_point() +
      geom_smooth(se = F, span = 1) +
      facet_wrap(. ~ primary) +
      ylim(c(-100, 110)) +
      scale_x_continuous(trans = "log10") +
      theme_classic() +
      ylab("Neutralisation") +
      xlab("Dilution") +
      ggtitle(paste(unique(assay_df$study), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$primary)))
    plotly::ggplotly(ggplot2::last_plot())
  })
  output$drc <- renderPlotly({
    req(input$luminescence_files)
    data <- values[["assay_df"]]
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE) # TODO: filter for each primary
    data$subject <- unlist(data$subject)
    model <- eval(parse(text = paste("drc::drm(", input$drm_string, ")")))
    # plot(model, type="all", col=TRUE)

    n <- 100
    new_dilution <- exp(seq(log(min(data$dilution)), log(max(data$dilution)), length.out = n))
    subjects <- unique(data$subject)
    new_data <- expand.grid(new_dilution, subjects)
    names(new_data) <- c("dilution", "subject")
    new_data$inoculate <- data$inoculate[match(new_data$subject, data$subject)]
    new_data$pred <- predict(model, newdata = new_data, )
    facets <- if(length(unique(data$primary))>1) c("inoculate","primary") else c("inoculate")
    # inoculate_cols <- gg_color_hue(10) # TODO: make controls different colour
    # ccs <- c('grey', inoculate_cols[1], inoculate_cols[2], inoculate_cols[3], inoculate_cols[4],
    #         inoculate_cols[5], inoculate_cols[6], inoculate_cols[7], inoculate_cols[8], inoculate_cols[9], inoculate_cols[10],
    #         'black')

    values[["drc"]] <- ggplot2::ggplot(new_data, aes(x = dilution, y = pred, colour = inoculate, group = subject)) +
      geom_line() +
      geom_point(data = data, aes(y = neutralisation)) +
      facet_wrap(facets) +
      scale_x_continuous(trans = "log10") +
      # scale_colour_manual(values=ccs) +
      theme_classic() +
      ylab("Neutralisation") +
      xlab("Dilution") +
      ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    plotly::ggplotly(ggplot2::last_plot())
  })
  output$ic50_boxplot <- renderPlotly({
    req(input$luminescence_files)
    data <- values[["assay_df"]]
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE) # TODO: filter for each primary?
    data$subject <- unlist(data$subject)
    model <- eval(parse(text = paste("drc::drm(", input$drm_string, ")")))

    ied <- as.data.frame(ED(model, 50, display = FALSE))
    ied$subject <- gsub("e:|:50", "", row.names(ied))
    ied$inoculate <- data$inoculate[match(ied$subject, data$subject)]
    ied$plate_number <- data$plate_number[match(ied$subject, data$subject)]
    ied$primary <- data$primary[match(ied$subject, data$subject)]
    facets <- if(length(unique(data$primary))>1) c("primary") else NULL
    # Average Neutralisation
    avied <- summarise(group_by(ied, inoculate), av = median(Estimate))
    ied_order <- avied$inoculate[order(avied$av)]

    values[["ic50"]] <- ggplot2::ggplot(ied, aes(x = inoculate, y = Estimate, colour = inoculate)) +
      geom_boxplot() +
      geom_point() +
      facet_wrap(facets) +
      # scale_colour_manual(values=ccs) +
      scale_x_discrete(limits = ied_order) +
      ylab("Individual IC50 log10") +
      xlab("Inoculate") +
      theme_classic() +
      ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary))) +
      coord_flip()
    plotly::ggplotly(ggplot2::last_plot())
  })
  output$cv_boxplot <- renderPlotly({
    req(input$luminescence_files)
    assay_df <- values[["assay_df"]]
    assay_df <- dplyr::filter(assay_df, types %in% c("c", "v"), exclude == FALSE) %>%
      dplyr::mutate(types = ifelse((types == "c"), "cell", types)) %>%
      dplyr::mutate(types = ifelse((types == "v"), "virus", types))
    assay_df$plate_number <- as.factor(assay_df$plate_number)

    values[["cv_boxplot"]] <- ggplot2::ggplot(assay_df, aes(x = types, y = rlu, colour = plate_number)) +
      geom_boxplot() +
      geom_point(position = position_dodge(0.75)) +
      scale_y_continuous(trans = "log10") +
      ylab("Log10 raw luminescence value") +
      xlab("Cell only or Virus only") +
      theme_classic() +
      ggtitle(paste(unique(assay_df$study), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$primary)))
    plotly::ggplotly(ggplot2::last_plot()) %>% layout(boxmode = "group")
  })
}
