#!/usr/bin/env Rscript

########################################################
# Helper functions for AutoPlate Step 2) Quality Control
########################################################

#' @title Plot heatmap
#'
#' @description Generate a heatmap plot in 96-well plate format for a feature from the assay dataframe
#' @param plate_number integer, to specify plate number you wish to plot from assay dataframe
#' @param assay_df dataframe, main assay dataframe
#' @param feature character, name of feature to plot, must be a column in assay_df
#' @param title character, title to be used for the plot title along with the plate number
#' @return heatmap plot
#' @keywords plot heatmap
#' @importFrom grDevices rainbow
#' @importFrom graphics plot
#' @export
plot_heatmap <- function(plate_number, assay_df, feature, title) {
  # Make the dataframe for the single input plate
  plate_df <- assay_df[assay_df$plate_number == plate_number, ]
  if (all(is.na(unique(plate_df[feature])))) plate_df[feature][is.na(plate_df[feature])] <- ""

  # Define vars
  set.seed(42)
  features <- c("types", "sample_id", "dilution", "virus", "rlu", "neutralisation", "treatment", "experiment_id", "bleed", "exclude")
  cont_features <- c("dilution","rlu","neutralisation")
  # Define text length for different features
  fmt.cells <- c("%.5s", "%.8s", "%.5s", "%.15s", "%.0f", "%.0f", "%.15s", "%.8s", "%.8s", "%.8s")
  features <- do.call(rbind, Map(data.frame, features = features, fmt.cells = fmt.cells))
  fmt.cell <- as.character(features[feature, ]$fmt.cells)
  # Set different colour schemes and legend positions for some features
  col <- if (feature %in% cont_features) viridis::viridis else rainbow
  side <- if (feature %in% c("sample_id", "treatment", "experiment_id", "bleed", "exclude")) 3 else 4
  # Make feature list to make the matrix using rounded values if feature is continuous
  plate_df[feature] <- if (feature %in% cont_features) round(plate_df[feature], 0) else plate_df[feature]
  feature_list <- unlist(plate_df[[feature]], use.names = FALSE) 

  # Make colours consistent between heatmaps depending on if the feature is continuous or categorical
  if (feature %in% cont_features) {
    feature_vals <- round(assay_df[feature][!is.na(assay_df[feature])], 0)
    min <- min(feature_vals)
    max <- max(feature_vals)
    breaks <- c(min, max)
    breaks <- if (feature == "dilution") exp(seq(0, log(max), length.out = 10)) else breaks
  } else {
    all_levels <- sample(sort(unique(assay_df[[feature]])))
    plate_levels <- sort(unique(plate_df[[feature]]))
    all_col <- col(length(all_levels))
    col <- all_col[match(plate_levels,all_levels)]
    breaks <- NULL
  }

  # Make matrix for feature values
  vals <- matrix(feature_list, byrow = T, ncol = 12, nrow = 8)
  row.names(vals) <- LETTERS[1:8]

  # Generate heatmap plot
  par(mar=c(4, 4, 4, 6))
  plot(vals, col = col, fmt.cell = fmt.cell, main = paste("Plate", plate_number, title), key = list(side = side), breaks=breaks)
}

#' @title Exclude wells
#'
#' @description Exclude values in dataframe based on exclusion criteria
#' @param assay_df dataframe, main assay dataframe
#' @param exclusion_string character, comma-seperated string containing exclusion criteria
#' @return dataframe, main assay with updated values in the exclude column based on the exclusion criteria
#' @keywords exclude wells
#' @importFrom magrittr %>%
#' @export
exclude_wells <- function(assay_df, exclusion_string) {
  exclusion_string <- gsub(" ", "", exclusion_string)
  wells_to_exclude <- lapply(strsplit(exclusion_string, ","), function(x) strsplit(x, ":"))[[1]]
  for (i in seq(1, length(wells_to_exclude))) {
    tryCatch(
      {
        exclusion <- wells_to_exclude[[i]]
        # Exclude a whole plate
        if (length(exclusion) == 1 & nchar(exclusion[1]) <= 1) {
          plate <- exclusion[1]
          assay_df <- assay_df %>%
            dplyr::mutate(exclude = ifelse((plate_number == plate), TRUE, exclude))
        }
        # Exclude an individual well
        if (length(exclusion) == 1 & nchar(exclusion[1]) > 1) {
          plate <- as.numeric(sub("\\D*(\\d+).*", "\\1", exclusion[1]))
          row <- strsplit(exclusion[1], split = "[0-9]+")[[1]]
          col <- strsplit(exclusion[1], split = "[A-Z]+")[[1]][2]
          assay_df <- assay_df %>%
            dplyr::mutate(exclude = ifelse((plate_number == plate) & (wcol %in% col) & (wrow %in% row), TRUE, exclude))
        }
        # Exclude a range of wells
        if (length(exclusion) > 1) {
          # get plate to exclude
          plate <- as.numeric(sub("\\D*(\\d+).*", "\\1", exclusion[1]))
          exclusion[1] <- strsplit(exclusion[1], split = "^[0-9]+")[[1]][2]
          # get start and end rows and columns
          row_start <- strsplit(exclusion[1], split = "[0-9]+")[[1]]
          row_end <- strsplit(exclusion[2], split = "[0-9]+")[[1]]
          col_start <- strsplit(exclusion[1], split = "[A-Z]+")[[1]][2]
          col_end <- strsplit(exclusion[2], split = "[A-Z]+")[[1]][2]
          # get vector of rows & columns to exclude
          cols_to_exclude <- seq(col_start, col_end)
          rows_to_exclude <- seq(match(row_start, LETTERS), match(row_end, LETTERS))
          rows_to_exclude <- sapply(rows_to_exclude, function(i) LETTERS[i])
          # update excluded plates in main assay dataframe
          assay_df <- assay_df %>%
            dplyr::mutate(exclude = ifelse((plate_number == plate) & (wcol %in% cols_to_exclude) & (wrow %in% rows_to_exclude), TRUE, exclude))
        }
      },
      error = function(error_message) {
        print(error_message)
      }
    )
  }
  return(assay_df)
}

#' @title Initialise average luminescence dataframe
#'
#' @description Initialise average luminescence dataframe
#' @param assay_df dataframe, main assay dataframe
#' @return dataframe, containing average luminescence data for each plate
#' @keywords average luminescence dataframe
#' @export
init_av_lum_df <- function(assay_df) {
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
}

#' @title Initialise types boxplot
#'
#' @description Generate types boxplot
#' @param assay_df dataframe, main assay dataframe
#' @return boxplot containing types data for each of the different plates 
#' @keywords types boxplot
#' @importFrom magrittr %>%
#' @export
init_types_boxplot <- function(assay_df) {
  assay_df <- assay_df %>%
    dplyr::filter(exclude == FALSE) %>%
    dplyr::mutate(types = ifelse((types == "c"), "cell", types)) %>%
    dplyr::mutate(types = ifelse((types == "m"), "monoclonal antibody", types)) %>%
    dplyr::mutate(types = ifelse((types == "v"), "virus", types)) %>%
    dplyr::mutate(types = ifelse((types == "x"), "serum sample", types))
  assay_df$plate_number <- as.factor(assay_df$plate_number)
  title <- init_title(assay_df)
  types_boxplot <- ggplot2::ggplot(assay_df, ggplot2::aes(x = plate_number, y = rlu, colour = types)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point(position=ggplot2::position_dodge(0.75)) +
    ggplot2::ylab("Raw luminescence value") +
    ggplot2::xlab("Plate number") +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(title)
  return(types_boxplot)
}

#' @title Assay to PRISM dataframe
#'
#' @description Convert full assay dataframe to an exportable format for PRISM for the specified feature
#' @param assay_df dataframe, containing biological assay data from plate reader
#' @param feature feature to generate the plate data format for (default = `neutralisation`)
#' @return dataframe, PRISM dataframe in 96-well plate format for all plates
#' @keywords assay PRISM
#' @export
assay_to_prism_df <- function(assay_df, feature = "neutralisation") {
  plates <- sort(unique(assay_df$plate_number))
  prism_df <- data.frame()
  for (plate in plates) {
    plate_df <- assay_to_plate_df(assay_df, plate, feature)
    plate_df <- rbind(names(plate_df), plate_df)
    colnames(plate_df) <- paste0("V", 1:length(names(plate_df)))
    row.names(plate_df)[1] <- "sample_ids"
    plate_df[1,] <- gsub(pattern = "V[0-9]+ (.*)", '\\1', plate_df[1,])
    plate_df <- data.frame(row = row.names(plate_df), plate_df)
    row.names(plate_df) <- 1:dim(plate_df)[1]
    plate_df[] <- lapply(plate_df, as.character)
    plate_df <- rbind(c("plate_number", plate, rep("",length(names(plate_df))-2)), plate_df)
    plate_df[nrow(plate_df)+1,] <- ""
    prism_df <- dplyr::bind_rows(prism_df,plate_df)
  }
  return(prism_df)
}