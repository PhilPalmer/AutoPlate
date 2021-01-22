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
  plate_df <- assay_df[assay_df$plate_number == plate_number, ]
  feature_list <- unlist(plate_df[[feature]], use.names = FALSE)
  vals <- matrix(feature_list, byrow = T, ncol = 12, nrow = 8)
  row.names(vals) <- LETTERS[1:8]
  # Set params for plot based on the feature
  features <- c("types", "sample_id", "dilution", "virus", "rlu", "neutralisation", "treatment", "experiment_id", "bleed", "exclude")
  fmt.cells <- c("%.5s", "%.8s", "%.5s", "%.15s", "%.0f", "%.0f", "%.15s", "%.8s", "%.8s", "%.8s")
  features <- do.call(rbind, Map(data.frame, features = features, fmt.cells = fmt.cells))
  fmt.cell <- as.character(features[feature, ]$fmt.cells)
  col <- if (feature %in% c("dilution", "rlu", "neutralisation")) viridis::viridis else rainbow
  side <- if (feature %in% c("sample_id", "treatment", "experiment_id", "bleed", "exclude")) 3 else 4
  # Generate heatmap plot
  plot(vals, col = col, fmt.cell = fmt.cell, main = paste("Plate", plate_number, title), key = list(side = side))
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
  types_boxplot <- ggplot2::ggplot(assay_df, ggplot2::aes(x = plate_number, y = rlu, colour = types)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point(position=ggplot2::position_dodge(0.75)) +
    ggplot2::ylab("Raw luminescence value") +
    ggplot2::xlab("Plate number") +
    ggplot2::theme_classic() +
    ggplot2::ggtitle(paste(unique(assay_df$experiment_id), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$virus)))
  return(types_boxplot)
}
