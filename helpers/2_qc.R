#!/usr/bin/env Rscript

########################################################
# Helper functions for AutoPlate Step 2) Quality Control
########################################################

plot_heatmap <- function(plate_number, values, feature, title) {
  assay_df <- isolate(values[["assay_df"]])
  plate_df <- assay_df[assay_df$plate_number == plate_number, ]
  feature_list <- unlist(plate_df[[feature]], use.names = FALSE)
  vals <- matrix(feature_list, byrow = T, ncol = 12, nrow = 8)
  row.names(vals) <- LETTERS[1:8]
  # Set params for plot based on the feature
  features <- c("types", "subject", "dilution", "primary", "rlu", "neutralisation", "inoculate", "study", "bleed", "exclude")
  fmt.cells <- c("%.5s", "%.8s", "%.5s", "%.15s", "%.0f", "%.0f", "%.15s", "%.8s", "%.8s", "%.8s")
  features <- do.call(rbind, Map(data.frame, features = features, fmt.cells = fmt.cells))
  fmt.cell <- as.character(features[feature, ]$fmt.cells)
  col <- if (feature %in% c("dilution", "rlu", "neutralisation")) viridis else rainbow
  side <- if (feature %in% c("subject", "inoculate", "study", "bleed", "exclude")) 3 else 4
  # Generate heatmap plot
  plot(vals, col = col, fmt.cell = fmt.cell, main = paste("Plate", plate_number, title), key = list(side = side))
}
exclude_wells <- function(assay_df, exclusion_string) {
  exclusion_string <- gsub(" ", "", exclusion_string)
  wells_to_exclude <- lapply(strsplit(exclusion_string, ","), function(x) strsplit(x, ":"))[[1]]
  for (i in seq(1, length(wells_to_exclude))) {
    exclusion <- wells_to_exclude[[i]]
    tryCatch(
      {
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
