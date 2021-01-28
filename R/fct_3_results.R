#!/usr/bin/env Rscript

################################################
# Helper functions for AutoPlate Step 3) Results
################################################

#' @title Prism add tags
#'
#' @description Add code HTML tags to a HTML block input
#' @param code character, HTML code to add the code tags to
#' @param language character, programming language of the HTML code block, default is R
#' @return A HTML block with relevant langugage code tags
#' @keywords HTML code
#' @export
prism_add_tags <- function(code, language = "r") {
  paste0("<pre><code class = 'language-", language, "'>",code,"</code></pre>")
}

#' @title Prism code block
#'
#' @description Add syntax highlighting to a HTML code block with Prism
#' @param code character, HTML code to highlight with Prism
#' @param language character, programming language of the HTML code block, default is R
#' @return A HTML block with syntax highlighting
#' @keywords HTML code Prism
#' @export
prism_code_block <- function(code, language = "r") {
  tagList(
    HTML(prism_add_tags(code, language = language)),
    tags$script("Prism.highlightAll()")
  )
}

#' @title Setup code
#'
#' @description Function to return the setup code for a results plot
#' @param is_drc_plot bool, value to specify if the plot requires the drc library
#' @param is_vc_plot bool, value to specify if the plot requires the metafolio library
#' @return character, containing the code required to setup a results plot
#' @keywords setup code
#' @importFrom utils read.csv
#' @export
setup_code <- function(is_drc_plot, is_vc_plot) {
  libraries <- '
    # Install libraries
    install.packages("devtools")
    devtools::install_github("PhilPalmer/AutoPlate")

    # Load libraries
    library(autoplate)
    library(dplyr)
    library(ggplot2)
    library(plotly)
  '
  # TODO: remove unecessary input params
  if (is_drc_plot) libraries <- paste0(libraries,"\tlibrary(drc)\n")
  if (!is_vc_plot) libraries <- paste0(libraries,"\tlibrary(metafolio)\n")
  input_file <- '
    # Load input file - make sure your input file path is correct!
    platelist_file <- "pmn_platelist.csv"
    data <- read.csv(platelist_file, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  '
  setup_code <- paste0(libraries,input_file)
  return(setup_code)
}

#' @title Update treatments colours and order code
#'
#' @description Function to return the code to update the colours and ordering of treatments before plotting
#' @return character, containing code to update the treatments colour and order before plotting a results plot
#' @keywords colours code
#' @export
update_cols_order_code <- function() {
  '
    # Update order and colours of treatments
    treatments <- unique(data$treatment)
    treatment_cols <- metafolio::gg_color_hue(length(treatments),0,360)
    negative_control <- c("pbs", "negative_control")
    posotive_control <- unique(data[data$type == "m",]$treatment)
    if (any(negative_control %in% tolower(treatments))) {
      negative_control_index <- which(tolower(treatments) %in% negative_control)
      treatments <- c(treatments[negative_control_index],treatments[-negative_control_index])
      treatment_cols[1] <- "grey"
    }
    if (!is.na(posotive_control)) {
      posotive_control_index <- which(treatments %in% posotive_control)
      treatments <- c(treatments[-posotive_control_index],treatments[posotive_control_index])
      treatment_cols[length(treatments)] <- "black"
    }
    data$treatment <- factor(data$treatment, levels = treatments)
  '
}

#' @title Update treatments colours and order
#'
#' @description Function to return to update the colours and ordering of treatments before plotting
#' @param assay_df dataframe, main assay dataframe
#' @return list, containing a modified assay dataframe, a list of treatment colours and a list of the treatments
#' @keywords colours code
#' @importFrom metafolio gg_color_hue
#' @export
update_cols_order <- function(assay_df) {
  treatments <- unique(assay_df$treatment)
  treatment_cols <- metafolio::gg_color_hue(length(treatments),0,360)
  negative_control <- c("pbs", "negative_control")
  posotive_control <- unique(assay_df[assay_df$type == "m",]$treatment)
  if (any(negative_control %in% tolower(treatments))) {
    negative_control_index <- which(tolower(treatments) %in% negative_control)
    treatments <- c(treatments[negative_control_index],treatments[-negative_control_index])
    treatment_cols[1] <- "grey"
  }
  if (!is.na(posotive_control)) {
    posotive_control_index <- which(treatments %in% posotive_control)
    treatments <- c(treatments[-posotive_control_index],treatments[posotive_control_index])
    treatment_cols[length(treatments)] <- "black"
  }
  assay_df$treatment <- factor(assay_df$treatment, levels = treatments)
  return(list(assay_df=assay_df, treatment_cols=treatment_cols,treatments=treatments))
}

#' @title Plot data exploration
#'
#' @description Function to generate a data exploration plot from assay data frame
#' @param assay_df dataframe, main assay dataframe
#' @return plot, data exploration plot ggplot2 object 
#' @keywords data exploration plot
#' @export
plot_data_exploration <- function(assay_df) {
  # Preprocessing
  attach(update_cols_order(assay_df), warn.conflicts=FALSE)
  # Generate plot
  data_exploration_plot <- ggplot2::ggplot(assay_df, ggplot2::aes(x=dilution, y=neutralisation, colour=treatment)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se=F, span=1) +
    ggplot2::facet_wrap(.~virus) +
    ggplot2::ylim(c(-100, 110)) +
    ggplot2::scale_x_continuous(trans="log10") +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_manual(breaks=treatments,values=treatment_cols) +
    ggplot2::ylab("Neutralisation") +
    ggplot2::xlab("Dilution") +
    ggplot2::ggtitle(paste(unique(assay_df$experiment_id), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$virus)))
  return(data_exploration_plot)
}

#' @title Data exploration code
#'
#' @description Function to return the setup code for the data exploration results plot
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @return character, containing the code required for the data exploration plot results code
#' @keywords plot code
#' @export
data_exploration_code <- function(code) {
  setup <- setup_code(is_drc_plot = FALSE, is_vc_plot = FALSE)
  plot <- paste0('
    # Preprocessing
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
    
    # Generate plot
    data_exploration_plot <- plot_data_exploration(data)
    plotly::ggplotly(data_exploration_plot)
  ')
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}

#' @title Plot DRC
#'
#' @description Function to generate a dose-response curve (DRC) plot from assay data frame
#' @param assay_df dataframe, main assay dataframe
#' @param drm DRM object, the dose response model
#' @return plot, dose-response curve plot ggplot2 object 
#' @keywords plot drc
#' @export
plot_drc <- function(assay_df, drm) {
  # Preprocessing
  attach(update_cols_order(assay_df), warn.conflicts=FALSE)
  assay_df$sample_id <- unlist(assay_df$sample_id)        
  n <- 100
  new_dilution <- exp(seq(log(min(assay_df$dilution)), log(max(assay_df$dilution)), length.out=n))
  sample_ids<-unique(assay_df$sample_id)
  new_assay_df <- expand.grid(new_dilution, sample_ids)
  names(new_assay_df) <- c("dilution", "sample_id")
  new_assay_df$treatment <- assay_df$treatment[match(new_assay_df$sample_id, assay_df$sample_id)]
  new_assay_df$neutralisation <- predict(drm, newdata=new_assay_df,)

  # Generate plot
  drc_plot <- ggplot2::ggplot(new_assay_df, ggplot2::aes(x=dilution, y=neutralisation, colour=treatment, group=sample_id)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data=assay_df, ggplot2::aes(y=neutralisation)) +
      ggplot2::facet_wrap("treatment") +  
      ggplot2::scale_x_continuous(trans="log10") +
      ggplot2::theme_classic() +
      ggplot2::scale_colour_manual(breaks=treatments,values=treatment_cols) +
      ggplot2::theme(strip.background = ggplot2::element_blank()) +
      ggplot2::ylab("Neutralisation") +
      ggplot2::xlab("Dilution") +
      ggplot2::ggtitle(paste(unique(assay_df$experiment_id), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$virus)))
  return(drc_plot)
}

#' @title DRC code
#'
#' @description Function to generate an IC50 boxplot plot from assay data frame
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @param drm_string character, specifying the code the dose response model
#' @param virus character, specifying the virus to plot
#' @return character, containing the code required for the DRC plot results code
#' @keywords plot code
#' @export
drc_code <- function(code, drm_string, virus) {
  setup <- setup_code(is_drc_plot = TRUE, is_vc_plot = FALSE)
  plot <- paste0('
    # Preprocessing
    virus_to_plot <- "',virus,'"
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE, virus == virus_to_plot)
    model <- drc::drm(',drm_string,')

    # Generate plot
    drc_plot <- plot_drc(data, model)
    plotly::ggplotly(drc_plot)
  ')
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}

#' @title Plot IC50 boxplot
#'
#' @description Function to gneerate IC50 plot from input main assay dataframe and dose-response model (DRM)
#' @param assay_df dataframe, main assay dataframe
#' @param drm DRM object, the dose response model
#' @param plot_type character, either "boxplot" (default) or "jitter"
#' @return plot, dose-response curve plot ggplot2 object 
#' @keywords plot code
#' @importFrom stats median
#' @export
plot_ic50_boxplot <- function(assay_df, drm, plot_type="boxplot") {
  # Preprocessing
  attach(update_cols_order(assay_df), warn.conflicts=FALSE)
  assay_df$sample_id <- as.character(assay_df$sample_id)
  ied <- as.data.frame(drc::ED(drm, 50, display=FALSE))
  ied$sample_id <- gsub("e:|:50", "", row.names(ied))
  ied$treatment <- assay_df$treatment[match(ied$sample_id, assay_df$sample_id)]
  ied$plate_number <- assay_df$plate_number[match(ied$sample_id, assay_df$sample_id)]
  ied$virus <- assay_df$virus[match(ied$sample_id, assay_df$sample_id)]
  control_median <- median(ied[tolower(ied$treatment) %in% tolower(c("PBS", "negative_control")),]$Estimate)
  # Average Neutralisation
  avied <- dplyr::summarise(dplyr::group_by(ied, treatment), av=median(Estimate))
  ied_order <- avied$treatment[order(avied$av)]
  plot_type <- if(plot_type == tolower("boxplot")) ggplot2::geom_boxplot() else ggplot2::geom_jitter()

  # Generate plot
  ic50_boxplot <- ggplot2::ggplot(ied, ggplot2::aes(x=treatment, y=Estimate, colour=treatment))+
      plot_type +
      ggplot2::geom_point() +
      ggplot2::scale_x_discrete(limits=ied_order) +
      ggplot2::ylab("Individual IC50 log10") +
      ggplot2::xlab("Treatment") +
      ggplot2::theme_classic() +
      ggplot2::scale_colour_manual(breaks=treatments,values=treatment_cols) +
      ggplot2::ggtitle(paste(unique(assay_df$experiment_id), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$virus))) +
      ggplot2::coord_flip() + 
      ggplot2::geom_hline(yintercept=c(control_median), linetype="dotted", color="grey")
  return(ic50_boxplot)
}

#' @title IC50 boxplot code
#'
#' @description Function to return the IC50 plot results plot code
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @param drm_string character, specifying the code the dose response model
#' @param ic50_is_boxplot bool, specify plot type, true for boxplot, false for scatter plot 
#' @param virus character, specifying the virus to plot
#' @return character, containing the code required for the IC50 plot results code
#' @keywords plot code
#' @importFrom stats median
#' @export
ic50_boxplot_code <- function(code, drm_string, ic50_is_boxplot, virus) {
  setup <- setup_code(is_drc_plot = TRUE, is_vc_plot = FALSE)
  plot_type <- if(ic50_is_boxplot) "boxplot" else "jitter"
  plot <- paste0('
    # Preprocessing
    virus_to_plot <- "',virus,'"
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE, virus == virus_to_plot)
    model <- drc::drm(',drm_string,')

    # Generate plot
    ic50_boxplot <- plot_ic50_boxplot(data, model, plot_type="',plot_type,'")
    plotly::ggplotly(ic50_boxplot)
  ')
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}

#' @title Plot cell-virus boxplot
#'
#' @description Function to generate a cell-virus boxplot plot from assay data frame
#' @param assay_df dataframe, main assay dataframe
#' @return plot, cell-virus boxplot ggplot2 object 
#' @keywords boxplot cell virus
#' @export
plot_cv_boxplot <- function(assay_df) {
  # Preprocessing
  assay_df$plate_number <- as.factor(assay_df$plate_number)

  # Generate plot
  cv_boxplot <- ggplot2::ggplot(assay_df, ggplot2::aes(x=types, y=rlu, colour=plate_number)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(position=ggplot2::position_dodge(0.75)) +
      ggplot2::scale_y_continuous(trans="log10") +
      ggplot2::ylab("Log10 raw luminescence value") +
      ggplot2::xlab("Cell only or Virus only") +
      ggplot2::theme_classic() +
      ggplot2::ggtitle(paste(unique(assay_df$experiment_id), "- Bleed", unique(assay_df$bleed), "- Virus", unique(assay_df$virus)))
  return(cv_boxplot)
}

#' @title Cell-virus boxplot code
#'
#' @description Function to return the cell virus boxplot results plot code
#' @param code character, that's required, either "all" for setup and plot code or "plot" for just plot code
#' @return character, containing the code required for the cell virus boxplot results code
#' @keywords plot code
#' @export
cv_boxplot_code <- function(code) {
  setup <- setup_code(is_drc_plot = FALSE, is_vc_plot = TRUE)
  plot <- '
    # Preprocessing
    data <- dplyr::filter(data, types %in% c("c", "v"), exclude == FALSE)  %>%
      dplyr::mutate(types = ifelse( (types == "c"), "cell", types)) %>%
      dplyr::mutate(types = ifelse( (types == "v"), "virus", types))

    # Generate plot
    cv_boxplot <- plot_cv_boxplot(data)
    plotly::ggplotly(cv_boxplot) %>% plotly::layout(boxmode = "group")
    '
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}
