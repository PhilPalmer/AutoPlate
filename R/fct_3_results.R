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
#' @noRd
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
#' @noRd
prism_code_block <- function(code, language = "r") {
  tagList(
    HTML(prism_add_tags(code, language = language)),
    tags$script("Prism.highlightAll()")
  )
}

#' @title Setup code
#'
#' @description Function to return the setup code for a results plot
#' @return character, containing the code required to setup a results plot
#' @keywords setup code
#' @importFrom utils read.csv
#' @noRd
setup_code <- function() {
  '
    # Install libraries
    install.packages("devtools")
    devtools::install_github("PhilPalmer/AutoPlate", dependencies = TRUE)

    # Load library
    library(autoplate)

    # Load input file - make sure your input file path is correct!
    platelist_file <- "pmn_platelist.csv"
    data <- read.csv(platelist_file, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  '
}

#' @title Initialise title
#'
#' @description Function to return the title for a given assay dataframe
#' @param assay_df dataframe, main assay dataframe
#' @param full_title bool, to specify if the full length title should be used
#' @return character, title for plot
#' @keywords plot title
#' @export
init_title <- function(assay_df, full_title = FALSE) {
  features <- c("bleed", "experiment_id", "virus")
  for (feature in features) {
    if (all(unique(assay_df[feature]) %in% c(NA, ""))) assay_df[feature] <- NA
  }
  if ((nchar(toString(unique(assay_df$virus)))>50) && full_title == FALSE) {
    virus_title <-  "- All Viruses"
  }
  else {
    virus_title <- paste("- Virus",toString(unique(assay_df$virus)))
  }
  paste(toString(unique(assay_df$experiment_id)), "- Bleed", toString(unique(assay_df$bleed)), virus_title)
}

#' @title Update treatments colours and order
#'
#' @description Function to return to update the colours and ordering of treatments before plotting
#' @param assay_df dataframe, main assay dataframe
#' @return list, containing a modified assay dataframe, a list of treatment colours and a list of the treatments
#' @keywords colours
#' @importFrom metafolio gg_color_hue
#' @export
update_cols_order <- function(assay_df) {
  treatments <- unique(assay_df$treatment)
  colorblind_safe <- c("#000000", "#ff0066", "#0f7f80", "#410080", "#ab66ff", "#66ccfd")
  if (length(treatments) > length(colorblind_safe)) {
    treatment_cols <- gg_color_hue(length(treatments),0,360)
  } else {
    treatment_cols <- colorblind_safe
  }
  negative_control <- c("pbs", "negative_control")
  posotive_control <- unique(assay_df[assay_df$type == "m",]$treatment)
  if (any(negative_control %in% tolower(treatments))) {
    negative_control_index <- which(tolower(treatments) %in% negative_control)
    treatments <- c(treatments[negative_control_index],treatments[-negative_control_index])
    treatment_cols[1] <- "grey"
  }
  if (length(posotive_control > 0)) {
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
#' @param text_size integer, base text size to be used for the plot (default = 12)
#' @return plot, data exploration plot ggplot2 object 
#' @keywords data exploration plot
#' @export
plot_data_exploration <- function(assay_df, text_size=12) {
  # Preprocessing
  attach(update_cols_order(assay_df), warn.conflicts=FALSE)
  title <- init_title(assay_df)
  # Generate plot
  data_exploration_plot <- ggplot2::ggplot(assay_df, ggplot2::aes(x=dilution, y=neutralisation, colour=treatment)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se=F, span=1) +
    ggplot2::facet_wrap(.~virus) +
    ggplot2::ylim(c(-100, 110)) +
    ggplot2::scale_x_continuous(trans="log10") +
    ggprism::theme_prism(palette = "colorblind_safe", base_size = text_size) +
    ggplot2::theme(axis.title=ggplot2::element_text(size=text_size+2,face="bold")) +
    ggplot2::annotation_logticks(side="b", outside = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=-Inf)) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept=-Inf)) + 
    ggplot2::coord_cartesian(clip="off") +
    ggplot2::scale_colour_manual(breaks=treatments,values=treatment_cols) +
    ggplot2::ylab("% Neutralisation") +
    ggplot2::xlab("Serum Dilution") +
    ggplot2::ggtitle(title)
  return(data_exploration_plot)
}

#' @title Data exploration code
#'
#' @description Function to return the setup code for the data exploration results plot
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @return character, containing the code required for the data exploration plot results code
#' @keywords plot code
#' @noRd
data_exploration_code <- function(code) {
  setup <- setup_code()
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
#' @param text_size integer, base text size to be used for the plot (default = 12)
#' @return plot, dose-response curve plot ggplot2 object 
#' @keywords plot drc
#' @export
plot_drc <- function(assay_df, drm, text_size=12) {
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
  title <- init_title(assay_df)

  # Generate plot
  drc_plot <- ggplot2::ggplot(new_assay_df, ggplot2::aes(x=dilution, y=neutralisation, colour=treatment, group=sample_id)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(data=assay_df, ggplot2::aes(y=neutralisation)) +
      ggplot2::facet_wrap("treatment") +  
      ggplot2::scale_x_continuous(trans="log10") +
      ggplot2::scale_colour_manual(breaks=treatments,values=treatment_cols) +
      ggplot2::theme(strip.background = ggplot2::element_blank()) +
      ggprism::theme_prism(palette = "colorblind_safe", base_size = text_size) +
      ggplot2::theme(axis.title=ggplot2::element_text(size=text_size+2,face="bold")) +
      ggplot2::annotation_logticks(side="b", outside = TRUE) +
      ggplot2::geom_hline(yintercept=50, linetype="dotted") +
      ggplot2::geom_hline(ggplot2::aes(yintercept=-Inf)) + 
      ggplot2::geom_vline(ggplot2::aes(xintercept=-Inf)) + 
      ggplot2::coord_cartesian(clip="off") +
      ggplot2::ylab("% Neutralisation") +
      ggplot2::xlab("Serum Dilution") +
      ggplot2::ggtitle(title)
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
#' @noRd
drc_code <- function(code, drm_string, virus) {
  setup <- setup_code()
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
#' @description Function to generate IC50 plot from input main assay dataframe and dose-response model (DRM)
#' @param assay_df dataframe, main assay dataframe
#' @param drm DRM object, the dose response model
#' @param plot_type character, either "boxplot" (default) or "jitter"
#' @param text_size integer, base text size to be used for the plot (default = 12)
#' @return list, containing IC50 boxplot ggplot2 object and individual effective dose dataframe
#' @keywords plot code
#' @importFrom stats median
#' @export
plot_ic50_boxplot <- function(assay_df, drm, plot_type="boxplot", text_size=12) {
  # Preprocessing
  attach(update_cols_order(assay_df), warn.conflicts=FALSE)
  assay_df$sample_id <- as.character(assay_df$sample_id)
  ied <- as.data.frame(drc::ED(drm, 50, display=FALSE))
  ied$sample_id <- gsub("e:|:50", "", row.names(ied))
  ied$treatment <- assay_df$treatment[match(ied$sample_id, assay_df$sample_id)]
  ied$plate_number <- assay_df$plate_number[match(ied$sample_id, assay_df$sample_id)]
  ied$virus <- assay_df$virus[match(ied$sample_id, assay_df$sample_id)]
  ied <- dplyr::rename(ied, log_ic50_dilution = Estimate, log_ic50_dilution_std_error = "Std. Error")
  ied$raw_ic50_dilution <- exp(ied$log_ic50_dilution)
  ied <- ied[c("sample_id","treatment","raw_ic50_dilution","log_ic50_dilution","log_ic50_dilution_std_error","virus","plate_number")]
  control_median <- median(ied[tolower(ied$treatment) %in% tolower(c("PBS", "negative_control")),]$log_ic50_dilution)
  min_diution <- log(min(assay_df$dilution))
  # Average Neutralisation
  avied <- dplyr::summarise(dplyr::group_by(ied, treatment), av=median(log_ic50_dilution))
  ied_order <- avied$treatment[order(avied$av)]
  title <- init_title(assay_df)

  # Generate plot
  ic50_boxplot <- ggplot2::ggplot(ied, ggplot2::aes(x=treatment, y=log_ic50_dilution, colour=treatment)) +
      { if (plot_type == tolower("boxplot")) ggplot2::geom_boxplot() } +
      ggplot2::geom_point() +
      ggplot2::scale_x_discrete(limits=ied_order) +
      ggplot2::ylab("IC50 Dilution natural log") +
      ggplot2::xlab("Treatment") +
      ggprism::theme_prism(palette = "colorblind_safe", base_size = text_size) +
      ggplot2::theme(axis.title=ggplot2::element_text(size=text_size+2,face="bold")) +
      ggplot2::scale_colour_manual(breaks=treatments,values=treatment_cols) +
      ggplot2::ggtitle(title) +
      ggplot2::geom_hline(yintercept=c(min_diution), linetype="dotted", color="grey")
  return(list(ic50_boxplot=ic50_boxplot, ied=ied))
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
#' @noRd
ic50_boxplot_code <- function(code, drm_string, ic50_is_boxplot, virus) {
  setup <- setup_code()
  plot_type <- if(ic50_is_boxplot) "boxplot" else "jitter"
  plot <- paste0('
    # Preprocessing
    virus_to_plot <- "',virus,'"
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE, virus == virus_to_plot)
    model <- drc::drm(',drm_string,')

    # Generate plot
    ic50_boxplot <- plot_ic50_boxplot(data, model, plot_type="',plot_type,'")
    plotly::ggplotly(ic50_boxplot$ic50_boxplot)
  ')
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}

#' @title Plot cell-virus boxplot
#'
#' @description Function to generate a cell-virus boxplot plot from assay data frame
#' @param assay_df dataframe, main assay dataframe
#' @param text_size integer, base text size to be used for the plot (default = 12)
#' @return plot, cell-virus boxplot ggplot2 object 
#' @keywords boxplot cell virus
#' @export
plot_cv_boxplot <- function(assay_df, text_size=12) {
  # Preprocessing
  assay_df$plate_number <- as.factor(assay_df$plate_number)
  title <- init_title(assay_df)

  # Generate plot
  cv_boxplot <- ggplot2::ggplot(assay_df, ggplot2::aes(x=types, y=rlu, colour=plate_number)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(position=ggplot2::position_dodge(0.75)) +
      ggplot2::scale_y_continuous(trans="log10") +
      ggplot2::ylab("Log10 raw luminescence value") +
      ggplot2::xlab("Cell only or Virus only") +
      ggplot2::annotation_logticks(side="l") +
      ggprism::theme_prism(palette = "colorblind_safe", base_size = 12) +
      ggplot2::theme(axis.title=ggplot2::element_text(size=text_size+2,face="bold")) +
      ggplot2::ggtitle(title)
  return(cv_boxplot)
}

#' @title Cell-virus boxplot code
#'
#' @description Function to return the cell virus boxplot results plot code
#' @param code character, that's required, either "all" for setup and plot code or "plot" for just plot code
#' @return character, containing the code required for the cell virus boxplot results code
#' @keywords plot code
#' @noRd
cv_boxplot_code <- function(code) {
  setup <- setup_code()
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
