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
#' @examples
#' prism_add_tags()
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
#' @examples
#' prism_code_block()
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
#' @export
#' @examples
#' setup_code()
setup_code <- function(is_drc_plot, is_vc_plot) {
  libraries <- '
    # Load libraries
    library(dplyr)
    library(ggplot2)
    library(plotly)
  '
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

#' @title Update inoculates colours and order code
#'
#' @description Function to return the code to update the colours and ordering of inoculates before plotting
#' @return character, containing code to update the inoculates colour and order before plotting a results plot
#' @keywords colours code
#' @export
#' @examples
#' update_cols_order_code()
update_cols_order_code <- function() {
  '
    # Update order and colours of incoluates
    inoculates <- unique(data$inoculate)
    inoculate_cols <- gg_color_hue(length(inoculates),0,360)
    negative_control <- c("pbs", "negative_control")
    posotive_control <- unique(data[data$type == "m",]$inoculate)
    if (any(negative_control %in% tolower(inoculates))) {
      negative_control_index <- which(tolower(inoculates) %in% negative_control)
      inoculates <- c(inoculates[negative_control_index],inoculates[-negative_control_index])
      inoculate_cols[1] <- "grey"
    }
    if (!is.na(posotive_control)) {
      posotive_control_index <- which(inoculates %in% posotive_control)
      inoculates <- c(inoculates[-posotive_control_index],inoculates[posotive_control_index])
      inoculate_cols[length(inoculates)] <- "black"
    }
    data$inoculate <- factor(data$inoculate, levels = inoculates)
  '
}

#' @title Data exploration code
#'
#' @description Function to return the setup code for the data exploration results plot
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @return character, containing the code required for the data exploration plot results code
#' @keywords plot code
#' @export
#' @examples
#' data_exploration_code()
data_exploration_code <- function(code) {
  setup <- setup_code(is_drc_plot = FALSE, is_vc_plot = FALSE)
  plot <- paste0('
    # Filter out unwanted types and excluded data
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
    ',update_cols_order_code(),'
    # Generate plot
    data_exploration_plot <- ggplot2::ggplot(data, aes(x=dilution, y=neutralisation, colour=inoculate)) +
      geom_point() +
      geom_smooth(se=F, span=1) +
      facet_wrap(.~primary) +
      ylim(c(-100, 110)) +
      scale_x_continuous(trans="log10") +
      theme_classic() +
      scale_colour_manual(breaks=inoculates,values=inoculate_cols) +
      ylab("Neutralisation") +
      xlab("Dilution") +
      ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    data_exploration_plotly <- plotly::ggplotly(data_exploration_plot)
    data_exploration_plotly
  ')
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}

#' @title DRC code
#'
#' @description Function to return the DRC plot results plot code
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @param drm_string character, specifying the code the dose response model
#' @return character, containing the code required for the DRC plot results code
#' @keywords plot code
#' @export
#' @examples
#' drc_code()
drc_code <- function(code, drm_string) {
  setup <- setup_code(is_drc_plot = TRUE, is_vc_plot = FALSE)
  plot <- paste0('
    # Filter out unwanted types and excluded data
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
    ',update_cols_order_code(),'
    # Preprocessing
    data$sample_id <- unlist(data$sample_id)        
    model <- drc::drm(', drm_string, ')
    n <- 100
    new_dilution <- exp(seq(log(min(data$dilution)), log(max(data$dilution)), length.out=n))
    sample_ids<-unique(data$sample_id)
    new_data <- expand.grid(new_dilution, sample_ids)
    names(new_data) <- c("dilution", "sample_id")
    new_data$inoculate <- data$inoculate[match(new_data$sample_id, data$sample_id)]
    new_data$pred <- predict(model, newdata=new_data,)
    facets <- if(length(unique(data$primary))>1) c("inoculate","primary") else c("inoculate")

    # Generate plot
    drc_plot <- ggplot2::ggplot(new_data, aes(x=dilution, y=pred, colour=inoculate, group=sample_id)) +
        geom_line() +
        geom_point(data=data, aes(y=neutralisation)) +
        facet_wrap(facets) +
        scale_x_continuous(trans="log10") +
        theme_classic() +
        scale_colour_manual(breaks=inoculates,values=inoculate_cols) +
        theme(strip.background = element_blank()) +
        ylab("Neutralisation") +
        xlab("Dilution") +
        ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    drc_plotly <- plotly::ggplotly(drc_plot)
    drc_plotly
    ')
    if (code == "plot") code_text <- plot
    if (code == "all") code_text <- paste0(setup,plot)
    return(code_text)
}

#' @title IC50 boxplot code
#'
#' @description Function to return the IC50 plot results plot code
#' @param code character, code that's required, either "all" for setup and plot code or "plot" for just plot code
#' @param drm_string character, specifying the code the dose response model
#' @param ic50_is_boxplot bool, specify plot type, true for boxplot, false for scatter plot 
#' @return character, containing the code required for the IC50 plot results code
#' @keywords plot code
#' @export
#' @examples
#' ic50_boxplot_code()
ic50_boxplot_code <- function(code, drm_string, ic50_is_boxplot) {
  setup <- setup_code(is_drc_plot = TRUE, is_vc_plot = FALSE)
  plot_type <- if(ic50_is_boxplot) "boxplot" else "jitter"
  plot <- paste0('
    # Filter out unwanted types and excluded data
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
    ',update_cols_order_code(),'
    # Preprocessing
    data$sample_id <- as.character(data$sample_id)
    model <- drc::drm(', drm_string, ')
    ied <- as.data.frame(ED(model, 50, display=FALSE))
    ied$sample_id <- gsub("e:|:50", "", row.names(ied))
    ied$inoculate <- data$inoculate[match(ied$sample_id, data$sample_id)]
    ied$plate_number <- data$plate_number[match(ied$sample_id, data$sample_id)]
    ied$primary <- data$primary[match(ied$sample_id, data$sample_id)]
    facets <- if(length(unique(data$primary))>1) c("primary") else NULL
    control_median <- median(ied[tolower(ied$inoculate) %in% tolower(c("PBS", "negative_control")),]$Estimate)
    # Average Neutralisation
    avied <- summarise(group_by(ied, inoculate), av=median(Estimate))
    ied_order <- avied$inoculate[order(avied$av)]

    # Generate plot
    ic50_boxplot <- ggplot2::ggplot(ied, aes(x=inoculate, y=Estimate, colour=inoculate))+
        geom_',plot_type,'() +
        geom_point() +
        facet_wrap(facets) +
        scale_x_discrete(limits=ied_order) +
        ylab("Individual IC50 log10") +
        xlab("Inoculate") +
        theme_classic() +
        scale_colour_manual(breaks=inoculates,values=inoculate_cols) +
        ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary))) +
        coord_flip() + 
        geom_hline(yintercept=c(control_median), linetype="dotted", color="grey")
    ic50_boxplotly <- plotly::ggplotly(ic50_boxplot)
    ic50_boxplotly
  ')
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}

#' @title Cell virus boxplot code
#'
#' @description Function to return the cell virus boxplot results plot code
#' @param code character, that's required, either "all" for setup and plot code or "plot" for just plot code
#' @param drm_string character, specifying the code the dose response model
#' @return character, containing the code required for the cell virus boxplot results code
#' @keywords plot code
#' @export
#' @examples
#' cv_boxplot_code()
cv_boxplot_code <- function(code) {
  setup <- setup_code(is_drc_plot = FALSE, is_vc_plot = TRUE)
  plot <- '
    # Preprocessing
    data <- dplyr::filter(data, types %in% c("c", "v"), exclude == FALSE)  %>%
        dplyr::mutate(types = ifelse( (types == "c"), "cell", types)) %>%
        dplyr::mutate(types = ifelse( (types == "v"), "virus", types))
    data$plate_number <- as.factor(data$plate_number)

    # Generate plot
    cv_boxplot <- ggplot2::ggplot(data, aes(x=types, y=rlu, colour=plate_number)) +
        geom_boxplot() +
        geom_point(position=position_dodge(0.75)) +
        scale_y_continuous(trans="log10") +
        ylab("Log10 raw luminescence value") +
        xlab("Cell only or Virus only") +
        theme_classic() +
        ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    cv_boxplotly <- plotly::ggplotly(cv_boxplot) %>% layout(boxmode = "group")
    cv_boxplotly
    '
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}
