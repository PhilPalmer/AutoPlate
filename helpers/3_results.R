#!/usr/bin/env Rscript

################################################
# Helper functions for AutoPlate Step 3) Results
################################################

# Format code with tags and language
prism_add_tags <- function(code, language = "r") {
  paste0("<pre><code class = 'language-", language, "'>",code,"</code></pre>")
}
prism_code_block <- function(code, language = "r") {
  tagList(
    HTML(prism_add_tags(code, language = language)),
    tags$script("Prism.highlightAll()")
  )
}
setup_code <- function(drc_plot) {
  libraries <- '
    # Load libraries
    library(dplyr)
    library(ggplot2)
    library(plotly)
  '
  if (drc_plot) libraries <- paste0(libraries,"\tlibrary(drc)\n")
  input_file <- '
    # Load input file - make sure your input file path is correct!
    platelist_file <- "pmn_platelist.csv"
    data <- read.csv(platelist_file, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  '
  setup_code <- paste0(libraries,input_file)
  return(setup_code)
}
data_exploration_code <- function(code) {
  setup <- setup_code(drc_plot = FALSE)
  plot <- '
    # Preprocessing
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)

    # Generate plot
    data_exploration_plot <- ggplot2::ggplot(data, aes(x=dilution, y=neutralisation, colour=inoculate)) +
      geom_point() +
      geom_smooth(se=F, span=1) +
      facet_wrap(.~primary) +
      ylim(c(-100, 110)) +
      scale_x_continuous(trans="log10") +
      theme_classic() +
      ylab("Neutralisation") +
      xlab("Dilution") +
      ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    plotly::ggplotly(data_exploration_plot)
  '
  if (code == "plot") code_text <- plot
  if (code == "all") code_text <- paste0(setup,plot)
  return(code_text)
}
print_drc_code <- function(drm_string) {
  paste0('
    library(dplyr)
    library(drc)
    library(ggplot2)

    platelist_file <- "pmn_platelist.csv"

    data <- read.csv(platelist_file, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
    data$subject <- unlist(data$subject)        
    model <- drc::drm(', drm_string, ')
    n <- 100
    new_dilution <- exp(seq(log(min(data$dilution)), log(max(data$dilution)), length.out=n))
    subjects<-unique(data$subject)
    new_data <- expand.grid(new_dilution, subjects)
    names(new_data) <- c("dilution", "subject")
    new_data$inoculate <- data$inoculate[match(new_data$subject, data$subject)]
    new_data$pred <- predict(model, newdata=new_data,)
    facets <- if(length(unique(data$primary))>1) c("inoculate","primary") else c("inoculate")

    drc_plot <- ggplot2::ggplot(new_data, aes(x=dilution, y=pred, colour=inoculate, group=subject)) +
        geom_line() +
        geom_point(data=data, aes(y=neutralisation)) +
        facet_wrap(facets) +
        scale_x_continuous(trans="log10") +
        theme_classic() +
        ylab("Neutralisation") +
        xlab("Dilution") +
        ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    plotly::ggplotly(drc_plot)
    ')
}
print_ic50_boxplot_code <- function(drm_string) {
  paste0('
    library(dplyr)
    library(drc)
    library(ggplot2)

    platelist_file <- "pmn_platelist.csv"

    data <- read.csv(platelist_file, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
    data <- dplyr::filter(data, types %in% c("x", "m"), exclude == FALSE)
    model <- drc::drm(', drm_string, ')
    ied <- as.data.frame(ED(model, 50, display=FALSE))
    ied$subject <- gsub("e:|:50", "", row.names(ied))
    ied$inoculate <- data$inoculate[match(ied$subject, data$subject)]
    ied$plate_number <- data$plate_number[match(ied$subject, data$subject)]
    ied$primary <- data$primary[match(ied$subject, data$subject)]
    facets <- if(length(unique(data$primary))>1) c("primary") else NULL
    # Average Neutralisation
    avied <- summarise(group_by(ied, inoculate), av=median(Estimate))
    ied_order <- avied$inoculate[order(avied$av)]

    ic50_boxplot <- ggplot2::ggplot(ied, aes(x=inoculate, y=Estimate, colour=inoculate))+
        geom_boxplot() +
        geom_point() +
        facet_wrap(facets) +
        scale_x_discrete(limits=ied_order) +
        ylab(expression("Individual IC50 log"[10])) +
        xlab("Inoculate") +
        theme_classic() +
        ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary))) +
        coord_flip()
    plotly::ggplotly(ic50_boxplot)
    ')
}
print_cv_boxplot_code <- function() {
  '
    library(dplyr)
    library(ggplot2)

    platelist_file <- "pmn_platelist.csv"

    data <- read.csv(platelist_file, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
    data <- dplyr::filter(data, types %in% c("c", "v"), exclude == FALSE)  %>%
        dplyr::mutate(types = ifelse( (types == "c"), "cell", types)) %>%
        dplyr::mutate(types = ifelse( (types == "v"), "virus", types))
    data$plate_number <- as.factor(data$plate_number)

    cv_boxplot <- ggplot2::ggplot(data, aes(x=types, y=rlu, colour=plate_number)) +
        geom_boxplot() +
        geom_point(position=position_dodge(0.75)) +
        scale_y_continuous(trans="log10") +
        ylab("Log10 raw luminescence value") +
        xlab("Cell only or Virus only") +
        theme_classic() +
        ggtitle(paste(unique(data$study), "- Bleed", unique(data$bleed), "- Virus", unique(data$primary)))
    plotly::ggplotly(cv_boxplot) %>% layout(boxmode = "group")
    '
}
