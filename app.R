#!/usr/bin/env Rscript

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
library(shinyWidgets)
library(metafolio)

# Import helper scripts
source("R/fct_1_input.R")
source("R/fct_2_qc.R")
source("R/fct_3_results.R")
source("R/app_server.R")
source("R/app_ui.R")
# devtools::load_all()

app = shiny::shinyApp(ui = ui, server = server)
