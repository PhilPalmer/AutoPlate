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

# Import helper scripts
source("R/1_input.R")
source("R/2_qc.R")
source("R/3_results.R")
source("R/server.R")
source("R/ui.R")
# devtools::load_all()

app = shiny::shinyApp(ui = ui, server = server)