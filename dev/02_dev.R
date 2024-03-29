# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("rhandsontable")
usethis::use_package("tidyr")
usethis::use_package("dplyr")
usethis::use_package("plot.matrix")
usethis::use_package("viridis")
usethis::use_package("ggplot2")
usethis::use_package("drc")
usethis::use_package("rmarkdown")
usethis::use_package("knitr")
usethis::use_package("svglite")
usethis::use_package("plotly")
usethis::use_package("shinyWidgets")
usethis::use_package("metafolio")
usethis::use_package("magrittr")
usethis::use_package("grDevices")
usethis::use_package("graphics")
usethis::use_package("stats")
usethis::use_package("utils")
usethis::use_package("readxl")
usethis::use_package("ggprism")
usethis::use_package("formattable", "suggests")
usethis::use_package("shinycssloaders")

## Add modules ----
## Create a module infrastructure in R/
# golem::add_module( name = "name_of_module1" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("1_input")
golem::add_fct("2_qc")
golem::add_fct("3_results")
# golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file( "script" )
# golem::add_js_handler( "handlers" )
golem::add_css_file( "styles" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "dilutions", open = FALSE )
usethis::use_data_raw( name = "example_data", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("autoplate")
usethis::use_vignette("r_lib")
usethis::use_vignette("run_your_own_app")
usethis::use_vignette("shiny_app")
usethis::use_vignette("web_app")
devtools::build_vignettes()

# Add code coverage
usethis::use_coverage()
usethis::use_github_action("test-coverage")

# Add more GitHub actions
usethis::use_github_action(url = "https://raw.githubusercontent.com/r-lib/actions/master/examples/pkgdown.yaml")

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_github_actions()
usethis::use_github_actions_badge()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

