#!/usr/bin/env Rscript

#################################################
# The user-interface of the AutoPlate application
#################################################

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "AutoPlate",
        tags$li(tags$a(
          span(icon("book"),"Docs"), href = "https://philpalmer.github.io/AutoPlate/", target = "_blank"
        ), class = "dropdown")
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Home", tabName = "home", icon = icon("home")),
          shinydashboard::menuItem("1) Input", tabName = "input", icon = icon("sign-in-alt")),
          shinydashboard::menuItem("2) QC", tabName = "qc", icon = icon("check-square")),
          shinydashboard::menuItem("3) Results", tabName = "results", icon = icon("chart-line"))
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "home",
            shiny::tagList(
              shiny::fluidRow(
                shiny::column(
                  width = 9,
                  shiny::fluidRow(
                    shinydashboard::box(
                      width = 12, status = "primary",
                      shiny::div(shiny::img(
                        src = "www/favicon.png",
                        width = 300, id = "logo"
                      ),
                      style = "text-align:left; float:right;"
                      ),
                      h3("Automate Your 96-Well Plate Analysis with AutoPlate!"),
                      h4("What is AutoPlate?"),
                      p(
                        "AutoPlate is an ", a(href = "https://shiny.rstudio.com/", "R Shiny web application"),
                        "(and R library) that helps you automate the analysis of biological assays conducted on 96-well plates.",
                        "It lets you go from raw data to publication ready figures in minutes!"
                      ),
                      h4("What biological assays can I analyse?"),
                      p("Currently, the only supported assay types are the ", a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6526431/", " Pseudotype Micro Neutralisation (pMN)", .noWS = "outside"),
                        " and ELLA assays, for which dose-response curves can be fit.",
                        "In the future, other assays such as ELISA, HIA or even any custom assay may be supported.",
                        "Let us know if there's an assay that you would like us to support!",
                        .noWS = c("after-begin", "before-end")
                      ),
                      h4("How does it work?"),
                      p("An analysis can be run in three simple steps:"),
                      tags$ol(
                        uiOutput(outputId = "steps")
                      )
                    )
                  ),
                  shiny::fluidRow(
                    shinydashboard::box(
                      width = 12, status = "primary",
                      h3("Citing AutoPlate"),
                      p("AutoPlate is yet to be published but we're hoping to change this soon!")
                    )
                  )
                ),
                shiny::column(
                  width = 3,
                  shiny::fluidRow(
                    shinydashboard::valueBoxOutput("autoplate_version", width = 12)
                  ),
                  shiny::fluidRow(
                    shinydashboard::box(
                      width = 12, status = "primary",
                      h3("Contact"),
                      p("If you have questions or feedback, please don't hesitate to contact me!"),
                      shiny::fluidRow(
                        shinydashboard::valueBoxOutput("new_issue", width = 12)
                      ),
                      shiny::fluidRow(
                        shinydashboard::valueBoxOutput("email", width = 12)
                      )
                    )
                  )
                )
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "input",
            fluidRow(
              column(6, 
                shinydashboard::box(
                  title = "1) Assay Type",
                  width = NULL,
                  uiOutput("assay_type")
                ),
                shinydashboard::box(
                  title = "2) Luminescence files",
                  width = NULL,
                  uiOutput(outputId = "tooltip_input_files"),
                  fileInput("luminescence_files",
                    span("Please upload all input luminescence files",
                      tags$a(
                          "(see supported input formats)",
                          href = "https://philpalmer.github.io/AutoPlate/articles/web_app.html",
                          target = "_blank"
                        )
                    ),
                    multiple = TRUE,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv",
                      ".xls",
                      ".xlsx"
                    )
                  ),
                  uiOutput(outputId = "message_input_files"),
                )
              ),
              column(6,
                shinydashboard::box(
                  title = "3) Concentrations/dilutions",
                  width = NULL,
                  uiOutput(outputId = "tooltip_dilutions"),
                  rhandsontable::rHandsontableOutput("dilutions")
                )
              ),
              shinydashboard::box(
                title = "4) 96-Well Plate Data",
                width = 12,
                uiOutput(outputId = "tooltip_plates"),
                div(uiOutput("plate_feature"), class = 'inline control'),
                uiOutput("plate_tabs"),
                rhandsontable::rHandsontableOutput("plate_data")
              ),
              shinydashboard::box(
                title = "5) Other features",
                width = 12,
                uiOutput(outputId = "tooltip_features"),
                shinydashboard::tabBox(
                  title = "",
                  width = 8,
                  # The id lets us use input$tabset_features on the server to find the current tab
                  id = "tabset_features",
                  tabPanel(
                    "Bleed",
                    fluidRow(
                      shiny::column(width = 3, uiOutput("bleed")),
                      shiny::column(width = 8,
                        conditionalPanel(
                          condition = "input.bleed == 'well'",
                          textInput("bleed_text", NULL)
                        ),
                        conditionalPanel(
                          condition = "input.bleed != 'well'",
                          rhandsontable::rHandsontableOutput("bleed_table"),
                        )
                      ),
                      shiny::column(width = 1,
                        actionButton("go_bleed", "Submit", icon("check-circle")),
                      )
                    )
                  ),
                  tabPanel(
                    "Treatment",
                    actionButton("go_treatment", "Submit feature", icon("check-circle")),
                    uiOutput("treatment"),
                    rhandsontable::rHandsontableOutput("treatment_table")
                  ),
                  tabPanel(
                    "Virus",
                    actionButton("go_virus", "Submit feature", icon("check-circle")),
                    uiOutput("virus"),
                    rhandsontable::rHandsontableOutput("virus_table")
                  ),
                  tabPanel(
                    "Experiment ID",
                    actionButton("go_experiment_id", "Submit feature", icon("check-circle")),
                    uiOutput("experiment_id"),
                    rhandsontable::rHandsontableOutput("experiment_id_table")
                  )
                )
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "qc",
            fluidRow(
              shinydashboard::tabBox(
                title = "",
                width = 12,
                # The id lets us use input$tabset_qc on the server to find the current tab
                id = "tabset_qc",
                tabPanel("Average luminescence values", tableOutput("av_lum")),
                tabPanel("Types"),
                tabPanel("Sample ID"),
                tabPanel("Dilution"),
                tabPanel("Virus"),
                tabPanel("RLU"),
                tabPanel("Neutralisation"),
                tabPanel("Treatment"),
                tabPanel("Experiment ID"),
                tabPanel("Bleed"),
                tabPanel("Exclude"),
                tabPanel("Types Boxplot", plotly::plotlyOutput("types_boxplot"))
              ),
              shinydashboard::box(
                width = 12,
                uiOutput(outputId = "tooltip_exclude"),
                textInput("exclude_wells",
                  label = tags$div(
                    h4("Wells to exclude from analysis (specify any combination of the following, comma-seperated)"),
                    tags$ul(
                      tags$li("whole plates - by number, eg: 1,2,3"),
                      tags$li("individual wells - by their plate number, row and column, eg: 1A1,1D6,3H12"),
                      tags$li("range of wells - by their plate numbers, rows and columns, eg: 1A1:A12,2B6:H12")
                    )
                  ),
                  placeholder = "1,2A1,3A1:B12"
                ),
                uiOutput(outputId = "tooltip_download_data"),
                downloadButton("download_data", "Download CSV data"),
                uiOutput("heatmaps")
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "results",
            fluidRow(
              shinydashboard::box(
                width = 12,
                uiOutput(outputId = "tooltip_download_report"),
                downloadButton("download_report", "Download HTML Report"),
                br(), br(),
                textInput("drm_string", uiOutput(outputId = "message_drm_string"), value = "formula=neutralisation~dilution, curveid=sample_id, fct=drc::LL2.4(), data=data, pmodels=data.frame(1,1,1,sample_id), upperl=c(NA,NA,100,NA), lowerl=c(0,NA,NA,0)", width = "100%"),
                shinydashboard::tabBox(
                  width = 12,
                  # The id lets us use input$tabset_results on the server to find the current tab
                  id = "tabset_results",
                  tabPanel(
                    "Data Exploration",
                    downloadButton("download_data_exploration", "Download SVG Plot"),
                    br(), br(),
                    shinydashboard::tabBox(
                      width = 12, height = "100vh",
                      tabPanel("View Plot", plotly::plotlyOutput("data_exploration")),
                      tabPanel("View code", uiOutput("data_exploration_code"))
                    )
                  ),
                  tabPanel(
                    "Dose Response Curve",
                    div(downloadButton("download_drc", "Download SVG Plot"), class = 'inline control'),
                    div(uiOutput("virus_drc"), class = 'inline control'),
                    br(), br(),
                    shinydashboard::tabBox(
                      width = 12, height = "100vh",
                      tabPanel("View Plot", plotly::plotlyOutput("drc")),
                      tabPanel("View code", uiOutput("drc_code"))
                    )
                  ),
                  tabPanel(
                    "IC50 Boxplot",
                    div(
                      downloadButton("download_ic50", "Download SVG Plot"), class = 'inline control',
                      br(),br(),
                      div(downloadButton("download_ied", "Download CSV IED Table"), class = 'inline control')
                    ),
                    div(uiOutput("virus_ic50"), class = 'inline control'),
                    div(br(), class = 'inline space'),
                    div(shinyWidgets::switchInput(inputId = "ic50_is_boxplot", value = TRUE, onLabel = "Boxplot", offLabel = "Scatter plot"), class = 'inline control'),
                    br(),br(),
                    shinydashboard::tabBox(
                      width = 12, height = "100vh",
                      tabPanel("View Plot", plotly::plotlyOutput("ic50_boxplot")),
                      tabPanel("View code", uiOutput("ic50_boxplot_code"))
                    )
                  ),
                  tabPanel(
                    "Virus + Cell Boxplot",
                    downloadButton("download_cv_boxplot", "Download SVG Plot"),
                    br(), br(),
                    shinydashboard::tabBox(
                      width = 12, height = "100vh",
                      tabPanel("View Plot", plotly::plotlyOutput("cv_boxplot")),
                      tabPanel("View code", uiOutput("cv_boxplot_code"))
                    )
                  )
                  # tabPanel("Posotive Control")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  golem::add_resource_path(
     'www', system.file('app/www', package = 'autoplate')
  )
 
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('inst/app/www'),
      app_title = 'AutoPlate'
    ),
    # Prism for syntax highting
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")
  )
}

