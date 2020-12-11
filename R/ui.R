#!/usr/bin/env Rscript

#################################################
# The user-interface of the AutoPlate application
#################################################

library(shinydashboard)
library(rhandsontable)
library(plotly)
library(shinyWidgets)

##' UI main function
##'
##' @param request Internal parameter for `{shiny}`.
##'
##' @noRd
ui <- dashboardPage(
  # includeCSS("www/styles.css"),
  dashboardHeader(title = "AutoPlate"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("1) Input", tabName = "input", icon = icon("sign-in-alt")),
      menuItem("2) QC", tabName = "qc", icon = icon("check-square")),
      menuItem("3) Results", tabName = "results", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      # Prism for syntax highting
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")
    ),
    tabItems(
      tabItem(
        tabName = "home",
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 9,
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12, status = "primary",
                  shiny::div(shiny::img(
                    src = "images/virus.svg",
                    width = 300, id = "logo"
                  ),
                  style = "text-align:left; float:right;"
                  ),
                  h3("Automate Your 96-Well Plate Analysis with AutoPlate!"),
                  h4("What is AutoPlate?"),
                  p(
                    "AutoPlate is an ", a(href = "https://shiny.rstudio.com/", "R Shiny web application"),
                    "(and UI) that helps you automate the analysis of biological assays conducted on 96-well plates.",
                    "It lets you go from raw data to publication ready figures in minutes!"
                  ),
                  h4("What biological assays can I analyse?"),
                  p("Currently, the only supported assay type is the ", a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6526431/", " Pseudotype Micro Neutralisation (pMN) assay", .noWS = "outside"),
                    ", for which dose-response curves can be fit.",
                    "In the future, other assays such as ELLA, ELISA, HIA or even any custom assay may be supported.",
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
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            title = "Luminescence files*",
            width = 6,
            uiOutput(outputId = "message_input_files"),
            uiOutput(outputId = "tooltip_input_files"),
            fileInput("luminescence_files", "Please select all luminescence readout CSV files (OR a CSV exported from AutoPlate)",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          box(
            title = "Concentrations/dilutions*",
            width = 6,
            uiOutput(outputId = "tooltip_dilutions"),
            rHandsontableOutput("dilutions")
          ),
          box(
            title = "96-Well Plate Data*",
            width = 12,
            uiOutput(outputId = "tooltip_plates"),
            uiOutput("plate_tabs"),
            rHandsontableOutput("plate_data")
          ),
          box(
            title = "Other features*",
            width = 12,
            uiOutput(outputId = "tooltip_features"),
            tabBox(
              title = "",
              width = 8,
              # The id lets us use input$tabset_features on the server to find the current tab
              id = "tabset_features",
              tabPanel(
                "Bleed",
                actionButton("go_bleed", "Submit feature", icon("check-circle")),
                uiOutput("bleed"),
                rHandsontableOutput("bleed_table")
              ),
              tabPanel(
                "Inoculate",
                actionButton("go_inoculate", "Submit feature", icon("check-circle")),
                uiOutput("inoculate"),
                rHandsontableOutput("inoculate_table")
              ),
              tabPanel(
                "Primary",
                actionButton("go_primary", "Submit feature", icon("check-circle")),
                uiOutput("primary"),
                rHandsontableOutput("primary_table")
              ),
              tabPanel(
                "Study",
                actionButton("go_study", "Submit feature", icon("check-circle")),
                uiOutput("study"),
                rHandsontableOutput("study_table")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "qc",
        fluidRow(
          tabBox(
            title = "",
            width = 12,
            # The id lets us use input$tabset_qc on the server to find the current tab
            id = "tabset_qc",
            tabPanel("Average luminescence values", tableOutput("av_lum")),
            tabPanel("Types"),
            tabPanel("Subject"),
            tabPanel("Dilution"),
            tabPanel("Primary"),
            tabPanel("RLU"),
            tabPanel("Neutralisation"),
            tabPanel("Inoculate"),
            tabPanel("Study"),
            tabPanel("Bleed"),
            tabPanel("Exclude"),
            tabPanel("Types Boxplot", plotlyOutput("types_boxplot"))
          ),
          box(
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
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
            uiOutput(outputId = "tooltip_download_report"),
            downloadButton("download_report", "Download HTML Report"),
            br(), br(),
            textInput("drm_string", uiOutput(outputId = "message_drm_string"), value = "formula=neutralisation~dilution, curveid=subject, fct=LL2.4(), data=data, pmodels=data.frame(1,1,1,subject), upperl=c(NA,NA,100,NA), lowerl=c(0,NA,NA,0)", width = "100%"),
            tabBox(
              width = 12,
              # The id lets us use input$tabset_results on the server to find the current tab
              id = "tabset_results",
              tabPanel(
                "Data Exploration",
                downloadButton("download_data_exploration", "Download SVG Plot"),
                br(), br(),
                tabBox(
                  width = 12,
                  tabPanel("View Plot", plotlyOutput("data_exploration")),
                  tabPanel("View code", uiOutput("data_exploration_code"))
                )
              ),
              tabPanel(
                "Dose Response Curve",
                downloadButton("download_drc", "Download SVG Plot"),
                br(), br(),
                tabBox(
                  width = 12,
                  tabPanel("View Plot", plotlyOutput("drc")),
                  tabPanel("View code", uiOutput("drc_code"))
                )
              ),
              tabPanel(
                "IC50 Boxplot",
                downloadButton("download_ic50", "Download SVG Plot"),
                br(), br(),
                switchInput(inputId = "ic50_is_boxplot", value = TRUE, onLabel = "Boxplot", offLabel = "Scatter plot"),
                tabBox(
                  width = 12,
                  tabPanel("View Plot", plotlyOutput("ic50_boxplot")),
                  tabPanel("View code", uiOutput("ic50_boxplot_code"))
                )
              ),
              tabPanel(
                "Virus + Cell Boxplot",
                downloadButton("download_cv_boxplot", "Download SVG Plot"),
                br(), br(),
                tabBox(
                  width = 12,
                  tabPanel("View Plot", plotlyOutput("cv_boxplot")),
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
