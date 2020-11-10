library(shinydashboard)
library(rhandsontable)

dashboardPage(
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
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        tabItems(
            tabItem(tabName = "home",
                    fluidRow(
                        box(
                            title = "Home"
                        )
                    )
            ),
            tabItem(tabName = "input",
                    fluidRow(
                        box(
                            title = "Luminescence files*",
                            width = 6,
                            icon("question-circle", lib = "font-awesome"),
                            fileInput("luminescence_files", "Please select all luminescence readout CSV files (OR a CSV exported from AutoPlate)",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv"))
                        ),
                        box(
                            title = "Concentrations/dilutions*",
                            width = 6,
                            icon("question-circle", lib = "font-awesome"),
                            rHandsontableOutput("dilutions")
                        ),
                        box(
                            title = "96-Well Plate Data*",
                            width = 12,
                            icon("question-circle", lib = "font-awesome"),
                            # TODO: Hide message below once user has uploaded their luminescence files
                            "Before entering the plate data you must upload your luminescence files above.",
                            uiOutput('plate_tabs'),
                            rHandsontableOutput("plate_data")
                        ),
                        box(
                            title = "Other features*",
                            width = 12,
                            icon("question-circle", lib = "font-awesome"),
                            tabBox(
                                title = "",
                                width = 8,
                                # The id lets us use input$tabset_features on the server to find the current tab
                                id = "tabset_features",
                                tabPanel("Bleed",
                                         actionButton("go_bleed", "Submit feature", icon("check-circle")),
                                         uiOutput("bleed"),
                                         rHandsontableOutput("bleed_table")
                                        ),
                                tabPanel("Inoculate",
                                         actionButton("go_inoculate", "Submit feature", icon("check-circle")),
                                         uiOutput("inoculate"),
                                         rHandsontableOutput("inoculate_table")
                                        ),
                                tabPanel("Primary",
                                         actionButton("go_primary", "Submit feature", icon("check-circle")),
                                         uiOutput("primary"),
                                         rHandsontableOutput("primary_table")
                                        ),
                                tabPanel("Study",
                                         actionButton("go_study", "Submit feature", icon("check-circle")),
                                         uiOutput("study"),
                                         rHandsontableOutput("study_table")
                                        )
                            )
                        )
                    )
            ),
            tabItem(tabName = "qc",
                    fluidRow(
                        tabBox(
                                title = "",
                                width = 12,
                                # The id lets us use input$tabset_qc on the server to find the current tab
                                id = "tabset_qc",
                                tabPanel("Average luminescence values",tableOutput('av_lum')),
                                tabPanel("Types"),
                                tabPanel("Subject"),
                                tabPanel("Dilution"),
                                tabPanel("Primary"),
                                tabPanel("RLU"),
                                tabPanel("Neutralisation"),
                                tabPanel("Inoculate"),
                                tabPanel("Study"),
                                tabPanel("Bleed"),
                                tabPanel("Exclude")
                        ),
                        box(
                            width = 12,
                            icon("question-circle", lib = "font-awesome"),
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
                            icon("question-circle", lib = "font-awesome"),
                            downloadButton("download_data", "Download CSV data"),
                            uiOutput("heatmaps")
                        )
                    )
            ),
            tabItem(tabName = "results",
                    fluidRow(
                        box(
                            width = 12,
							downloadButton("download_report", "Download HTML Report"),
							br(), br(),
                            textInput("drm_string", "DRM model", value="formula=neutralisation~dilution, curveid=subject, fct=LL2.4(), data=data, pmodels=data.frame(1,1,1,subject), upperl=c(NA,NA,100,NA), lowerl=c(0,NA,NA,NA)", width="100%"),
                            tabBox(
                                    width = 12,
                                    # The id lets us use input$tabset_results on the server to find the current tab
                                    id = "tabset_results",
                                    tabPanel("Data Exploration",
										downloadButton("download_data_exploration", "Download SVG Plot"),
										br(), br(),
                                        tabBox(
                                            width = 12,
                                            tabPanel("View Plot", plotOutput("data_exploration")),
                                            tabPanel("View code", verbatimTextOutput("data_exploration_code"))
                                        )
                                    ),
                                    tabPanel("Dose Response Curve",
										downloadButton("download_drc", "Download SVG Plot"),
										br(), br(),
                                        tabBox(
                                            width = 12,
                                            tabPanel("View Plot", plotOutput("drc")),
                                            tabPanel("View code", verbatimTextOutput("drc_code"))
                                        )
                                    ),
                                    tabPanel("IC50 Boxplot",
										downloadButton("download_ic50", "Download SVG Plot"),
										br(), br(),
                                        tabBox(
                                            width = 12,
                                            tabPanel("View Plot", plotOutput("ic50_boxplot")),
                                            tabPanel("View code", verbatimTextOutput("ic50_boxplot_code"))
                                        )
                                    ),
                                    tabPanel("Virus + Cell Boxplot",
										downloadButton("download_cv_boxplot", "Download SVG Plot"),
										br(), br(),
                                        tabBox(
                                            width = 12,
                                            tabPanel("View Plot", plotOutput("cv_boxplot")),
                                            tabPanel("View code", verbatimTextOutput("cv_boxplot_code"))
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