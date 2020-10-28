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
                            fileInput("luminescence_files", "Please select all luminescence readout CSV files",
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
                            ),
                            h4("Export to CSV once data entry complete"),
                            downloadButton("downloadData", "Download")
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
                                tabPanel("Types",
                                        ),
                                tabPanel("Subject",
                                        ),
                                tabPanel("Dilution",
                                        ),
                                tabPanel("Primary",
                                        ),
                                tabPanel("RLU",
                                ),
                                tabPanel("Neutralisation",
                                        ),
                                tabPanel("Inoculate",
                                ),
                                tabPanel("Study",
                                ),
                                tabPanel("Bleed",
                                )
                        )
                    )
            ),
            tabItem(tabName = "results",
                    fluidRow(
                        box(
                            title = "3) Results"
                        )
                    )
            )
        )
    )
)