library(shinydashboard)
library(rhandsontable)

dashboardPage(
    dashboardHeader(title = "Assay Analyser"),
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
                            title = "Dilutions*",
                            width = 6,
                            icon("question-circle", lib = "font-awesome"),
                            rHandsontableOutput("dilutions")
                        ),
                        box(
                            title = "Plate metadata*",
                            width = 12,
                            icon("question-circle", lib = "font-awesome"),
                            rHandsontableOutput("metadata")
                        )
                    )
            ),
            tabItem(tabName = "qc",
                    fluidRow(
                        box(
                            title = "2) QC"
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