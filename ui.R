library(shinydashboard)

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("1) Input", tabName = "input", icon = icon("sign-in-alt")),
            menuItem("2) QC", tabName = "qc", icon = icon("check-square")),
            menuItem("3) Analyse", tabName = "analyse", icon = icon("chart-line"))
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
                            title = "1) Input"
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
            tabItem(tabName = "analyse",
                    fluidRow(
                        box(
                            title = "3) Analyse"
                        )
                    )
            )
        )
    )
)