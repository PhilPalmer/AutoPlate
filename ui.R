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
    dashboardBody()
)
