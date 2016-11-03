
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Import Data", tabName = "import", icon = icon("dashboard")),
    menuItem("Settings", tabName = "mlsettings", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "import",
            h2("Import Data")
    ),
    
    tabItem(tabName = "mlsettings",
            h2("ML Settings")
    )
  )
)



dashboardPage(
  dashboardHeader(title = "ML Pipeline"),
  sidebar,
  body
)

