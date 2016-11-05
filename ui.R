
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboard)
library(caret)

#View(caret::modelLookup())

models <- c("xgbTree", "rf", "C5.0", "Boruta")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Import Data", tabName = "import", icon = icon("plus", lib = "glyphicon")),
    menuItem("Settings", tabName = "mlsettings", icon = icon("cog", lib = "glyphicon")),
    menuItem("Results", tabName = "mlresults", icon = icon("knight", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "import",
            fluidRow(
              box(height = 150, title = "Select your file", solidHeader = TRUE, status = "primary", fileInput("file", label = NULL)),
              box(height = 150, title = "Select the class variable", solidHeader = TRUE, status = "warning", uiOutput("classVarUI"))
            ),
            
            fluidRow(
              box(title = "Labels summary", solidHeader = TRUE, uiOutput("classVarSummaryUI")),
              box(title = "Data splitting", solidHeader = TRUE, status = "primary", 
                  sliderInput("trainSplit", "Select Train Percent", 0, 100, value = 70, step = 1),
                  uiOutput("splittingTableUI")
              ))
            
    ),
    
    tabItem(tabName = "mlsettings",
            fluidRow(
              box(title = "Add model configuration", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  column(width = 4,  actionButton("addModel", "Add", icon = icon("pushpin", lib = "glyphicon"))),
                  column(width = 4,  actionButton("clearModels", "Clear", icon = icon("trash", lib = "glyphicon"))),
                  column(width = 4,  actionButton("runModels", "Run",icon = icon("play", lib = "glyphicon")))),
              box(title = "Selected Models", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                  verbatimTextOutput("selectedModels")
              )
              ),
            fluidRow(
              box(title = "Model Selection", status = "primary", solidHeader = TRUE, 
                  selectInput("model", "Model", models),
                  checkboxInput("useTuneGrid", label = "Use hyperparameter optimization", value = FALSE),
                  uiOutput("tuneGridUI")),
              box(title = "Train Control", status = "danger", solidHeader = TRUE, 
                  uiOutput("trainControlMethodUI"),
                  uiOutput("trainControlNumberUI"),
                  uiOutput("trainControlRepeatsUI"),
                  uiOutput("trainControlPLGOCVUI"),
                  uiOutput("trainControlMethodDescriptionUI"))
            )
    ),
    
    tabItem(tabName = "mlresults",
            box(title = "Test the result output",
                verbatimTextOutput("results"))
    )
  )
)



dashboardPage(
  dashboardHeader(title = "ML Pipeline"),
  sidebar,
  body
)

