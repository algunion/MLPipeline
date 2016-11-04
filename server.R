
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(yaml)
library(readr)
library(caret)

options(shiny.maxRequestSize=3000*1024^2) 

resamplingMethods <- c("none", "boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "oob")




shinyServer(function(input, output) {
  
  serverData <- reactiveValues()
  serverData$Ready <- FALSE
  
  output$classVarUI <- renderUI({
    
    if (! is.null(input$file)) {
      serverData$Imported <- read_csv(input$file$datapath)
      serverData$Ready <- TRUE
    }
    
    selectInput("labels", "Classes variable", colnames(serverData$Imported))
  })
  
  output$classVarSummaryUI <- renderUI({
    if (serverData$Ready) {
      verbatimTextOutput(outputId = "classVarSummary")
    }
  })
  
  output$classVarSummary <- renderPrint({
    if (serverData$Ready) {
      (summary((serverData$Imported[,input$labels])))
    }
  })
  
  output$trainControlMethodUI <- renderUI({
    resMethods <- resamplingMethods
    
    if (input$model == "xgbTree") {
      resMethods <- c("none", "boot", "cv", "LOOCV", "LGOCV", "repeatedcv")
    } else if (input$model == "rf") {
      resMethods <- c("none", "boot", "cv", "LOOCV", "LGOCV", "repeatedcv", "oob")
    } else if (input$model == "C5.0") {
      resMethods <- c("none", "boot", "cv", "LOOCV", "LGOCV", "repeatedcv")
    } else if (input$model == "Boruta") {
      resMethods <- c("none", "boot", "cv", "LOOCV", "LGOCV", "repeatedcv")
    }
    
    selectInput("trainControlMethod", "Method", resMethods)
  })
  
  output$trainControlNumberUI <- renderUI({
    if (input$trainControlMethod %in% c("cv", "repeatedcv", "boot", "LGOCV")) {
      sliderInput("trainControlNumber", "Select Number", 0, 20, value = 10, step = 1)
    }
  })
  
  output$trainControlRepeatsUI <- renderUI({
    if (input$trainControlMethod == "repeatedcv") {
      sliderInput("trainControlRepeats", "Select Repeats", 0, 50, value = 10, step = 1)
    }
  })
  
  output$trainControlPLGOCVUI <- renderUI({
    if (input$trainControlMethod == "LGOCV") {
      sliderInput("trainControlP", "Select p", 0, 100, value = 90, step = 1)
    }
  })
  
  output$trainControlMethodDescriptionUI <- renderUI ({
    if (input$trainControlMethod == "cv") {
      p("k-fold cross-validation randomly divides the data into k blocks of roughly equal size. Each of the blocks is left out in turn and the other k-1 blocks are used to train the model. The held out block is predicted and these predictions are summarized into some type of performance measure (e.g. accuracy, root mean squared error (RMSE), etc.). The k estimates of performance are averaged to get the overall resampled estimate. k is 10 or sometimes 5. Why? I have no idea. When k is equal to the sample size, this procedure is known as Leave-One-Out CV.")
      
    } else if(input$trainControlMethod == "repeatedcv") {
      p("Repeated k-fold CV does the same as above but more than once. For example, five repeats of 10-fold CV would give 50 total resamples that are averaged. Note this is not the same as 50-fold CV.")
    } else if (input$trainControlMethod == "LGOCV") {
      p("Leave Group Out cross-validation (LGOCV), aka Monte Carlo CV, randomly leaves out some set percentage of the data B times. It is similar to min-training and hold-out splits but only uses the training set.")
    } else if (input$trainControlMethod == "boot") {
      p("The bootstrap takes a random sample with replacement from the training set B times. Since the sampling is with replacement, there is a very strong likelihood that some training set samples will be represented more than once. As a consequence of this, some training set data points will not be contained in the bootstrap sample. The model is trained on the bootstrap sample and those data points not in that sample are predicted as hold-outs.")
    } else if (input$trainControlMethod == "LOOCV") {
      p("Standard CV when k is equal to the sample size.")
    }
  })
  

})
