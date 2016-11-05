
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

xgb_grid_default = expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1
)

rf_grid_default <- expand.grid(mtry = seq(4,16,4), ntree = c(700, 1000,2000) )

trainFunction <- function(input, serverData) {
  localLabels <- factor(serverData$Imported[,input$labels])
  trainIndexes <- createDataPartition(y = localLabels, p = input$trainSplit / 100, list = FALSE)
  
  
  trainLabels <- localLabels[trainIndexes]
  testLabels <- localLabels[-trainIndexes]
  
  trainData <- serverData$Imported[trainIndexes,]
  testData <- serverData$Imported[trainIndexes,]
  
  trainData[, input$labels] <- NULL
  testData[, input$labels] <- NULL
  
  for (model in serverData$SelectedModels) {
    print(model)
    fittedModel <- train(x = trainData, y = trainLabels, method = model, trControl = serverData[[model]]$trainControl, tuneGrid = serverData[[model]]$tuneGrid)
    print(model)
    isolate(serverData[[model]]$fittedModel <- fittedModel)
  }
}


shinyServer(function(input, output) {
  
  serverData <- reactiveValues()
  serverData$Ready <- FALSE
  
  serverAddModel <- 0
  serverClearModels <- 0
  serverRunModels <- 0
  
  output$classVarUI <- renderUI({
    
    if (! is.null(input$file)) {
      serverData$Imported <- data.frame(read_csv(input$file$datapath))
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
      (summary(factor(serverData$Imported[,input$labels])))
    }
  })
  
  output$splittingTableUI <- renderUI({
    if (serverData$Ready) {
      verbatimTextOutput("dataSplittingCases")
    }
  })
  
  output$dataSplittingCases <- renderPrint({
    if (serverData$Ready) {
      localLabels <- factor(serverData$Imported[,input$labels])
      trainIndexes <- createDataPartition(y = localLabels, p = input$trainSplit / 100, list = FALSE)
      trainLabels <- localLabels[trainIndexes]
      testLabels <- localLabels[-trainIndexes]
      
      print(table(trainLabels))
      print(table(testLabels))
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
      p("Repeated k-fold CV does the same as CV but more than once. For example, five repeats of 10-fold CV would give 50 total resamples that are averaged. Note this is not the same as 50-fold CV.")
    } else if (input$trainControlMethod == "LGOCV") {
      p("Leave Group Out cross-validation (LGOCV), aka Monte Carlo CV, randomly leaves out some set percentage of the data B times. It is similar to min-training and hold-out splits but only uses the training set.")
    } else if (input$trainControlMethod == "boot") {
      p("The bootstrap takes a random sample with replacement from the training set B times. Since the sampling is with replacement, there is a very strong likelihood that some training set samples will be represented more than once. As a consequence of this, some training set data points will not be contained in the bootstrap sample. The model is trained on the bootstrap sample and those data points not in that sample are predicted as hold-outs.")
    } else if (input$trainControlMethod == "LOOCV") {
      p("Standard CV when k is equal to the sample size.")
    }
  })
  
  output$tuneGridUI <- renderUI({
    
    if (input$useTuneGrid && input$model == "xgbTree") {
      column( width = 12,
              numericInput("tuneGridXGBoostNRounds", label = "nrounds", value = 1000, min = 0, max = 10000, step = 1),
              sliderInput("tuneGridXGBoostEta", label = "eta", min = 0, max = 0.05, value = c(0.01, 0.03), step = 0.0001),
              numericInput("tuneGridXGBoostMaxDepthStep", label = "maxdepth step", value = 2, min = 1, max = 10, step = 1),
              sliderInput("tuneGridXGBoostMaxDepth", label = "maxdepth", min = 2, max = 30, value = c(2, 10), step = 1),
              numericInput("tuneGridXGBoostGammaStep", label = "gamma step", value = 1, min = 1, max = 5, step = 1),
              sliderInput("tuneGridXGBoostGamma", label = "gamma", min = 0, max = 10, value = c(1, 3), step = 1))
    } else if (input$useTuneGrid && input$model %in% c("rf", "Boruta")) {
      column( width = 12,
              numericInput("tuneGridRFMtryStep", label = "mtry step", value = 4, min = 1, max = 8, step = 1),
              sliderInput("tuneGridRFMtry", label = "mtry", min = 1, max = 40, value = c(4, 16), step = 1),
              numericInput("tuneGridRFNTreeStep", label = "ntree step", value = 500, min = 100, max = 1000, step = 100),
              sliderInput("tuneGridRFNTree", label = "ntree", min = 100, max = 10000, value = c(1000, 5000), step = 100))
    } else if (input$useTuneGrid && input$model == "C5.0") {
      column( width = 12,
              numericInput("tuneGridC50TrialsStep", label = "trials step", value = 5, min = 1, max = 10, step = 1),
              sliderInput("tuneGridC50Trials", label = "trials", min = 2, max = 100, value = c(5, 30), step = 1),
              numericInput("tuneGridC50SplitsStep", label = "splits step", value = 2, min = 1, max = 10, step = 1),
              sliderInput("tuneGridC50Splits", label = "splits", min = 2, max = 100, value = c(2, 10), step = 1))
    }
  })
  
  
  output$selectedModels <- renderPrint({
    
    if (input$clearModels > serverClearModels) {
      serverClearModels <<- input$clearModels
      isolate({
        serverData$SelectedModels <- c()
      })
    }
    
    if (input$addModel > serverAddModel) {
      serverAddModel <<- input$addModel
      isolate( {
        if (!input$model %in% serverData$SelectedModels) {
          serverData$SelectedModels <- c(serverData$SelectedModels, input$model)
        }
        
        localTrainControl <- NULL
        
        if (input$trainControlMethod != "none") {
          if (input$trainControlMethod %in% c("boot", "cv")) {
            localTrainControl <- trainControl(method = input$trainControlMethod, number = input$trainControlNumber)
          } else if (input$trainControlMethod == "repeatedcv") {
            localTrainControl <- trainControl(method = input$trainControlMethod, number = input$trainControlNumber, repeats = input$trainControlRepeats)
          } else if (input$trainControlMethod == "LGOCV") {
            localTrainControl <- trainControl(method = input$trainControlMethod, p = input$trainControlP)
          } else {
            localTrainControl <- trainControl(method = input$trainControlMethod)
          }
          
          serverData[[input$model]]$trainControl <- localTrainControl
          
        }
        
        if (input$model == "xgbTree") {
          if (input$useTuneGrid) {
            serverData$xgbTree$tuneGrid <- expand.grid(
              nrounds = input$tuneGridXGBoostNRounds,
              eta = seq(input$tuneGridXGBoostEta[1], input$tuneGridXGBoostEta[2], (input$tuneGridXGBoostEta[2] - input$tuneGridXGBoostEta[1]) / 4),
              max_depth = seq(input$tuneGridXGBoostMaxDepth[1], input$tuneGridXGBoostMaxDepth[2], input$tuneGridXGBoostMaxDepthStep),
              gamma = seq(input$tuneGridXGBoostGamma[1], input$tuneGridXGBoostGamma[2], input$tuneGridXGBoostGammaStep),
              colsample_bytree = 1,
              min_child_weight = 1,
              subsample = 1
            )
            
            serverData$xgbTree$useTuneGrid <- TRUE
          }
        } else if (input$model == "rf") {
          if (input$useTuneGrid) {
            serverData[[input$model]]$tuneGrid <- expand.grid(
              mtry = seq(input$tuneGridRFMtry[1], input$tuneGridRFMtry[2], input$tuneGridRFMtryStep),
              ntree = seq(input$tuneGridRFNTree[1], input$tuneGridRFNTree[2], input$tuneGridRFNTreeStep)
            )
            
            serverData[[input$model]]$useTuneGrid <- TRUE
          }
        } else if (input$model == "C5.0") {
          if (input$useTuneGrid) {
            serverData[[input$model]]$tuneGrid <- expand.grid(
              trials = seq(input$tuneGridC50Trials[1], input$tuneGridC50Trials[2], input$tuneGridC50TrialsStep),
              #split = seq(input$tuneGridC50Splits[1], input$tuneGridC50Splits[2], input$tuneGridC50SplitsStep),
              model = "tree",
              winnow = FALSE,
              columns = 1
            )
            
            serverData[[input$model]]$useTuneGrid <- TRUE
          }
        } else if (input$model == "Boruta") {
          if (input$useTuneGrid) {
            serverData[[input$model]]$tuneGrid <- expand.grid(
              mtry = seq(input$tuneGridRFMtry[1], input$tuneGridRFMtry[2], input$tuneGridRFMtryStep),
              ntree = seq(input$tuneGridRFNTree[1], input$tuneGridRFNTree[2], input$tuneGridRFNTreeStep)
            )
            
            serverData[[input$model]]$useTuneGrid <- TRUE
          }
        }
      })
      
    }
    
    str(serverData$SelectedModels)
  })
  
  output$results <- renderPrint({
    if (input$runModels > serverRunModels) {
      
      
      localLabels <- factor(serverData$Imported[,input$labels])
      trainIndexes <- createDataPartition(y = localLabels, p = input$trainSplit / 100, list = FALSE)
      
      
      trainLabels <- localLabels[trainIndexes]
      testLabels <- localLabels[-trainIndexes]
      
      trainData <- serverData$Imported[trainIndexes,]
      testData <- serverData$Imported[trainIndexes,]
      
      trainData[, input$labels] <- NULL
      testData[, input$labels] <- NULL
      
      trainData <- sapply(X = trainData, FUN = as.numeric)
      
      print(dim(trainData))
      print(dim(testData))
      
      fittedModel <- NULL
      
      for (model in serverData$SelectedModels) {
        
        if (input$useTuneGrid && input$trainControlMethod != "none") {
          fittedModel <- train(x = trainData, y = trainLabels, method = model, trControl = serverData[[model]]$trainControl, tuneGrid = serverData[[model]]$tuneGrid)
        } else if (input$useTuneGrid) {
          fittedModel <- train(x = trainData, y = trainLabels, method = model, tuneGrid = serverData[[model]]$tuneGrid)
        } else if (input$trainControlMethod != "none") {
          fittedModel <- train(x = trainData, y = trainLabels, method = model, trControl = serverData[[model]]$trainControl)
        } else {
          fittedModel <- train(x = trainData, y = trainLabels, method = model)
        }
        
        print(summary(fittedModel))
        print(fittedModel)
        
            #print(model)
            #serverData[[model]]$fittedModel <- fittedModel
          }
      
      # isolate({
      #   
      # 
      #   for (model in serverData$SelectedModels) {
      #     print(model)
      #     fittedModel <- train(x = trainData, y = trainLabels, method = model, trControl = serverData[[model]]$trainControl, tuneGrid = serverData[[model]]$tuneGrid)
      #     print(model)
      #     serverData[[model]]$fittedModel <- fittedModel
      #   }
      # })
      
      
      
      #print(summary(serverData[["xgbTree"]]$fittedModel))
    }
  })
  

})
