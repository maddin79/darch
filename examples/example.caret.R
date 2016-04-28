library(caret)

# Example for using the caret integration, requires package e1071
example.caret <- function(...)
{
  data(iris)
  tc <- trainControl(method = "boot", number = 5, allowParallel = F,
    verboseIter = T)
  
  parameters <- data.frame(parameter = c("layers", "bp.learnRate", "darch.unitFunction"),
    class = c("character", "numeric", "character"),
    label = c("Network structure", "Learning rate", "unitFunction"))
  
  grid <- function(x, y, len = NULL, search = "grid")
  {
    df <- expand.grid(layers = c("c(0,20,0)","c(0,10,10,0)","c(0,10,5,5,0)"), bp.learnRate = c(1,2,5,10))
    
    df[["darch.unitFunction"]] <- rep(c("c(tanhUnit, softmaxUnit)", "c(tanhUnit, tanhUnit, softmaxUnit)", "c(tanhUnit, tanhUnit, tanhUnit, softmaxUnit)"), 4)
    
    df
  }
  
  darch <- train(Species ~ ., data = iris, tuneLength = 12, trControl = tc,
    method = darchModelInfo(parameters, grid), preProc = c("center", "scale"),
    darch.numEpochs = 15, darch.batchSize = 6, testing = T, ...)
  
  darch
}