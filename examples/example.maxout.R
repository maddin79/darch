# MNIST example using dropout and maxout
example.maxout <- function(dataFolder = "data/", downloadMNIST = F, ...)
{
  provideMNIST(dataFolder, downloadMNIST)
  
  load(paste0(dataFolder, "train.RData")) # trainData, trainLabels
  load(paste0(dataFolder, "test.RData")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size = 1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  darch <- darch(x = trainDataSmall, y = trainLabelsSmall,
    xValid = testData, yValid = testLabels,
    layers = c(784, 500, 10),
    darch.batchSize = 100,
    darch.dropout = .5,
    darch.dropout.oneMaskPerEpoch = T,
    # custom activation functions
    darch.unitFunction = c(maxoutUnit, softmaxUnit),
    darch.maxout.poolSize = 5,
    darch.maxout.unitFunction = exponentialLinearUnit,
    darch.elu.alpha = 2,
    darch.weightUpdateFunction = c(maxoutWeightUpdate, weightDecayWeightUpdate),
    darch.numEpochs = 5,
    logLevel = "DEBUG",
    ...
  )
  
  darch
}