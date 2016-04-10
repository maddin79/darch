# MNIST example with pre-training
example.dropconnect <- function(dataFolder = "data/", downloadMNIST = F, ...)
{
  # Make sure to prove the correct folder if you have already downloaded the
  # MNIST data somewhere, or otherwise set downloadMNIST to TRUE
  provideMNIST(dataFolder, downloadMNIST)
  
  # Load MNIST data
  load(paste0(dataFolder, "train.RData")) # trainData, trainLabels
  load(paste0(dataFolder, "test.RData")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  darch  <- darch(trainDataSmall, trainLabelsSmall,
    layers = c(784,100,10),
    bootstrap = T,
    bootstrap.unique = F,
    darch.batchSize = 100,
    darch.dropout = .25,
    darch.dropout.dropConnect = T,
    darch.dropout.momentMatching = 10,
    bp.learnRate = 1,
    darch.unitFunction = c(rectifiedLinearUnit, softmaxUnit),
    darch.numEpochs = 25,
    ...
  )
  
  predictions <- predict(darch, newdata = testData, type = "class")
  
  labels <- cbind(predictions, testLabels)
  numIncorrect <- sum(apply(labels, 1, function(i) { any(i[1:10] != i[11:20]) }))
  cat(paste0("Incorrect classifications on test data: ", numIncorrect,
             " (", round(numIncorrect/nrow(testLabels)*100, 2), "%)\n"))
  
  darch
}
