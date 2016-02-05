example.maxout <- function(dataFolder = "data/", downloadMNIST = F)
{
  provideMNIST(dataFolder, downloadMNIST)
  
  load(paste0(dataFolder, "train.RData")) # trainData, trainLabels
  load(paste0(dataFolder, "test.RData")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  darch <- darch(x = trainDataSmall, y = trainLabelsSmall,
    rbm.numEpochs = 0,
    
    # DArch constructor arguments
    layers = c(784,500,10), # required
    darch.batchSize = 100,
    darch.learnRate = 1,
    darch.dropoutHidden = .5,
    darch.dropoutInput = .2,
    # custom activation functions
    darch.unitFunction = c(maxoutUnit, softmaxUnit),
    darch.unitFunction.maxout.poolSize = 5,
    darch.weightUpdateFunction = c(maxoutWeightUpdate, weightDecayWeightUpdate),
    darch.retainData = F,
    darch.numEpochs = 5
  )

  e <- testDarch(darch, data=testData, targets=testLabels)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  darch
}