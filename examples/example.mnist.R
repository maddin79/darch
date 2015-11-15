
##
# MNIST example #4: Uses a small network that is trained on a small chunk of the
# MNIST data set.
##

# Simply call example.mnist() after executing example("darch") or manually
# sourcing this function
example.mnist <- function(dataFolder = "data/", downloadMNIST = F)
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
  
  # See XOE example #1 for details on the parameter values
  darch  <- darch(trainDataSmall, trainLabelsSmall,
    # We use 10 epochs of pre-training, disable this to see the difference
    rbm.numEpochs = 10,
    rbm.batchSize = 100,
    # Don't train the output layer, backprop does that just fine
    rbm.trainOutputLayer = F,
    layers = c(784,100,10),
    darch.batchSize = 100,
    # Smaller learn rate for faster convergence after pre-training
    darch.learnRateWeights = .01,
    darch.learnRateBiases = .01,
    # fine-tune configuration
    darch.isBin = T,
    # use this when handling bigger data sets, it will make the resulting DArch
    # instance much smaller
    darch.retainData = F,
    darch.numEpochs = 20
  )
  
  print(darch)
  
  predictions <- predict(darch, newdata=testData[], type="class")
  labels <- cbind(predictions, testLabels[])
  numIncorrect <- sum(apply(labels, 1, function(i) { any(i[1:10] != i[11:20]) }))
  cat(paste0("Incorrect classifications on test data: ", numIncorrect,
             " (", round(numIncorrect/nrow(testLabels[])*100, 2), "%)\n"))

  return(darch)
}
