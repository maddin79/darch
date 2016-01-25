
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
    # We use 5 epochs of pre-training, not as important for smaller networks
    rbm.numEpochs = 5,
    rbm.consecutive = F,
    rbm.batchSize = 100,
    # Don't train the output layer, backprop does that just fine
    rbm.lastLayer = -1,
    layers = c(784,100,10),
    darch.batchSize = 100,
    darch.learnRate = 2,
    darch.unitFunction = c(tanhUnitDerivative, softmaxUnitDerivative),
    # fine-tune configuration.
    # use this when handling bigger data sets, it will make the resulting DArch
    # instance much smaller
    darch.retainData = F,
    # activate bootstrapping which splits of part of the training data for
    # validation
    darch.bootstrap = T,
    darch.numEpochs = 20
  )
  
  # the predict function can be used to get the network output for a new set of
  # data, it will even convert the output back to the original character labels
  predictions <- predict(darch, newdata=testData, type="class")
  
  # And these labels can then easily be compared to the correct ones
  labels <- cbind(predictions, testLabels[])
  numIncorrect <- sum(apply(labels, 1, function(i) { any(i[1:10] != i[11:20]) }))
  cat(paste0("Incorrect classifications on test data: ", numIncorrect,
             " (", round(numIncorrect/nrow(testLabels[])*100, 2), "%)\n"))
  
  # For an easier way to test classification performance, see ?testDarch
  
  darch
}
