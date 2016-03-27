# XOR example with logical targets and a custom weight generation function

example.xor <- function(...)
{
  genWeightsExample <- function (numUnits1, numUnits2, ...)
  {
    generateWeightsGlorotUniform(numUnits1, numUnits2, weights.mean=.1, ...)
  }
  
  # dataset
  trainData <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = TRUE)
  trainTargets <- matrix(c(FALSE,TRUE,TRUE,FALSE), nrow = 4)
  
  darch <- darch(trainData, trainTargets,
    # Note how you can pass deparsed values here, these will be parsed
    layers = "c(0,10,0)",
    generateWeightsFunction = # multiple weight generation functions
      c(genWeightsExample, generateWeightsGlorotNormal),
    bp.learnRate = c(2,3),
    darch.nesterovMomentum = F,
    darch.errorFunction = rmseError,
    # Stop when the network classifies all of the training examples correctly
    darch.stopClassErr = 0,
    darch.returnBestModel = F, # returns the last model and not the best
    darch.numEpochs = 1000,
    retainData = T,
    ...
  )
  
  # darchTest() can be used to obtain information about the classification
  # performance
  e <- darchTest(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  # the predict function can be used to get the network output for a new set of
  # data, it will even convert the output back to the original character labels
  predictions <- predict(darch, type = "bin")
  
  print(predictions)
  
  darch
}
