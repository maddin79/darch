
##
# Example #1: Minimal XOR example with a custom weight generation function.
##

# Exemplary custom generate weight function
genWeightsExample <- function (numUnits1, numUnits2)
{
  generateWeightsRunif(numUnits1, numUnits2, weights.min=-.1, weights.max=.1)
}

# Simply call example.xor() after executing example("darch") or manually
# sourcing this function
example.xor <- function()
{
  # dataset
  trainData <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = TRUE)
  trainTargets <- matrix(c(0,1,1,0), nrow = 4)
  
  darch <- darch(trainData, trainTargets,
    # We don't need pre-training for this problem
    rbm.numEpochs = 0,
    # Small net so solve XOR
    layers = c(2,10,1),
    darch.batchSize = 1,
    # Bootstrapping would create a training and validation set from the training
    # data, we don't want that here
    darch.bootstrap = F,
    # The default function is generateWeights, we use the custom function above
    darch.genWeightFunc = genWeightsExample,
    # The defaults are 0.8, for this simple problem we can go a little higher
    darch.learnRate = 5,
    # stop when the network classifies all of the training examples correctly.
    darch.stopClassErr = 0,
    # the problem is usually solved within much less than 1000 epochs
    darch.numEpochs = 1000
  )
  
  # prints all parameters and stats
  print(darch)
  
  # check the performance of our network
  predictions <- predict(darch, type="bin")
  numCorrect <- sum(predictions == trainTargets)
  cat(paste0("Correct classifications on all data: ", numCorrect,
             " (", round(numCorrect/nrow(trainTargets)*100, 2), "%)\n"))
  
  return(darch)
}
