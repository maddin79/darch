
##
# Example #1: Minimal XOR example with a custom weight generation function.
##

# Exemplary custom generate weight function, note the '...' parameter!
genWeightsExample <- function (numUnits1, numUnits2, ...)
{
  generateWeightsUniform(numUnits1, numUnits2, weights.min=-.1, weights.max=.1, ...)
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
    # Small net so solve XOR, input and output layer will be set automatically
    layers = c(0,10,0),
    darch.batchSize = 1,
    # The default function is generateWeights, we use the custom function above
    darch.genWeightFunc = genWeightsExample,
    darch.unitFunction = sigmoidUnit,
    darch.fineTuneFunction = backpropagation,
    darch.nesterovMomentum = F,
    # The default is 1, for this simple problem we can go a little higher
    darch.learnRate = 2,
    # Stop when the network classifies all of the training examples correctly
    darch.stopClassErr = 0,
    # Train for a maximum of 1000 epochs
    darch.numEpochs = 1000
  )
  
  # testDarch() can be used to obtain information about the classification
  # performance
  e <- testDarch(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  # the predict function can be used to get the network output for a new set of
  # data, it will even convert the output back to the original character labels
  predictions <- predict(darch, type="bin")
  
  print(predictions)
  
  darch
}
