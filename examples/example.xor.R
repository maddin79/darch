
##
# Example #1: Minimal XOR example with a custom weight generation function.
##

# Exemplary custom generate weight function, note the '...' parameter!
genWeightsExample <- function (numUnits1, numUnits2, ...)
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
    darch.unitFunction = sigmoidUnitDerivative,
    # The defaults are 0.8, for this simple problem we can go a little higher
    darch.learnRate = 4,
    # stop when the network classifies all of the training examples correctly.
    darch.stopClassErr = 0,
    # the problem is usually solved within much less than 1000 epochs
    darch.numEpochs = 1000
  )
  
  e <- testDarch(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  darch
}
