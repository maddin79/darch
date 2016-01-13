# Example using conjugate gradients
example.cg <- function()
{
  data(iris)
  
  ##
  # Configuration
  ##
  darch <- darch(Species ~ ., iris, scale=T,
                 rbm.numEpochs = 0,
                 # DArch configuration.
                 # minimal net so solve XOR
                 layers = c(4,20,3),
                 darch.batchSize = 3,
                 # higher for sigmoid activation
                 darch.learnRate = .8,
                 darch.unitFunction = sigmoidUnitDerivative,
                 darch.fineTuneFunction = minimizeClassifier,
                 darch.initialMomentum = .5,
                 # keep momentum the same, not recommended for more complex problems
                 darch.finalMomentum = .9,
                 # binary classification
                 darch.isClass = T,
                 # stop when the network classifies all of the training examples correctly.
                 darch.stopClassErr = 0,
                 darch.stopValidClassErr = 0,
                 darch.numEpochs = 20,
                 # change to DEBUG if needed
                 darch.logLevel = futile.logger::INFO,
                 cg.length = 3,
                 cg.switchLayers = 2
  )
  
  e <- testDarch(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  darch
}