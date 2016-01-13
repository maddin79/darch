##
# IRIS example #3, in which a small network is trained on the IRIS data set
# which is part of every R installation.
##

# Simply call example.iris() after executing example("darch") or manually
# sourcing this function
example.iris <- function()
{
  data(iris)
  
  # See XOR example #1 for more details on the parameter values.
  darch <- darch(Species ~ ., iris,
                 # We'll scale all data, useful for faster convergence when data
                 # is not already relatively close to 0 (or, say, within -1..1)
                 scale=T,
                 normalizeWeights=T,
                 rbm.numEpochs = 0,
                 layers = c(4,20,3),
                 # rpropagation works well with bigger batch sizes
                 darch.batchSize = 30,
                 darch.fineTuneFunction = rpropagation,
                 # higher for sigmoid activation
                 # We'll stop when either all training examples are correctly
                 # classified or the validation error drops below 1%...
                 darch.stopClassErr = 0,
                 darch.stopValidClassErr = 1,
                 # ... or when training has been going on for 250 epochs.
                 darch.numEpochs = 1000,
                 rprop.incFact = 1.4,
                 rprop.decFact = .7
  )
  
  # Test network performance
  e <- testDarch(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))

  darch
}
