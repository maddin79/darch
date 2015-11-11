
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
                 rbm.numEpochs = 0,
                 layers = c(4,20,10,3),
                 # batch size equals the number of classes, which is usually a
                 # sensible choice
                 darch.batchSize = 3,
                 # higher for sigmoid activation
                 darch.learnRateWeights = .8,
                 darch.learnRateBiases = .8,
                 # binary classification
                 darch.isBin = T,
                 # We'll stop when either all training examples are correctly
                 # classified or the validation error drops below 1%...
                 darch.stopClassErr = 0,
                 darch.stopValidClassErr = 1,
                 # ... or when training has been going on for 250 epochs.
                 darch.numEpochs = 250,
                 # change to DEBUG if needed
                 darch.logLevel = futile.logger::INFO
  )
  
  print(darch)
  
  # the predict function can be used to get the network output for a new set of
  # data, it will even convert the output back to the original character labels
  predictions <- predict(darch, newdata=iris, type="class")
  
  # And these labels can then easily be compared to the correct ones
  numIncorrect <- sum(predictions != iris[,5])
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
         round(numIncorrect/nrow(iris)*100, 2), "%)\n"))

  return (darch)
}
