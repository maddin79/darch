# IRIS example using the paramsList parameter to specify parameters
example.paramsList <- function(...)
{
  data(iris)
  
  paramsList <- list()
  paramsList$rbm.numEpochs = 2
  paramsList$preProc.params = list("method" = c("scale", "center"))
  paramsList$normalizeWeights = T
  paramsList$normalizeWeightsBound = 1
  paramsList$layers = 20 # one hidden layer with 20 neurons
  paramsList$darch.batchSize = 30
  paramsList$darch.fineTuneFunction = "rpropagation"
  paramsList$darch.unitFunction = c("tanhUnit", "softmaxUnit")
  paramsList$darch.stopValidClassErr = 0
  paramsList$darch.stopValidErr = .15
  paramsList$bootstrap = T
  paramsList$bootstrap.unique = F
  paramsList$rprop.incFact = 1.3
  paramsList$rprop.decFact = .7
  paramsList$rprop.initDelta = .1
  paramsList$rprop.maxDelta = 5
  paramsList$rprop.method = "iRprop-"
  paramsList$rprop.minDelta = 1e-5
  paramsList$autosave = T
  paramsList$autosave.dir = "darch.autosave"
  paramsList$autosave.epochs = 10
  paramsList$autosave.trim = T
  
  darch <- darch(Species ~ ., iris,
    paramsList = paramsList,
    ...
  )
  
  # The predict function can be used to get the network output for a new set of
  # data, it will even convert the output back to the original class labels
  predictions <- predict(darch, newdata = iris, type = "class")
  
  # And these labels can then easily be compared to the correct ones
  numIncorrect <- sum(predictions != iris[,5])
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(iris)*100, 2), "%)\n"))

  darch
}
