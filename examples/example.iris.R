# IRIS example using RPROP fine-tuning and autosaving
example.iris <- function(...)
{
  data(iris)
  
  darch <- darch(Species ~ ., iris,
    preProc.params = list("method" = c("scale", "center")),
    normalizeWeights = T,
    normalizeWeightsBound = 1,
    layers = 20, # one hidden layer with 20 neurons
    darch.batchSize = 30,
    darch.fineTuneFunction = "rpropagation",
    darch.unitFunction = c("tanhUnit", "softmaxUnit"),
    darch.stopValidClassErr = 0,
    darch.stopValidErr = .15,
    bootstrap = T,
    bootstrap.unique = F,
    rprop.incFact = 1.3,
    rprop.decFact = .7,
    rprop.initDelta = .1,
    rprop.maxDelta = 5,
    rprop.method = "iRprop-",
    rprop.minDelta = 1e-5,
    autosave = T,
    autosave.dir = "darch.autosave",
    autosave.epochs = 10,
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
