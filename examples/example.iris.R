
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
                 caret.preProcessParams=list("method" = c("scale", "center")),
                 # We'll be using softmax, which is sensitive to very big
                 # weights (causes divisions by 0), hence weight normalization
                 normalizeWeights=T,
                 layers = c(0,20,0),
                 # rpropagation works well with bigger batch sizes
                 darch.batchSize = 30,
                 darch.fineTuneFunction = "rpropagation",
                 # Softmax is effective for classification tasks
                 darch.unitFunction = c("tanhUnit", "softmaxUnit"),
                 # We'll stop when all training examples are correctly
                 # classified
                 darch.stopClassErr = 0,
                 # ... or when training has been going on for 100 epochs.
                 darch.numEpochs = 100,
                 darch.bootstrap = T,
                 rprop.incFact = 1.3,
                 rprop.decFact = .7
  )
  
  # The predict function can be used to get the network output for a new set of
  # data, it will even convert the output back to the original character labels
  predictions <- predict(darch, newdata=iris, type="class")
  
  # And these labels can then easily be compared to the correct ones
  numIncorrect <- sum(predictions != iris[,5])
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
             round(numIncorrect/nrow(iris)*100, 2), "%)\n"))
  
  # For an easier way to test classification performance, see ?testDarch

  darch
}
