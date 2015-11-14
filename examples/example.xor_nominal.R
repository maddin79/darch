
##
# XOR example #2, with nominal data, to demonstrate that darch can deal with
# nominal data and to show how model formulae can be used.
##

# Simply call example.xorNominal() after executing example("darch") or manually
# sourcing this function
example.xorNominal <- function()
{
  # dataset
  trainData <- matrix(c("zero","zero","zero","one","one","zero","one","one"),
                      ncol=2, byrow=TRUE)
  trainTargets <- matrix(c("zero", "one", "one", "zero"), nrow=4)
  # create data frame with column names V1 through V3, which will used in the
  # model formula
  dataFrame <- cbind(trainData, trainTargets)

  # see XOR example #1 for explanation of the parameter values
  darch <- darch(V3 ~ V1 + V2, dataFrame,
                 rbm.numEpochs = 0,
                 layers = c(2,10,1),
                 darch.batchSize = 1,
                 darch.bootstrap = F,
                 darch.learnRateWeights = 1,
                 darch.learnRateBiases = 1,
                 darch.isBin = T,
                 darch.isClass = T,
                 darch.stopClassErr = 0,
                 darch.numEpochs = 1000
  )

  # Print parameters, stats, and results
  print(darch)

  predictions <- predict(darch, type="class")
  numCorrect <- sum(predictions == trainTargets)
  cat(paste0("Correct classifications on all data: ", numCorrect,
             " (", round(numCorrect/nrow(trainTargets)*100, 2), "%)\n"))

  return(darch)
}
