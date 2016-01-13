
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
                 darch.unitFunction = sigmoidUnitDerivative,
                 darch.bootstrap = F,
                 darch.learnRate = 5,
                 darch.stopClassErr = 0,
                 darch.numEpochs = 1000
  )

  e <- testDarch(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  darch
}
