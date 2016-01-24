
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
  # create data frame with column names X1 through X3, which will used in the
  # model formula
  dataFrame <- data.frame(cbind(trainData, trainTargets))

  # see XOR example #1 for explanation of the parameter values
  darch <- darch(X3 ~ ., dataFrame,
                 layers = c(2,10,2),  # when using factors, number of output
                 darch.batchSize = 1, # neurons has to equal number of classes
                 darch.unitFunction =
                   c(sigmoidUnitDerivative, softmaxUnitDerivative),
                 darch.learnRate = 4,
                 darch.stopClassErr = 0,
                 darch.numEpochs = 1000
  )

  e <- testDarch(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))

  predictions <- predict(darch, newdata=dataFrame, type="class")
  
  print(predictions)
  
  darch
}
