
##
# XOR example #2, with nominal target data, to demonstrate that darch can deal
# with nominal data and to show how model formulae can be used.
##

# Simply call example.xorNominal() after executing example("darch") or manually
# sourcing this function
example.xorNominal <- function(...)
{
  # dataset
  trainData <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = TRUE)
  trainTargets <- matrix(c("zero", "one", "one", "zero"), nrow=4)
  # create data frame with column names X1, X2, and trainTargets, which will
  # used in the model formula
  dataFrame <- data.frame(trainData, trainTargets)
  # Convert input data to ordered factors
  dataFrame[, c("X1", "X2")] <- lapply(dataFrame[, c("X1", "X2")],
    FUN = function(x) { as.ordered(x)})

  # see XOR example #1 for explanation of the parameter values
  darch <- darch(trainTargets ~ ., dataFrame,
                 layers = c(0, 10, 0),  # when using factors, number of output
                 darch.batchSize = 1, # neurons has to equal number of classes
                 darch.unitFunction = sigmoidUnit,
                 bp.learnRate = 1,
                 darch.stopClassErr = 0,
                 darch.numEpochs = 1000,
                 ...
  )

  e <- darchTest(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))

  predictions <- predict(darch, newdata=dataFrame, type="class")
  
  print(predictions)
  
  darch
}
