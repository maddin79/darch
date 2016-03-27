# XOR with nominal/ordinal data
example.xor_nominal <- function(...)
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
                 layers = c(2, 10, 2),
                 shuffleTrainData = F,
                 preProc.logicalToNumeric = F, # don't convert targets
                 darch.errorFunction = mseError,
                 darch.stopErr = .1,
                 darch.unitFunction = sigmoidUnit,
                 bp.learnRate = 1,
                 darch.numEpochs = 1000,
                 retainData = T,
                 ...
  )

  e <- darchTest(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))

  predictions <- predict(darch, newdata = dataFrame, type = "class")
  
  print(predictions, max.levels = 0)
  
  darch
}
