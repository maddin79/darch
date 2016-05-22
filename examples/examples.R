\dontrun{
data(iris)
model <- darch(Species ~ ., iris)
print(model)
predictions <- predict(model, newdata = iris, type = "class")
cat(paste("Incorrect classifications:", sum(predictions != iris[,5])))

trainData <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = TRUE)
trainTargets <- matrix(c(0,1,1,0), nrow = 4)
model2 <- darch(trainData, trainTargets, layers = c(2, 10, 1),
  darch.numEpochs = 500, darch.stopClassErr = 0, retainData = T)
e <- darchTest(model2)
cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
  e[2], "%)\n"))

plot(model2)
}

#
# More examples can be found at
# https://github.com/maddin79/darch/tree/v0.12.0/examples
