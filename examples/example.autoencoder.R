example.autoencoder <- function(...)
{
  data(iris)
  darch <- darch(iris[,1:4], iris[,1:4], c(4,100,2,100,4), darch.isClass = F,
    preProc.params = list(method = c("center", "scale")), preProc.targets = T,
    darch.numEpochs = 200, darch.batchSize = 3,
    darch.unitFunction = softplusUnit, bp.learnRate = .1,
    darch.fineTuneFunction = "backpropagation")
  
  predict(darch, newdata = iris[,1:4])
}