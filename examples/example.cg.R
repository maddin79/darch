# Example using conjugate gradients (minimizeClassifier)
example.cg <- function(...)
{
  data(iris)

  darch <- darch(Species ~ ., iris,
    layers = c(0,20,0),
    preProc.params = list("method" = c("scale", "center")),
    darch.batchSize = 6,
    darch.fineTuneFunction = minimizeClassifier,
    darch.unitFunction = c(linearUnit, softmaxUnit),
    darch.weightDecay = .001,
    darch.numEpochs = 20,
    cg.length = 3,
    cg.switchLayers = 2,
    generateWeightsFunction = generateWeightsHeNormal,
    retainData = T,
    ...
  )
  
  # Since retainData is TRUE, when no new data is passed, predict() and
  # darchTest() will use the data stored in the DArch instance
  e <- darchTest(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  darch
}