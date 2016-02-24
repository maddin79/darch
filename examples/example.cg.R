# Example using conjugate gradients
example.cg <- function()
{
  data(iris)
  
  ##
  # Configuration
  ##
  darch <- darch(Species ~ ., iris,
                 rbm.numEpochs = 0,
                 # DArch configuration.
                 # minimal net so solve XOR
                 #layers = c(4,20,3),
                 caret.preProcessParams = list("method" = c("scale", "center")),
                 #darch.batchSize = 3,
                 # higher for sigmoid activation
                 #darch.learnRate = .8,
                 #darch.unitFunction = c(sigmoidUnit, softmaxUnit),
                 darch.fineTuneFunction = minimizeClassifier,
                 #darch.initialMomentum = .5,
                 #darch.finalMomentum = .9,
                 #darch.isClass = T,
                 #darch.bootstrap = T,
                 darch.numEpochs = 20,
                 #cg.length = 3,
                 #cg.switchLayers = 2
  )
  
  e <- darchTest(darch)
  cat(paste0("Incorrect classifications on all examples: ", e[3], " (",
             e[2], "%)\n"))
  
  darch
}