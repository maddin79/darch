example.regression <- function()
{
  library("MASS")
  data(cats)
  catsScaled <- scale(cats[,2:3])
  
  darch <- darch(Hwt ~ Bwt,
                 catsScaled,
                 normalizeWeights = T,
                 scale = F,
                 layers = c(1,200,50,1),
                 darch.batchSize =  1,
                 darch.learnRate = .01,
                 darch.isClass = F,
                 darch.bootstrap = F,
                 darch.numEpochs = 100,
                 darch.unitFunction = c(tanSigmoidUnitDerivative, rectifiedLinearUnitDerivative, linearUnitDerivative))
  
  e <- testDarch(darch, data=catsScaled)
  
  darch
}