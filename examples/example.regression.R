example.regression <- function()
{
  library("MASS")
  library(caret)
  data(cats)
  pp <- preProcess(cats, method=c("scale"))
  catsScaled <- predict(pp, newdata = cats)
  
  darch <- darch(Hwt ~ Bwt,
                 catsScaled,
                 normalizeWeights = T,
                 layers = c(1,50,100,1),
                 darch.batchSize =  10,
                 darch.learnRate = .001,
                 darch.isClass = F,
                 darch.bootstrap = F,
                 darch.numEpochs = 100,
                 darch.unitFunction = rectifiedLinearUnit)
  
  e <- testDarch(darch, data=catsScaled)
  
  darch
}