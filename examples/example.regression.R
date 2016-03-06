example.regression <- function(...)
{
  library("MASS")
  library(caret)
  data(cats)
  pp <- preProcess(cats, method=c("scale"))
  catsScaled <- predict(pp, newdata = cats)

  darch <- darch(Hwt ~ Bwt,
    cats,
    caret.preProcessParams = list(method = c("center", "scale")),
    caret.preProcessTargets = T,
    normalizeWeights = T,
    layers = c(1,50,100,50,1),
    darch.batchSize =  10,
    bp.learnRate = c(1, 1, 1, .1),
    darch.isClass = F,
    darch.bootstrap = F,
    darch.numEpochs = 100,
    darch.unitFunction = c(tanhUnit, tanhUnit, tanhUnit, softplusUnit),
    ...)

  e <- darchTest(darch, data = catsScaled)

  darch
}
