example.regression <- function(...)
{
  library("MASS")
  library(caret)
  data(cats)
  pp <- preProcess(cats, method=c("scale"))
  catsScaled <- predict(pp, newdata = cats)

  darch <- darch(Hwt ~ Bwt,
    cats,
    preProc.params = list(method = c("center", "scale")),
    preProc.targets = T,
    layers = c(1,20,50,20,1),
    darch.batchSize =  10,
    bp.learnRate = .01,
    darch.isClass = F,
    darch.numEpochs = 100,
    darch.unitFunction = linearUnit,
    ...)

  print(darchTest(darch, newdata = cats))

  darch
}
