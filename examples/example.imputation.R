# PIMA2 example using caret imputation, requires packages MASS and RANN
example.imputation <- function(...)
{
  library(MASS)
  data("PimaIndiansDiabetes2")
  
  darch <- darch(diabetes ~ ., PimaIndiansDiabetes2,
    preProc.params = list("method" = c("knnImpute")),
    layers = c(0,100,50,0),
    darch.batchSize = 1,
    darch.fineTuneFunction = "backpropagation",
    darch.unitFunction = c("tanhUnit", "tanhUnit", "softmaxUnit"),
    bootstrap = T,
    bootstrap.unique = F,
    ...
  )
  
  darchTest(darch, newdata = PimaIndiansDiabetes2)
  
  darch
}
