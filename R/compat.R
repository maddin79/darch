compatibilityList <- function()
{
  list(
      "scale" = "preProc.params",
      "caret.preProcessParams" = "preProc.params",
      "preProc.logicalToNumeric" = "preProc.fullRank",
      "preProc.logicalToNumeric.targets" = "preProc.fullRank.targets",
      "preProc.orderedToNumeric" = "preProc.factorToNumeric",
      "preProc.orderedToNumeric.targets" = "preProc.factorToNumeric.targets",
      "rbm.trainOutputLayer" = "rbm.lastLayer",
      "rbm.learnRateWeights" = "rbm.learnRate",
      "rbm.learnRateBiasVisible" = "rbm.learnRate",
      "rbm.learnRateBiasHidden" = "rbm.learnRate",
      "rbm.weightCost" = "rbm.weightDecay",
      "rbm.momentumSwitch" = "rbm.momentumRampLength",
      "rbm.visibleUnitFunction" = "rbm.unitFunction",
      "rbm.hiddenUnitFunction" = "rbm.unitFunction",
      "rbm.genWeightFunction" = "generateWeightsFunction",
      "darch.bootstrap" = "bootstrap",
      "darch.genWeightFunc" = "generateWeightsFunction",
      "darch.logLevel" = "logLevel",
      "darch.momentumSwitch" = "darch.momentumRampLength",
      "darch.learnRateWeights" = "bp.learnRate",
      "darch.learnRateBiases" = "bp.learnRate",
      "darch.dropoutHidden" = "darch.dropout",
      "darch.dropoutInput" = "darch.dropout",
      "darch.dropoutOneMaskPerEpoch" = "darch.dropout.oneMaskPerEpoch",
      "darch.layerFunction" = "darch.unitFunction",
      "darch.layerFunctions" = "darch.unitFunction",
      "darch.layerFunctionDefault" = "darch.unitFunction",
      "darch.layerFunction.maxout.poolSize" = "darch.unitFunction.maxout.poolSize",
      "darch.retainData" = "retainData"
  )
}