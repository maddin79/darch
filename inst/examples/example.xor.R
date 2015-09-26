# Copyright (C) 2013-2015 Martin Drees
#
# This file is part of darch.
#
# Darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch.  If not, see <http://www.gnu.org/licenses/>.

# exemplary custom generate weight function
genWeightsExample <- function (numUnits1, numUnits2)
{
  ret <- matrix(rnorm(numUnits1 * numUnits2),numUnits1, numUnits2)
  return(ret)
}

# For documentation, see R/examples.R or ?example.xor
example.xor <- function()
{
  startOutputCapture("example.xor")
  
  # dataset
  trainData <- matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = TRUE)
  trainTargets <- matrix(c(0,1,1,0), nrow = 4)
  
  #dataSet <- createDataSet(trainData=trainData, trainTargets=trainTargets)
  
  ##
  # Configuration
  ##
  darch <- darch(trainData, trainTargets,
    rbm.numEpochs = 5,
    rbm.batchSize = 1,
    rbm.trainOutputLayer = F,
    
    # DArch configuration.
    # minimal net so solve XOR
    layers = c(2,2,1),
    darch.fineTuneFunction = backpropagation,
    darch.layerFunctionDefault = sigmoidUnitDerivative,
    darch.batchSize = 1,
    darch.bootstrap = F,
    # the default function is generateWeights
    darch.genWeightFunc = genWeightsExample,
    # higher for sigmoid activation
    darch.learnRateWeights = 1,
    darch.learnRateBiases = 1,
    darch.initialMomentum = .9,
    # keep momentum the same, not recommended for more complex problems
    darch.finalMomentum = .9,
    # binary classification
    darch.isBin = T,
    # stop at MSE error below this, e.g. 0.02
    darch.stopErr = -Inf,
    # stop when the network classifies all of the training examples correctly.
    # set to 101 (default) if you want to use darch.stopErr instead
    darch.stopClassErr = 0,
    darch.numEpochs = 1000,
    gputools = F
  )
  
  print(darch)
  
  predictions <- predict(darch, type="bin")
  numCorrect <- sum(predictions == trainTargets)
  cat(paste0("Correct classifications on all data: ", numCorrect,
             " (", round(numCorrect/nrow(trainTargets)*100, 2), "%)\n"))
  
  finalizeOutputCapture()
  
  return(darch)
}