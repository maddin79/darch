# Copyright (C) 2015 darch2
# based on code by Martin Drees, copyright (C) 2013 Darch
#
# This file is part of darch2.
#
# Darch2 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch2.  If not, see <http://www.gnu.org/licenses/>.

#' Another MNIST example, this time using both dropout and maxout.
#' 
#' No pre-training is used. Since maxout requires linear activations, all
#' activations have been changed accordingly. You will notice an overall drop
#' in convergence speed compared to the basic MNIST example, but also a smaller
#' difference between the accuracies on the training and validation set.
#' 
#' We use a bigger second layer (400 neurons) and a maxout pool size of 4,
#' which will effectively result in 100 outputs for this layer, the same as in
#' the basic MNIST example.
#' 
#' A higher number of fine-tuning epochs and a bigger DBN is necessary to
#' observe the true potential of dropout and maxout.
example.maxout <- function()
{
  startOutputCapture("example.maxout")
  
  ffload("data/train") # trainData, trainLabels
  ffload("data/test") # testData, testLabels
  
  # only take 10000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  dataSetTrain <- createDataSet(data=trainDataSmall, targets=trainLabelsSmall)
  dataSetValid <- createDataSet(data=testData[], targets=testLabels[])
  
  ##
  # Configuration
  ##
  darch <- darch(dataSetTrain,
    # RBM configuration
    rbm.learnRateWeights = .1,
    rbm.learnRateBiasVisible = .1,
    rbm.learnRateBiasHidden = .1,
    rbm.weightCost = .0002,
    rbm.momentum = .9,
    rbm.finalMomentum = .5,
    rbm.momentumSwitch = 5,
    rbm.visibleUnitFunction = sigmUnitFunc,
    rbm.hiddenUnitFunction = sigmUnitFuncSwitch,
    rbm.updateFunction = rbmUpdate,
    rbm.errorFunction = mseError,
    rbm.genWeightFunction = generateWeights,
    # pre-train configuration.
    # higher values make everything much slower
    rbm.numCD = 1,
    rbm.maxEpoch = 0,
    
    # DArch constructor arguments
    darch.layers = c(784,400,10), # required
    darch.batchSize = 10,
    darch.genWeightFunc = generateWeights,
    # change to DEBUG if needed
    darch.logLevel = INFO,
    # DArch configuration
    darch.fineTuneFunction = backpropagation,
    darch.momentum = .9,
    darch.finalMomentum = .5,
    darch.momentumSwitch = 5,
    # higher for sigmoid activation
    darch.learnRateWeights = .001,
    darch.learnRateBiases = .001,
    darch.errorFunction = mseError,
    darch.dropoutInput = 0.,
    darch.dropoutHidden = .5,
    # layer configuration.
    # activation function
    darch.layerFunctionDefault = linearUnitDerivative,
    # custom activation functions
    darch.layerFunctions = list("1"=maxoutUnitDerivative),
    darch.layerFunction.maxout.poolSize = 4,
    # fine-tune configuration
    darch.isBin = T,
    darch.isClass = T,
    darch.stopErr = -Inf,
    darch.stopClassErr = 101,
    darch.stopValidErr = -Inf,
    darch.stopValidClassErr = 101,
    darch.maxEpoch = 20,
    additionalDataSets=list("valid"=dataSetValid)
  )
  
  finalizeOutputCapture(list(stats=getStats(darch)))
  
  return(darch)
}

# short description printed upon sourcing this file
cat(paste("Maxout example.\n",
          "Trains a small DBN on the MNIST problem using dropout and maxout",
          "for backpropagation fine-tuning (20 epochs) and no pre-training.\n",
      "Expects uncompressed MNIST data to be available in the data/ folder. \n",
          "Available functions: example.maxout().\n\n"))