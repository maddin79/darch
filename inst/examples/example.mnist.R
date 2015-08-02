# Copyright (C) 2013-2015 darch
#
# This file is part of darch.
#
# darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch.  If not, see <http://www.gnu.org/licenses/>.

# For documentation, see R/examples.R or ?example.mnist
example.mnist <- function(dataFolder = "data/")
{
  startOutputCapture("example.mnist")
  
  provideMNIST(dataFolder)
  
  ffload(paste0(dataFolder, "train")) # trainData, trainLabels
  ffload(paste0(dataFolder, "test")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  dataSetTrain <- createDataSet(data=trainDataSmall,
                           targets=trainLabelsSmall)
  dataSetValid <- createDataSet(data=testData[], targets=testLabels[])
  
  darch  <- darch(dataSetTrain,
    # pre-train configuration.
    rbm.maxEpoch = 5,
    
    # DArch constructor arguments
    darch.layers = c(784,100,10), # required
    darch.batchSize = 10,
    # change to DEBUG if needed
    darch.logLevel = futile.logger::INFO,
    # DArch configuration
    darch.fineTuneFunction = backpropagation,
    # higher for sigmoid activation
    darch.learnRateWeights = .1,
    darch.learnRateBiases = .1,
    # layer configuration.
    # activation function
    darch.layerFunctionDefault = sigmoidUnitDerivative,
    # fine-tune configuration
    darch.isBin = T,
    darch.isClass = T,
    darch.maxEpoch = 20,
    additionalDataSets=list("valid"=dataSetValid)
  )
  
  finalizeOutputCapture(list())
  
  return(darch)
}

# short description printed upon sourcing this file
cat(paste("MNIST example.\n",
          "Trains a small DBN on the MNIST problem using 5 epochs of RBM",
          "pre-training and 20 epochs of backpropagation fine-tuning.\n",
          "Available functions: example.mnist(dataFolder=\"data/\").\n\n"))