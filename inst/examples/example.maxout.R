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

# For documentation, see R/examples.R or ?example.maxout
example.maxout <- function(dataFolder="data/")
{
  startOutputCapture("example.maxout")
  
  provideMNIST(dataFolder)
  
  ff::ffload(paste0(dataFolder, "train")) # trainData, trainLabels
  ff::ffload(paste0(dataFolder, "test")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  dataSetTrain <- createDataSet(data=trainDataSmall,
                                targets=trainLabelsSmall)
  dataSetValid <- createDataSet(data=testData[], targets=testLabels[])
  
  ##
  # Configuration
  ##
  darch <- darch(dataSetTrain,
    # pre-train configuration.
    rbm.maxEpoch = 5,
    
    # DArch constructor arguments
    darch.layers = c(784,400,10), # required
    darch.batchSize = 10,
    # change to DEBUG if needed
    darch.logLevel = futile.logger::INFO,
    # DArch configuration
    darch.fineTuneFunction = backpropagation,
    # higher for sigmoid activation
    darch.learnRateWeights = .001,
    darch.learnRateBiases = .001,
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
    darch.maxEpoch = 20,
    additionalDataSets=list("valid"=dataSetValid)
  )
  
  finalizeOutputCapture(list(stats=getStats(darch)))
  
  return(darch)
}

# short description printed upon sourcing this file
cat(paste("Maxout example.\n",
          "Trains a small DBN on the MNIST problem using dropout and maxout",
          "for backpropagation fine-tuning (20 epochs) and 5 epochs of",
          "pre-training.\n",
          "Available functions: example.maxout(dataFolder).\n\n"))