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
  
  ff::ffload(paste0(dataFolder, "train")) # trainData, trainLabels
  ff::ffload(paste0(dataFolder, "test")) # testData, testLabels
  
  # only take 1000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  darch  <- darch(trainDataSmall, trainLabelsSmall,
    # pre-train configuration.
    rbm.numEpochs = 5,
    rbm.learnRateWeights = .1,
    rbm.learnRateBiasVisible = .1,
    rbm.learnRateBiasHidden = .1,
    rbm.weightCost = .002,
    
    # DArch constructor arguments
    layers = c(784,100,10), # required
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
    darch.numEpochs = 20
  )
  
  print(darch)
  
  predictions <- predict(darch, testData[], type="bin")
  numIncorrect <- sum(sapply(1:nrow(testLabels[]), function(i) { any(predictions[i,] != testLabels[i,]) }))
  cat(paste0("Incorrect classifications on validation data: ", numIncorrect,
             " (", round(numIncorrect/nrow(testLabels[])*100, 2), "%)\n"))
  
  finalizeOutputCapture()
  
  return(darch)
}

# short description printed upon sourcing this file
cat(paste("MNIST example.\n",
          "Trains a small DBN on the MNIST problem using 5 epochs of RBM",
          "pre-training and 20 epochs of backpropagation fine-tuning.\n",
          "Available functions: example.mnist(dataFolder=\"data/\").\n\n"))