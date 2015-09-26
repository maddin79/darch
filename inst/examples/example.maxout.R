# Copyright (C) 2013-2015 Martin Drees
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
  
  ##
  # Configuration
  ##
  darch <- darch(x = trainDataSmall, y = trainLabelsSmall,
    #testData[], testLabels[],
    # pre-train configuration.
    #normalizeWeights = T,
    rbm.learnRateWeights = .1,
    rbm.learnRateBiasVisible = .1,
    rbm.learnRateBiasHidden = .1,
    rbm.weightCost = .0002,
    rbm.initialMomentum = .5,
    rbm.finalMomentum = .9,
    rbm.momentumSwitch = 5,
    rbm.numEpochs = 15,
    rbm.batchSize = 100,
    rbm.trainOutputLayer = F,
    
    # DArch constructor arguments
    layers = c(784,500,100,10), # required
    darch.batchSize = 100,
    # change to DEBUG if needed
    darch.logLevel = futile.logger::INFO,
    # DArch configuration
    darch.genWeightFunc = generateWeights,
    darch.fineTuneFunction = backpropagation,
    # higher for sigmoid activation
    darch.learnRateWeights = .01,
    darch.learnRateBiases = .01,
    darch.dropoutHidden = .5,
    darch.dropoutInput = .2,
    # layer configuration.
    # activation function
    darch.layerFunctionDefault = sigmoidUnitDerivative,
    # custom activation functions
    darch.layerFunctions = list("1"=maxoutUnitDerivative, "2"=maxoutUnitDerivative),
    darch.layerFunction.maxout.poolSize = 5,
    # fine-tune configuration
    darch.isBin = T,
    darch.isClass = T,
    darch.retainData = F,
    darch.bootstrap = T,
    darch.numEpochs = 50,
  )

  print(darch)
  
  predictions <- predict(darch, testData[], type="bin")
  labels <- cbind(predictions, testLabels[])
  numIncorrect <- sum(apply(labels, 1, function(i) { any(i[1:10] != i[11:20]) }))
  cat(paste0("Incorrect classifications on test data: ", numIncorrect,
             " (", round(numIncorrect/nrow(testLabels[])*100, 2), "%)\n"))
  
  finalizeOutputCapture()
  
  return(darch)
}