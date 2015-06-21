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

# detailed documentation
# to load data or source other files, we need the directory of this script
script.dir <- dirname(sys.frame(1)$ofile)

# set up the environment for the example scripts
source(paste(script.dir,"source_first.R",sep="/"))

example.mnist <- function()
{
  ##
  # Configuration
  ##
  config <- list(
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
    rbm.maxEpoch = 5,
    
    # DArch constructor arguments
    darch.layers = c(784,100,10), # required
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
    darch.learnRateWeights = .1,
    darch.learnRateBiases = .1,
    darch.errorFunction = mseError,
    darch.dropoutInput = 0.,
    darch.dropoutHidden = 0.,
    # layer configuration.
    # activation function
    darch.layerFunctionDefault = sigmoidUnitDerivative,
    # custom activation functions
    darch.layerFunctions = list(),
    # fine-tune configuration
    darch.isBin = T,
    darch.isClass = T,
    darch.stopErr = -Inf,
    darch.stopClassErr = 101,
    darch.stopValidErr = -Inf,
    darch.stopValidClassErr = 101,
    darch.maxEpoch = 20
  )
  
  config <- mergeDefaultDArchConfig(config)
  
  ffload(paste(script.dir, "../data/train", sep="/")) # trainData, trainLabels
  ffload(paste(script.dir, "../data/test", sep="/")) # testData, testLabels
  
  # only take 10000 samples, otherwise training takes increasingly long
  chosenRowsTrain <- sample(1:nrow(trainData), size=1000)
  trainDataSmall <- trainData[chosenRowsTrain,]
  trainLabelsSmall <- trainLabels[chosenRowsTrain,]
  
  dataSet <- createDataSet(trainData=trainDataSmall,
                           trainTargets=trainLabelsSmall,
                           validData=testData[],
                           validTargets=testLabels[])
  
  darch <- createDArchFromConfig(config)
  
  
  if (config[["rbm.maxEpoch"]] > 0)
  {
    preTrainDArch(darch, dataSet, maxEpoch=config[["rbm.maxEpoch"]],
                  numCD=config[["rbm.numCD"]])
  }
  
  darch <- fineTuneDArch(darch,dataSet,
                         maxEpoch=config[["darch.maxEpoch"]],
                         isBin=config[["darch.isBin"]],
                         isClass=config[["darch.isClass"]],
                         stopErr=config[["darch.stopErr"]],
                         stopClassErr=config[["darch.stopClassErr"]],
                         stopValidErr=config[["darch.stopValidErr"]],
                         stopValidClassErr=config[["darch.stopValidClassErr"]]
  )
  
  return(getStats(darch))
}