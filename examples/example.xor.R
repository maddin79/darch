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

# TODO detailed documentation

# to load data or source other files, we need the directory of this script
script.dir <- dirname(sys.frame(1)$ofile)

# set up the environment for the example scripts
source(paste(script.dir,"source_first.R",sep="/"))

# exemplary custom generate weight function
genWeightsExample <- function (numUnits1, numUnits2) 	{
	ret <- matrix(rnorm(numUnits1 * numUnits2),numUnits1, numUnits2)
	return(ret)
}

example.xor <- function()
{
  ##
  # Configuration
  ##
  config <- list(
    # keep low for XOR, explanation above
    rbm.maxEpoch = 5,
    
    # DArch configuration.
    # minimal net so solve XOR
    darch.layers = c(2,3,1),
    darch.batchSize = 1,
    # the default function is generateWeights
    darch.genWeightFunc = genWeightsExample,
    # higher for sigmoid activation
    darch.learnRateWeights = 2,
    darch.learnRateBiases = 2,
    darch.momentum = .5,
    # keep momentum the same
    darch.finalMomentum = .5,
    # binary classification
    darch.isBin = T,
    # stop at MSE error below this, e.g. 0.02
    darch.stopErr = -Inf,
    # stop when the network classifies all of the training examples correctly.
    # set to 101 (default) if you want to use darch.stopErr instead
    darch.stopClassErr = 100,
    darch.maxEpoch = 1000,
    # change to DEBUG if needed
    darch.logLevel = INFO
  )
  
  config <- mergeDefaultDArchConfig(config)
  
  # dataset
  trainData <- matrix(c(0,0,0,1,1,0,1,1),ncol=2,byrow=TRUE)
  trainTargets <- matrix(c(0,1,1,0),nrow=4)
  dataSet <- createDataSet(trainData=trainData, trainTargets=trainTargets)
  
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
  
  # here we just present the classification results for the input data
  darch <- getExecuteFunction(darch)(darch,trainData)
  network_outputs <- getExecOutputs(darch=darch)
  cat("Inputs:\n")
  print(trainData)
  cat("Outputs:\n")
  print(network_outputs[[length(network_outputs)]])
  return (getStats(darch))
}