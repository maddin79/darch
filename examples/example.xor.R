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

# to load data or source other files, we need the directory of this script
script.dir <- dirname(sys.frame(1)$ofile)

# set up the environment for the example scripts
source(paste(script.dir,"source_first.R",sep="/"))

# exemplary custom generate weight function
genWeightsExample <- function (numUnits1, numUnits2) 	{
	ret <- matrix(rnorm(numUnits1 * numUnits2),numUnits1, numUnits2)
	return(ret)
}

#' An example using the simplest problem not solvable by a standard perceptron:
#' XOR.
#' 
#' The DBN uses three layers with 2, 3, and 1 neurons. Pre-training is
#' generally not as important for such small/simple problems, and it can even
#' be counterproductive and delay fine-tuning convergence.
#' 
#' The learning rate is chosen to be relatively high to achieve faster
#' convergence. Since a sigmoid activation function is used, higher learning
#' rates are less problematic than, e.g., for linear activations. Further
#' increasing the learning rate may lead to quicker convergence, but this
#' effect reverses at higher values and then leads to delayed convergence or no
#' convergence at all for more complex problems (try values higher than 10, for
#' example). The same is true for the momentum, which is kept at 90% here to
#' further increase convergence speed (change finalMomentum to .5 to see the
#' difference), something that is not recommended for more complex problems.
#' 
#' Learning is stopped as soon as 100% of input samples are correctly
#' classified.
#' 
#' See the github wiki for more general information on these examples.
example.xor <- function()
{
  #startOutputCapture("example.xor")
  
  # dataset
  trainData <- matrix(c(0,0,0,1,1,0,1,1),ncol=2,byrow=TRUE)
  trainTargets <- matrix(c(0,1,1,0),nrow=4)
  dataFrame <- cbind(trainData, trainTargets)
  
  #dataSet <- createDataSet(trainData=trainData, trainTargets=trainTargets)
  
  ##
  # Configuration
  ##
  darch <- darch(V3 ~ V1 + V2, dataFrame,
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
    darch.momentum = .9,
    # keep momentum the same, not recommended for more complex problems
    darch.finalMomentum = .9,
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
  
  # here we just present the classification results for the input data
  darch <- getExecuteFunction(darch)(darch,trainData)
  network_outputs <- getExecOutputs(darch=darch)
  cat("Inputs:\n")
  print(trainData)
  cat("Outputs:\n")
  print(network_outputs[[length(network_outputs)]])
  
  #finalizeOutputCapture(list(stats=getStats(darch)))
  
  return(darch)
}

# short description printed upon sourcing this file
cat(paste("XOR example.\n",
          "Solves the XOR problem using a minimal three layer DBN",
          "(2, 3, and 1 neurons, respectively) with 5 epochs of RBM",
          "pre-training and backpropagation fine-tuning.\n",
          "Available functions: example.xor().\n"))