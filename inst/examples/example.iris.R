# Copyright (C) 2013-2015 darch
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

# For documentation, see R/examples.R or ?example.iris
example.iris <- function()
{
  startOutputCapture("example.iris")
  
  data(iris)
  
  # shuffle the data
  irisShuffled <- iris[sample(nrow(iris)),]
  irisShuffled[,1:4]  <- irisShuffled[,1:4]/10
  
  dataSetValid <- createDataSet(data=irisShuffled[121:150,], formula=Species ~ .)
  
  ##
  # Configuration
  ##
  darch <- darch(Species ~ ., irisShuffled[1:120,],
                 rbm.maxEpoch = 25,
                 
                 # DArch configuration.
                 # minimal net so solve XOR
                 darch.layers = c(4,10,3),
                 darch.batchSize = 10,
                 # higher for sigmoid activation
                 darch.learnRateWeights = 1,
                 darch.learnRateBiases = 1,
                 darch.layerFunctionDefault = sigmoidUnitDerivative,
                 darch.momentum = .9,
                 # keep momentum the same, not recommended for more complex problems
                 darch.finalMomentum = .5,
                 # binary classification
                 darch.isBin = T,
                 darch.isClass = T,
                 # stop when the network classifies all of the training examples correctly.
                 darch.stopClassErr = 99,
                 darch.stopValidClassErr = 100,
                 darch.maxEpoch = 1000,
                 # change to DEBUG if needed
                 darch.logLevel = INFO,
                 additionalDataSets=list("valid"=dataSetValid)
  )
  
  predictions <- predict(darch, irisShuffled[121:150,], type="class")
  
  cat(paste0("Correct classifications on 30 validation examples: ",
         round(sum(predictions == irisShuffled[121:150,5])/30*100, 2), "%"))
}

# short description printed upon sourcing this file
cat(paste("iris example.\n",
          "Classifies the iris data set using a three-layer DBN",
          "(4, 20, and 3 neurons, respectively) with 5 epochs of RBM",
          "pre-training and backpropagation fine-tuning.\n",
          "Available functions: example.iris().\n\n"))