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

# For documentation, see R/examples.R or ?example.cg
example.cg <- function()
{
  startOutputCapture("example.cg")
  
  data(iris)
  
  ##
  # Configuration
  ##
  darch <- darch(Species ~ ., iris, scale=T,
                 rbm.numEpochs = 0,
                 # DArch configuration.
                 # minimal net so solve XOR
                 layers = c(4,20,3),
                 darch.batchSize = 3,
                 # higher for sigmoid activation
                 darch.learnRate = .8,
                 darch.layerFunctionDefault = sigmoidUnitDerivative,
                 darch.fineTuneFunction = minimizeClassifier,
                 darch.initialMomentum = .5,
                 # keep momentum the same, not recommended for more complex problems
                 darch.finalMomentum = .9,
                 # binary classification
                 darch.isClass = T,
                 # stop when the network classifies all of the training examples correctly.
                 darch.stopClassErr = 0,
                 darch.stopValidClassErr = 0,
                 darch.numEpochs = 100,
                 # change to DEBUG if needed
                 darch.logLevel = futile.logger::INFO,
                 length = 3,
                 switchLayers = 2
  )
  
  print(darch)
  
  predictions <- predict(darch, newdata=iris, type="class")
  
  numIncorrect <- sum(predictions != iris[,5])
  cat(paste0("Incorrect classifications on all examples: ", numIncorrect, " (",
         round(numIncorrect/nrow(iris)*100, 2), "%)\n"))
  
  finalizeOutputCapture()
  
  return (darch)
}