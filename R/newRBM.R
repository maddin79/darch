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
#
#
#' Constructor function for RBM object.
#' 
#' TODO: Doc ...
#' 
#' @param numVisible Number of visible units.
#' @param numHidden Number of hidden units.
#' @param batchSize Size of the batches
#' @param ff Indicates whether the \code{\link[ff]{ff}} package is used for the
#'        weights, biases and outputs
#' @param logLevel The logging level. 
#'        See \code{\link{setLogLevel}} for details.
#' @param genWeightFunc The function for generating the weight matrices
#' 
#' @return The new RBM object
#' @include rbm.R
#' @include rbm.Setter.R
#' @include rbm.Reset.R
#' @export
newRBM <- function(numVisible,numHidden,batchSize,ff=FALSE, logLevel=INFO,genWeightFunc=generateWeights){
  rbm <- new("RBM")
  setFF(rbm) <- ff
  setBatchSize(rbm) <- batchSize
  setNumHidden(rbm) <- numHidden
  setNumVisible(rbm) <- numVisible
  setGenWeightFunction(rbm) <- genWeightFunc
  flog.threshold(logLevel)
  flog.info(paste("Construct new RBM instance with ",numVisible, " visible and ", numHidden," hidden units.",sep=""))
  rbm <- resetRBM(rbm)
  
  return(rbm)
}