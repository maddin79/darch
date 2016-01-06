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
# along with darch. If not, see <http://www.gnu.org/licenses/>.

#' Constructor function for RBM object.
#' 
#' TODO: Doc ...
#' 
#' @param numVisible Number of visible units.
#' @param numHidden Number of hidden units.
#' @param batchSize Size of the batches
#' @param logLevel The logging level. 
#'        See \code{\link{setLogLevel}} for details.
#' @param genWeightFunc The function for generating the weight matrices
#' 
#' @return The new RBM object
#' @include rbm.R
#' @include rbm.Setter.R
#' @include rbm.Reset.R
#' @export
newRBM <- function(numVisible, numHidden, batchSize, logLevel=INFO,
                   genWeightFunc=generateWeightsRunif)
{
  rbm <- new("RBM")
  setBatchSize(rbm) <- batchSize
  setNumHidden(rbm) <- numHidden
  setNumVisible(rbm) <- numVisible
  setGenWeightFunction(rbm) <- genWeightFunc
  flog.threshold(logLevel)
  flog.info(paste("Construct new RBM instance with ",numVisible, " visible and ", numHidden," hidden units.",sep=""))
  rbm <- resetRBM(rbm)
  
  return(rbm)
}