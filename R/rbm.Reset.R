# Copyright (C) 2013-2016 Martin Drees
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

#' Resets the weights and biases of the \code{\link{RBM}} object
#' 
#' This function resets the weights and biases of the \code{\link{RBM}} object.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage resetRBM(rbm)
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname resetRBM-methods
setGeneric(
  name="resetRBM",
  def=function(rbm, ...){standardGeneric("resetRBM")}
)

#' Resets the weights and biases of the \code{\link{RBM}} object 
#' 
#' @rdname resetRBM-methods
#' @aliases resetRBM,RBM-method
setMethod(
  f="resetRBM",
  signature="RBM",
  definition=function(rbm, ...)
  {
    numVisible <- rbm@numVisible
    numHidden <- rbm@numHidden
    
    rbm@weights <- rbm@genWeightFunction(numVisible, numHidden, ...)
    rbm@hiddenBiases <- rbm@genWeightFunction(1, numHidden, ...)
    rbm@visibleBiases <- rbm@genWeightFunction(1, numVisible, ...)
    
    rbm@weightsInc <- matrix(0,numVisible,numHidden)
    rbm@hiddenBiasesInc <- matrix(0,1,numHidden)
    rbm@visibleBiasesInc <- matrix(0,1,numVisible)
    rbm@stats <- list()
    
    rbm
  }
)