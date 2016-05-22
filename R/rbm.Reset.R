# Copyright (C) 2013-2016 Martin Drees
# Copyright (C) 2015-2016 Johannes Rueckert
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

#' Resets the weights and biases of the \code{RBM} object
#' 
#' This function resets the weights and biases of the \code{\linkS4class{RBM}}
#' object.
#' 
#' @param rbm An instance of class \code{\linkS4class{RBM}}.
#' @param ... Additional arguments.
#' 
#' @seealso \code{\linkS4class{RBM}}
#' @keywords internal
#' @include rbm.Class.R
setGeneric(
  name = "resetRBM",
  def = function(rbm, ...){standardGeneric("resetRBM")}
)

setMethod(
  f = "resetRBM",
  signature = "RBM",
  definition = function(rbm, ...)
  {
    numVisible <- getParameter(".numVisible", net = rbm)
    numHidden <- getParameter(".numHidden", net = rbm)
    genWeightFunction <- getParameter(".generateWeightsFunction", net = rbm)
    
    rbm@weights <- genWeightFunction(numVisible, numHidden, ..., net = rbm)
    rbm@hiddenBiases <- genWeightFunction(1, numHidden, ..., net = rbm)
    rbm@visibleBiases <- genWeightFunction(1, numVisible, ..., net = rbm)
    
    rbm@weightsInc <- matrix(0, numVisible, numHidden)
    rbm@hiddenBiasesInc <- matrix(0, 1, numHidden)
    rbm@visibleBiasesInc <- matrix(0, 1, numVisible)
    rbm@stats <- list()
    
    rbm
  }
)