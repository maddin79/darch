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
#' @include rbm.Setter.R
#' 
#' @export
#' @docType methods
#' @rdname resetRBM-methods
setGeneric(
  name="resetRBM",
  def=function(rbm){standardGeneric("resetRBM")}
)

#' @rdname resetRBM-methods
#' @aliases resetRBM,RBM-method
setMethod(
  f="resetRBM",
  signature="RBM",
  definition=function(rbm){
    numVisible <- getNumVisible(rbm)
    numHidden <- getNumHidden(rbm)
    
    setWeights(rbm) <- getGenWeightFunction(rbm)(numVisible,numHidden) 
    setHiddenBiases(rbm) <- matrix(0,1,numHidden)
    setVisibleBiases(rbm) <- matrix(0,1,numVisible)
    
    setWeightInc(rbm) <- matrix(0,numVisible,numHidden)
    setHiddenBiasesInc(rbm) <- matrix(0,1,numHidden)
    setVisibleBiasesInc(rbm) <- matrix(0,1,numVisible)
    rbm@stats <- list()
    
    return(rbm)
  }
)