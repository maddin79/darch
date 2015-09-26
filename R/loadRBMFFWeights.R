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

#' Loads weights and biases for a RBM network from a ffData file.
#' 
#' Loads the weights and the biases for the given RBM object from the filename 
#' given through the parameter \code{name}. See \code{\link{ffload}} for more
#' details
#'  
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param name The name of the file without the ending ".net".
#' @return \code{rbm} - The RBM with the loaded weights and biases.
#' @usage loadRBMFFWeights(rbm,name)
#' 
#' @seealso \code{\link{ffload}}, \code{\link{saveRBM}}, \code{\link{loadRBM}}, \code{\link{saveRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname loadRBMFFWeights
loadRBMFFWeights <- function(rbm,name){
    w <- 1
    v <- 1
    h <- 1
    ffload(paste(name,"-WB",sep=""),overwrite=TRUE)
    rbm@ffWeights <- w
    rbm@ffHiddenBiases <- h
    rbm@ffVisibleBiases <- v
    return(rbm)
  }
