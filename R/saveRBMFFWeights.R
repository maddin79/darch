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

#' Saves weights and biases of a RBM network into a ffData file.
#' 
#' Saves the weights and the biases for the given RBM object to the filename 
#' given through the parameter \code{name}.
#' 
#' @details The weights and biases are saved in one file with the name given 
#' through the parameter \code{name} and the string "-WB". See \code{\link{ffsave}}
#' for more details.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param name The name for the file.
#' 
#' @usage saveRBMFFWeights(rbm,name="saveName")
#' 
#' @seealso \code{\link{ffsave}}, \code{\link{loadRBM}}, \code{\link{saveRBM}}, \code{\link{loadRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname saveRBMFFWeights-methods
setGeneric(
  name="saveRBMFFWeights",
  def=function(rbm,name="saveName"){standardGeneric("saveRBMFFWeights")}
)

#' @rdname saveRBMFFWeights-methods
#' @aliases saveRBMFFWeights,RBM-method
setMethod(
  f="saveRBMFFWeights",
  signature="RBM",
  definition=function(rbm,name="saveName"){
    w <- rbm@ffWeights
    h <- rbm@ffHiddenBiases
    v <- rbm@ffVisibleBiases
    file <- paste(name,"-WB",sep="")
    ffsave(w,h,v,file=file)
  }
)
