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

#' Saves a RBM network
#' 
#' Saves the RBM object to the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details If the field \code{ff} of the RBM object is \code{TRUE} then
#' the weights are saved in separate ff-files through the function
#' \code{\link{saveRBMFFWeights}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param name The name for the file. Default value is "rbm".
#' 
#' @usage saveRBM(rbm,name="rbm")
#' 
#' @seealso \code{\link{loadRBM}}, \code{\link{saveRBMFFWeights}} \code{\link{loadRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname saveRBM-methods
setGeneric(
  name="saveRBM",
  def=function(rbm,name="rbm"){standardGeneric("saveRBM")}
)

#' @rdname saveRBM-methods
#' @aliases saveRBM,RBM-method
setMethod(
  f="saveRBM",
  signature="RBM",
  definition=function(rbm,name="rbm"){
    if (getFF(rbm)){
      saveRBMFFWeights(rbm,name)		
    }
    save(rbm,file=paste(name,".net",sep=""))	
  }
)
