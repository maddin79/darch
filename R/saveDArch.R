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

#' Saves a DArch network
#' 
#' Saves the DArch object to the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details If the field \code{ff} of the DArch object is \code{TRUE} then
#' the weights are saved in separate ff-files named by the parameter \code{name}
#' plus the string "-W" and the number of the layer.
#' In the same way the weights from the RBMs of the DArch are saved, but only
#' if the parameter \code{saveRBM} is \code{TRUE}. For more information about 
#' the how the weights and biases from the RBMs are saved see 
#' \code{\link{saveRBMFFWeights}}.
#' If the parameter \code{saveRBM} is \code{FALSE} the field \code{rbmList} of 
#' the DArch object is overwritten by an empty list.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param name The name for the file. Default value is "darch".
#' @param saveRBM Boolean value to indicate if the RBM is saved.
#' 
#' @usage saveDArch(darch,name="darch",saveRBM=TRUE)
#' 
#' @seealso \code{\link{loadDArch}}, \code{\link{saveRBMFFWeights}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname saveDArch-methods
setGeneric(
  name="saveDArch",
  def=function(darch,name="darch",saveRBM=TRUE){standardGeneric("saveDArch")}
)

#' @rdname saveDArch-methods
#' @aliases saveDArch,DArch-method
setMethod(
  f="saveDArch",
  signature="DArch",
  definition=function(darch,name="darch",saveRBM=TRUE){
    if (!saveRBM){
      darch@rbmList <- list()
    }
    if (darch@ff){
      if (saveRBM){
        for(i in 1:length(darch@rbmList)){
          saveRBMFFWeights(darch@rbmList[[i]],paste(name,"RBM",i,sep=""))
        }
      }
      
      for(i in 1:length(darch@layers)){
        w <- darch@layers[[i]][[1]]
        ffsave(w,file=paste(name,"-W",i,sep=""))
      }
    }
    save(darch,saveRBM,file=paste(name,".net",sep=""))
  }
)
