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
#' @details If the parameter \code{saveRBM} is \code{FALSE} the field
#'   \code{rbmList} of the DArch object is overwritten by an empty list.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param name The name for the file. Default value is "darch".
#' @param saveRBM Boolean value to indicate if the RBM is saved.
#' 
#' @usage saveDArch(darch,name="darch",saveRBM=TRUE)
#' 
#' @seealso \code{\link{loadDArch}}
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
  definition=function(darch, name="darch", saveRBM=TRUE)
  {
    if (!saveRBM){
      darch@rbmList <- list()
    }
    
    save(darch, saveRBM, file=paste(name, ".net", sep=""))
  }
)
