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
#' Loads a DArch network
#' 
#' Loads the DArch object from the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details Make sure when you load a DArch object that every file written by 
#' the \code{\link{saveDArch}}-Funktion, specially when the parameter \code{ff}
#' of the saved DArch object is \code{TRUE}, are in the working directory.
#' 
#' @param name The name of the file without the ending ".net".
#' 
#' @return \code{darch} - The loaded deep architecture
#' 
#' @usage loadDArch(name="darch")
#' 
#' @seealso \code{\link{saveDArch}}, \code{\link{loadRBMFFWeights}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname loadDArch
loadDArch <- function(name="darch"){
    load(paste(name,".net",sep=""))
    if(darch@ff){
      if(saveRBM){
        for(i in 1:length(darch@rbmList)){
          darch@rbmList[[i]] <- loadRBMFFWeights(darch@rbmList[[i]],paste(name,"RBM",i,sep=""))
        }
      }
      w <- 1
      for(i in 1:length(darch@layers)){
        ffload(paste(name,"-W",i,sep=""),overwrite=TRUE)
        darch@layers[[i]][[1]] <- w			
      }
    }
    return(darch)
  }
