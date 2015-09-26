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

#' Makes start- and end-points for the batches.
#' 
#' The start- and end-points are used for dividing the data into batches. 
#' 
#' @details If the data is not divisible by the \code{batchSize} the last batch 
#' will contain the rest of the data. 
#' The function returns a list with in which the first entry is a list with the
#' values for the start and end points for reading the data matrix. The second
#' entry is the number of batches. 
#' 
#' @param batchSize Desired batch size
#' @param numRows Number of rows of the data
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname makeStartEndPoints
#' @include net.R
makeStartEndPoints <- function(batchSize,numRows){
    numBatches <- ceiling(numRows/batchSize)
    batchValues <- list()
    batchValues[[1]] <- 0
    for(n in 2:(numBatches)){
      batchValues[[n]] <- (n-1)*batchSize 
    }
    
    batchValues[[length(batchValues)+1]] <- numRows
    
    return(list(batchValues,numBatches))
  }
