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

#' Generates a weight matrix. 
#' 
#' This function is the standard method for generating weights for instances of
#' \code{\link{Net}}. When using another function to generate weights, the 
#' function must be like this one.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @docType methods
#' @rdname generateWeights
#' @export
generateWeights <- function(numUnits1,numUnits2){
  # TODO why 0.1?
  ret <- matrix(rnorm(numUnits1*numUnits2)*0.1,numUnits1,numUnits2) #matrix(0.02,numUnits1,numUnits2)
  return(ret)
}