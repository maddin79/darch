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

#' Dropout mask generator function.
#'
#' This function generates droput masks as a vector of given length, according
#' to the given dropout rate.
#'
#' @param length length of the dropout mask
#' @param dropoutRate The dropout rate (i.e. percentage of zeros in the mask)
#' @return Matrix containing the dropout mask
#'
#' @seealso \code{\link{DArch}}
generateDropoutMask <- function(length, dropoutRate)
{
  if (dropoutRate == 0)
  {
    ret <- rep(1, length)
  }
  else
  {
    ret <- sample(0:1, length, replace = T,
                  prob = c(dropoutRate, 1 - dropoutRate))
  }
  
  return (ret)
}

generateDropoutMasksForDarch <- function(darch)
{
  dropoutMasks <- list()
  numLayers <- length(getLayers(darch))
  
  # generate dropout masks
  setDropoutMask(darch, 0) <-
                  generateDropoutMask(nrow(getLayerWeights(darch, 1)[]) - 1,
                                      darch@dropoutInput)
  for (i in 1:(numLayers - 1))
  {
    setDropoutMask(darch, i) <-
                    generateDropoutMask(nrow(getLayerWeights(darch, i+1)[])-1,
                                        darch@dropoutHidden)
  }
  
  return (darch)
}

#' Applies the given dropout mask to the given data row-wise.
#' 
#' This function multiplies each row with the dropout mask. To apply the dropout
#' mask by row, it can simply be multiplied with the data matrix. This does not
#' work of the mask is to be applied row-wise, hence this function.
#' 
#' @param data Data to which the dropout mask should be applied
#' @param mask The dropout mask, a vector of 0 and 1.
#' @return Data with applied dropout mask
applyDropoutMask <- function(data, mask)
{
  return (data * matrix(rep(mask, nrow(data)), nrow=nrow(data), byrow=T))
}