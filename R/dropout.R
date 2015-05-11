# Copyright (C) 2015 darch2
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
#' Dropout mask generator function.
#'
#' This function generates droput masks as a vector of given length, according
#' to the given dropout rate.
#'
#' @param length length of the dropout mask
#' @param dropoutRate The dropout rate (i.e. percentage of zeros in the mask)
#' @return Matrix containing the dropout mask
#'
#' @usage generateDropoutMask(length,dropoutRate)
#'
#' @seealso \code{\link{DArch}}
#'
#' @docType methods
#' @rdname generateDropoutMask
#' @include darch.R
#' @export
generateDropoutMask <- function(length, dropoutRate){
  if (dropoutRate == 0.)
  {
    ret <- rep(1, length)
  }
  else
  {
    ret <- sample(0:1,length,replace=T,prob=c(dropoutRate,1-dropoutRate))
  }
  
  return (ret)
}

#' Applies the given dropout mask to the given data.
#'
#' This function multiplies each row/column with the according element of the
#' dropout mask.
#'
#' @param data Data to which the dropout mask should be applied
#' @param mask The dropout mask, a vector of 0 and 1.
#' @param byrow Whether to apply the mask to rows or columns.
#' @return Data with applied dropout mask
#'
#' @usage applyDropoutMask(data, mask, byrow)
#'
#' @seealso \code{\link{trainRBM}}
#'
#' @docType methods
#' @rdname applyDropoutMask
#' @include darch.R
#' @export
applyDropoutMask <- function(data, mask, byrow=F)
{
  if (byrow)
  {
    length <- nrow(data)
  }
  else
  {
    length <- ncol(data)
  }
  
  for (i in 1:length)
  {
    if (byrow)
    {
      data[i,] <- data[i,] * mask[i]
    }
    else
    {
      data[,i] <- data[,i] * mask[i]
    }
  }
  
  return (data)
}