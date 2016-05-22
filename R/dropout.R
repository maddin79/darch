# Copyright (C) 2013-2016 Martin Drees
# Copyright (C) 2015-2016 Johannes Rueckert
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

#' @include darch.Class.R
NULL

#' Dropout mask generator function.
#'
#' This function generates droput masks as a vector of given length, according
#' to the given dropout rate.
#'
#' @param length length of the dropout mask
#' @param dropoutRate The dropout rate (i.e. percentage of zeros in the mask)
#' @return Matrix containing the dropout mask
#'
#' @keywords internal
generateDropoutMask <- function(length, dropoutRate)
{
  sample(0:1, length, replace = T,
    prob = c(dropoutRate, 1 - dropoutRate))
}

generateDropoutMasksForDarch <- function(darch)
{
  numLayers <- length(darch@layers)
  dropConnect <- getParameter(".darch.dropout.dropConnect")
  dropout <- getParameter(".darch.dropout")
  
  # generate dropout masks
  setDropoutMask(darch, 0) <-
    generateDropoutMask(nrow(darch@layers[[1]][["weights"]]) - 1, dropout[1])
  for (i in 1:(numLayers - !dropConnect))
  {
    weights <- darch@layers[[i + !dropConnect]][["weights"]]
    length <- if (dropConnect) length(weights) else nrow(weights) - 1
    setDropoutMask(darch, i) <-
      generateDropoutMask(length, dropout[i + 1])
  }
  
  darch
}