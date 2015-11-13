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

# TODO documentation on weight generation functions in general (minimum params)

#' Generates a weight matrix using \link{rnorm}. 
#' 
#' This function is the standard method for generating weights for instances of
#' \code{\link{Net}}. It uses \link{rnorm} to do so.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param weights.mean \code{mean} parameter to the \link{rnorm} function
#' @param weights.sd \code{sd} parameter to the \link{rnorm} function
#' 
#' @seealso \code{\link{Net}}
#' 
#' @family weight generation functions
#' @export
generateWeightsRnorm <- function(numUnits1,numUnits2, weights.mean = 0, weights.sd = .025)
{
  matrix(rnorm(numUnits1*numUnits2, weights.mean, weights.sd),
                numUnits1, numUnits2)
}

#' Generates a weight matrix using \link{runif}
#' 
#' This function is used to generate random weights and biases using
#' \link{runif}.
#' 
#' @param numUnits1 Number of units in the lower layer
#' @param numUnits2 Number of units in the upper layer
#' @param weights.min \code{min} parameter to the \link{runif} function
#' @param weights.max \code{max} parameter to the \link{runif} function
#' 
#' @seealso \code{\link{Net}}
#' 
#' @family weight generation functions
#' @export
generateWeightsRunif <- function(numUnits1,numUnits2, weights.min = -.1, weights.max = .1)
{
  matrix(runif(numUnits1*numUnits2, weights.min, weights.max),
         numUnits1, numUnits2)
}