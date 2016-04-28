# Copyright (C) 2013-2016 Martin Drees
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

#' Mean squared error function
#' 
#' The function calculates the mean squared error (MSE) from the \code{original} 
#' and \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' @family error functions
#' @export
mseError <- function(original, estimate)
{
  # TODO colMeans? .5*?
  list("MSE", sum(colMeans((original - estimate)^2)))
}

#' Root-mean-square error function
#' 
#' The function calculates the root-mean-square error (RMSE) from the
#' \code{original} and \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' @family error functions
#' @export
rmseError <- function(original, estimate)
{
  list("RMSE", sum(sqrt(colMeans((original - estimate) ^ 2))))
}

#' Cross entropy error function
#' 
#' The function calculates the cross entropy error from the \code{original} and 
#' \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' @family error functions
#' @export
crossEntropyError <- function(original, estimate)
{
  # C = - sum [all cases and outputs] (d*log(y) + (1-d)*log(1-y) )
  c <- -sum(colMeans(original * log(pmax(estimate, .Machine$double.eps)) +
    (1 - original) * log(pmax(1 - estimate, .Machine$double.eps))))
  #c <- -sum(original*log(estimate))
  list("Cross Entropy error",c)
}
