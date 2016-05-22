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

#' Mean squared error function
#' 
#' The function calculates the mean squared error (MSE) from the \code{original}
#' and \code{estimate} parameters.
#' 
#' This function is a valid value for both \code{\link{darch}} parameters
#' \code{rbm.errorFunction} and \code{darch.errorFunction}.
#' 
#' @param original The original data matrix.
#' @param estimate The calculated data matrix.
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, rbm.errorFunction = "mseError",
#'  darch.errorFunction = "mseError")
#' }
#' @family error functions
#' @export
mseError <- function(original, estimate)
{
  # TODO colMeans? .5*?
  list("MSE", sum(colMeans((original - estimate) ^ 2)))
}

#' Root-mean-square error function
#' 
#' The function calculates the root-mean-square error (RMSE) from the
#' \code{original} and \code{estimate} parameters.
#' 
#' This function is a valid value for both \code{\link{darch}} parameters
#' \code{rbm.errorFunction} and \code{darch.errorFunction}.
#' 
#' @param original The original data matrix.
#' @param estimate The calculated data matrix.
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, rbm.errorFunction = "rmseError",
#'  darch.errorFunction = "rmseError")
#' }
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
#' This function can be used for the \code{darch.errorFunction} parameter of the
#' \code{\link{darch}} function, but is only a valid error function if used in
#' combination with the \code{\link{softmaxUnit}} activation function! It is not
#' a valid value for the parameter \code{rbm.errorFunction}.
#'
#' @param original The original data matrix.
#' @param estimate The calculated data matrix.
#' @return A list with the name of the error function in the first entry and the
#'   error value in the second entry.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.errorFunction = "crossEntropyError")
#' }
#' @family error functions
#' @references Rubinstein, R.Y., Kroese, D.P. (2004). The Cross-Entropy Method:
#'   A Unified Approach to Combinatorial Optimization, Monte-Carlo Simulation,
#'   and Machine Learning, Springer-Verlag, New York.
#' @export
crossEntropyError <- function(original, estimate)
{
  # C = - sum [all cases and outputs] (d*log(y) + (1-d)*log(1-y) )
  c <- -sum(colMeans(original * log(pmax(estimate, .Machine$double.eps)) +
    (1 - original) * log(pmax(1 - estimate, .Machine$double.eps))))
  #c <- -sum(original*log(estimate))
  list("Cross Entropy error",c)
}
