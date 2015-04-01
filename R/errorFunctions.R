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
#' Quadratic error function
#' 
#' The function calculates the quadratic error from the \code{original} and 
#' \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' 
#' @usage quadraticError(original, estimate)
#' 
#' @seealso \code{\link{mseError}},
#'          \code{\link{crossEntropyError}}
#'  
#' @docType methods
#' @rdname quadraticError
#' @export
quadraticError <- function(original, estimate){
    ret <- list("Quadratic-Error",sum((original[] - estimate[])^2))
  return(ret)
}

#' Mean squared error function
#' 
#' The function calculates the mean squared error (MSE) from the \code{original} 
#' and \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' 
#' @usage mseError(original, estimate)
#' 
#' @seealso \code{\link{quadraticError}},
#'          \code{\link{crossEntropyError}}
#'  
#' @docType methods
#' @rdname mseError
#' @export
mseError <- function(original, estimate){
  if(is.null(dim(original[]))){
    mFunc <- mean
  }else{
    mFunc <- colMeans
  }
  ret <- list("Mean-Sqared-Error",sum(mFunc((original[] - estimate[])^2)))
  return(ret)
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
#' 
#' @usage crossEntropyError(original, estimate)
#' 
#' @seealso \code{\link{quadraticError}},
#'          \code{\link{mseError}}
#'  
#' @docType methods
#' @rdname crossEntropyError
#' @export
crossEntropyError <- function(original, estimate){
  # C = - sum [all cases and outputs] (d*log(y) + (1-d)*log(1-y) )
  c <- -sum(original[]*log(estimate[]) + (1-original[])*log(1-estimate[]))
  ret <- list("Cross-Entropy-Error",c)
  return(ret)
}
