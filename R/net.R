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

setOldClass(c("ff_matrix","ff_array"))

#' Abstract class for neural networks.
#' 
#' This is a abstract class for neural networks. It provides some 
#' functionalities used in more than one network type.   
#' 
#' @slot batchSize Object of class \code{"numeric"}. The batch size 
#'   for the training and test data during the learning.
#' @slot errorFunction Object of class \code{"function"}. Function for error
#'   calculation.
#' @slot ff Object of class \code{"logical"}. Indicates if the package
#'   \code{\link[ff]{ff}} is used to save the network data.
#' @slot genWeightFunction Object of class \code{"function"}. A function to
#'   generate a randomly initialised weight matrix.
#' @slot normalizeWeights Logical indicating whether to normalize the weights.
#' @slot learnRateWeights Learn rate.
#' @slot initialMomentum Initial momentum.
#' @slot finalMomentum Final momentum.
#' @slot momentumSwitch Epoch at which to switch from \code{initialMomentum} to
#'  \code{finalMomentum}.
#' @slot epochs Number of epochs.
#' @slot stats Training statistics.
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{RBM}}
#' @author Martin Drees
#' @exportClass Net
#' @aliases Net
setClass(
  Class="Net",
  representation=representation(
    batchSize = "numeric",
    errorFunction = "function",
    ff = "logical",
    genWeightFunction = "function",
    normalizeWeights = "logical",
    learnRateWeights = "numeric",
    initialMomentum = "numeric",
    finalMomentum = "numeric",
    momentumSwitch = "numeric",
    epochs = "numeric",
    stats = "list"
  )
)