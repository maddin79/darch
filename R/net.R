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

#' Abstract class for neural networks.
#' 
#' This is a abstract class for neural networks. It provides some 
#' functionalities used in more than one network type.   
#' 
#' @slot batchSize Object of class \code{"numeric"}. The batch size 
#'   for the training and test data during the learning.
#' @slot errorFunction Object of class \code{"function"}. Function for error
#'   calculation.
#' @slot genWeightFunction Object of class \code{"function"}. A function to
#'   generate a randomly initialised weight matrix.
#' @slot normalizeWeights Logical indicating whether to normalize the weights.
#' @slot normalizeWeightsBound Upper bound on the L2 norm of incoming weight
#'  vectors
#' @slot learnRate Learning rate.
#' @slot learnRateScale Scale for learning rate.
#' @slot weightDecay Weight decay for the update of the weights. Weights will be
#'  multiplied by (1 - \code{weightDecay}) during each weight update.
#' @slot initialMomentum Initial momentum.
#' @slot finalMomentum Final momentum.
#' @slot momentumRampLength Defines the momentum ramp relative to the number of
#'  epochs.
#' @slot epochs Number of epochs.
#' @slot epochsScheduled Number of epochs the \code{\link{Net}} will have been
#'  trained for after the training, if the training is not aborted. Mainly to
#'  keep track of training session lengths for resumed training.
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
    genWeightFunction = "function",
    normalizeWeights = "logical",
    normalizeWeightsBound = "numeric",
    learnRate = "numeric",
    initialLearnRate = "numeric",
    learnRateScale = "numeric",
    weightDecay = "numeric",
    initialMomentum = "numeric",
    finalMomentum = "numeric",
    momentumRampLength = "numeric",
    epochs = "numeric",
    epochsScheduled = "numeric",
    stats = "list"
  )
)