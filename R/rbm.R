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

#' Class for restricted Boltzmann machines
#' 
#' This class represents a restricted Boltzmann machine.
#' 
#' For generating a RBM it is recommended to use the constructor function:
#' \code{\link{newRBM}}. The RBM can be trained with the implementation of the
#' contrastive divergence method \code{\link{trainRBM}}. The class inherits the
#' attributes from the \code{\linkS4class{Net}}.
#' 
#' @slot numHidden Object of class \code{"numeric"}. Number of hidden units.
#' @slot numVisible Object of class \code{"numeric"}. Number of visible units.
#' @slot weights Object of class \code{"matrix"}. Weight matrix.
#' @slot weightInc Object of class \code{"matrix"}. Matrix of update values for 
#'   the Weight.
#' @slot output Object of class \code{"matrix"}. Output matrix of the RBM.
#' @slot visibleBiases Object of class \code{"array"}. Visible biases array.
#' @slot visibleBiasesInc Object of class \code{"array"}. Array of update values
#'   for the visible biases
#' @slot visibleUnitFunction Object of class \code{"function"}. Unit function 
#'   for the visible units.
#' @slot visibleUnitStates Object of class \code{"list"}. States of the visible 
#'   units.
#' @slot hiddenBiases Object of class \code{"array"}. Hidden biases array.
#' @slot hiddenBiasesInc Object of class \code{"array"}. Array of update values 
#'   for the hidden biases.
#' @slot hiddenUnitFunction Object of class \code{"function"}. Unit function for
#'   the hidden units.
#' @slot hiddenUnitStates Object of class \code{"list"}. States of the hidden 
#'   units.
#' @slot updateFunction Object of class \code{"function"}. Function for updating
#'   the weights and biases.
#' @slot posPhaseData Object of class \code{"list"}. Attribute to save the 
#'   positive phase data during the training.
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{DArch}}, 
#'   \code{\link{trainRBM}}
#' @author Martin Drees
#' @include net.R
#' @exportClass RBM
#' @aliases RBM
setClass(
  Class="RBM",
  representation=representation(
    numHidden = "numeric",
    numVisible = "numeric",
    weights = "matrix",
    weightInc = "matrix",
    output = "matrix",
    visibleBiases = "array",
    visibleBiasesInc = "array",
    visibleUnitFunction = "function",
    visibleUnitStates = "list",
    hiddenBiases = "array",
    hiddenBiasesInc = "array",
    hiddenUnitFunction = "function",
    hiddenUnitStates = "list",
    updateFunction = "function",
    posPhaseData = "list"
  ),
  contains="Net"
)

setMethod ("initialize","RBM",
  function(.Object){
    .Object@learnRate <- .8
    .Object@initialLearnRate <- .8
    .Object@learnRateScale <- 1
    .Object@weightDecay <- .0002
    .Object@initialMomentum <- .9
    .Object@finalMomentum <- .5
    .Object@momentumRampLength <- 1
    .Object@epochs <- 0
    .Object@epochsScheduled <- 0
    .Object@visibleUnitFunction <- sigmUnitFunc
    .Object@hiddenUnitFunction <- sigmUnitFunc
    .Object@updateFunction <- rbmUpdate
    .Object@errorFunction <- mseError
    .Object@genWeightFunction <- generateWeightsRunif
    .Object@normalizeWeights <- F
    .Object@normalizeWeightsBound <- 1
    return(.Object)
  }
)
