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

#' Class for restricted Boltzmann machines
#' 
#' This class represents a restricted Boltzmann machine.
#' 
#' The RBM can be trained with the implementation of the
#' contrastive divergence method \code{\link{trainRBM}}. The class inherits the
#' attributes from the \code{\linkS4class{Net}}.
#' 
#' @slot weights Object of class \code{"matrix"}. Weight matrix.
#' @slot weightsInc Object of class \code{"matrix"}. Matrix of update values for 
#'   the Weight.
#' @slot visibleBiases Object of class \code{"array"}. Visible biases array.
#' @slot visibleBiasesInc Object of class \code{"array"}. Array of update values
#'   for the visible biases
#' @slot visibleUnitStates Object of class \code{"list"}. States of the visible 
#'   units.
#' @slot hiddenBiases Object of class \code{"array"}. Hidden biases array.
#' @slot hiddenBiasesInc Object of class \code{"array"}. Array of update values 
#'   for the hidden biases.
#' @slot hiddenUnitStates Object of class \code{"list"}. States of the hidden 
#'   units.
#' @slot posPhaseData Object of class \code{"list"}. Attribute to save the 
#'   positive phase data during the training.
#' @slot output Object of class \code{"matrix"}. Output matrix of the RBM.
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{DArch}}, 
#'   \code{\link{trainRBM}}
#' @author Martin Drees
#' @include net.Class.R
#' @exportClass RBM
#' @keywords internal
#' @family darch classes
#' @rdname RBM
setClass(
  Class = "RBM",
  representation = representation(
    weights = "matrix",
    weightsInc = "matrix",
    visibleBiases = "array",
    visibleBiasesInc = "array",
    visibleUnitStates = "list",
    hiddenBiases = "array",
    hiddenBiasesInc = "array",
    hiddenUnitStates = "list",
    posPhaseData = "list",
    output = "matrix"
  ),
  contains = "Net"
)

setMethod("initialize","RBM",
  function(.Object){
    .Object@epochs <- 0
    .Object@parameters <- list()
    return(.Object)
  }
)
