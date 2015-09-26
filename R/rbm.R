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
#' attributes from the \code{\linkS4class{Net}}. The if the attribute ff is
#' \code{TRUE}, the attributes with the ff-prefix are used to save the
#' parameters of the RBM network. It is recommended to use the setter and getter
#' method for access the attributes, because then there is no need to request
#' the ff attribute to access the right attribute.
#' 
#' @slot learnRateBiasVisible Object of class \code{"numeric"}. Learning rate of
#'   the visible biases.
#' @slot learnRateBiasHidden Object of class \code{"numeric"}. Learning rate of 
#'   the hidden biases.
#' @slot weightCost Object of class \code{"numeric"}. Weight cost for the update
#'   of the weights.
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
#' @slot ffWeights Object of class \code{"ff_matrix"}. Weight ff matrix. Used
#'   when the ff attribute is \code{TRUE}.
#' @slot ffOutput Object of class \code{"ff_matrix"}. Output ff matrix of the
#'   RBM. Used when the ff attribute is \code{TRUE}.
#' @slot ffHiddenBiases Object of class \code{"ff_array"}. Hidden biases ff
#'   array. Used when the ff attribute is \code{TRUE}.
#' @slot ffVisibleBiases Object of class \code{"ff_array"}. Hidden biases ff
#'   array. Used when the ff attribute is \code{TRUE}.
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{DArch}},
#'   \code{\link{trainRBM}}
#' @author Martin Drees
#' @include net.R
#' @exportClass RBM
#' @aliases RBM
setClass(
		Class="RBM",
		representation=representation(
				learnRateBiasVisible = "numeric",
				learnRateBiasHidden = "numeric",
				weightCost = "numeric",
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
				posPhaseData = "list",
				
				# ff slots
				ffWeights = "ff_matrix",
				ffOutput = "ff_matrix",
				ffHiddenBiases = "ff_array",
				ffVisibleBiases = "ff_array"
		),
		contains="Net"
)

setMethod ("initialize","RBM",
		function(.Object){
			
			.Object@learnRateWeights <- .1
			.Object@learnRateBiasVisible <- .1
			.Object@learnRateBiasHidden <- .1
			.Object@weightCost <- .0002
      .Object@initialMomentum <- .9
			.Object@finalMomentum <- .5
			.Object@momentumSwitch <- 5
      .Object@epochs <- 0
			.Object@visibleUnitFunction <- sigmUnitFunc
			.Object@hiddenUnitFunction <- sigmUnitFuncSwitch
			.Object@updateFunction <- rbmUpdate
			.Object@errorFunction <- mseError
			.Object@genWeightFunction <- generateWeights
      .Object@normalizeWeights <- F
			return(.Object)    
		}
)
