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

#' Class for deep architectures
#' 
#' This class implements deep architectures and provides the ability to train 
#' them with a pre training using contrastive divergence and fine tuning with 
#' backpropagation, resilient backpropagation and conjugate gradients.
#' 
#' The class is inherits all attributes from the class \code{link{Net}}. When
#' creating a new instance with the constructor \code{\link{newDArch}}
#' (recommended), the darch-object contained the number of layers -1 restricted
#' Boltzmann machines (\code{\link{RBM}}), which are used for the unsupervised
#' pre training of the network. The \code{\link{RBM}}s are saved in the
#' attribute \code{rbmList} and can be fetched over the getter method
#' (\code{\link{getRBMList}}. The two attributes \code{fineTuneFunction} and
#' \code{executeFunction} containing the functions for the fine tuning (default:
#' \code{\link{backpropagation}}) and for the execution (default: 
#' \code{\link{runDArch}}. The training of the network is performed by the two 
#' learning functions \code{\link{preTrainDArch}} and 
#' \code{\link{fineTuneDArch}}. The first function trains the network with the 
#' unsupervised method contrastive divergence. The second function used the 
#' function in the attribute \code{fineTuneFunction} for the fine tuning. After 
#' an execution of the network, the outputs of every layer can be found in the 
#' attribute \code{executeOutput}.
#' 
#' @slot rbmList A list which contains all RBMs for the pre-training.
#' @slot layers A list with the layer information. In the first field are the 
#'   weights and in the second field is the unit function.
#' @slot learnRateBiases The learning rate for the bias weights.
#' @slot fineTuneFunction Contains the function for the fine tuning.
#' @slot executeFunction Contains the function for executing the network.
#' @slot executeOutput A list which contains the outputs of every layer after 
#'  an execution of the network.
#' @slot cancel Boolean value which indicates if the network training is 
#'   canceled.
#' @slot cancelMessage The message when the execution is canceled.
#' @slot dropoutInput Dropout rate on the input layer.
#' @slot dropoutHidden Dropout rate on the hidden layers.
#' @slot dropoutOneMaskPerEpoch Logical indicating whether to generate a new
#'  dropout mask for each epoch (as opposed to for each batch).
#' @slot dropConnect Logical indicating whether to use DropConnect instead of
#'  Dropout.
#' @slot dropoutMasks List of dropout masks, used internally.
#' @slot dataSet \linkS4class{DataSet} instance.
#' @slot preTrainParameters A set of parameters keeping track of the state of
#'  the DBN in terms of pre-training.
#' @slot fineTuningParameters A set of parameters keeping track of the state of
#'  the DBN in terms of fine-tuning.
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{RBM}}
#' @author Martin Drees
#' @include net.R
#' @exportClass DArch
#' @aliases DArch
setClass(
  Class="DArch",
  representation=representation(
    rbmList = "list",
    layers = "list",
    fineTuneFunction = "function",
    executeFunction = "function",
    executeOutput = "list",
    cancel = "logical",
    cancelMessage = "character",
    dropoutInput = "numeric",
    dropoutHidden = "numeric",
    dropoutOneMaskPerEpoch = "logical",
    dropConnect = "logical",
    dither = "logical",
    dropoutMasks = "list",
    dataSet = "ANY",
    preTrainParameters = "list",
    fineTuningParameters = "list",
    params = "list"
  ),
  contains="Net"
)

setMethod ("initialize","DArch",
           function(.Object){	
             .Object@executeFunction <- runDArchDropout
             .Object@genWeightFunction <- generateWeightsRunif
             .Object@fineTuneFunction <- backpropagation
             .Object@initialMomentum <- .9
             .Object@finalMomentum <- .5
             .Object@momentumRampLength <- 1
             .Object@epochs <- 0
             .Object@epochsScheduled <- 0
             .Object@learnRate <- .8
             .Object@initialLearnRate <- .8
             .Object@learnRateScale <- 1
             .Object@weightDecay <- 0
             .Object@normalizeWeights <- F
             .Object@normalizeWeightsBound <- 1
             .Object@errorFunction <- mseError
             .Object@cancel <- FALSE
             .Object@cancelMessage <- "No reason specified."
             .Object@dropoutInput <- 0.
             .Object@dropoutHidden <- 0.
             .Object@dropoutOneMaskPerEpoch <- F
             .Object@dropConnect <- F
             .Object@dither <- F
             .Object@dataSet <- NULL
             .Object@preTrainParameters <- list()
             .Object@fineTuningParameters <- list()
             .Object@params <- list()
             return(.Object)    
           }
)
