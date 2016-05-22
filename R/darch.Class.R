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

#' Class for deep architectures
#' 
#' This class represents a model created, and/or configured, and/or trained with
#' the \code{\link{darch}} function.
#' It implements deep architectures and provides the ability to train 
#' them with a pre training using contrastive divergence and fine tuning with 
#' backpropagation, resilient backpropagation and conjugate gradients.
#' 
#' The class inherits all attributes from the class \code{\linkS4class{Net}}.
#' User-relevant slots include \code{stats} (training statistics), \code{epochs}
#' (numer of epoch this model was trained for), and \code{parameters} (all
#' parameters passed to \code{\link{darch}} as well as internal parameters).
#' 
#' @slot rbmList A list which contains all RBMs for the pre-training.
#' @slot layers A list with the layer information. In the first field are the 
#'   weights and in the second field is the unit function.
#' @slot cancel Boolean value which indicates if the network training is 
#'   canceled.
#' @slot cancelMessage The message when the execution is canceled.
#' @slot dropoutMasks List of dropout masks, used internally.
#' @slot dataSet \linkS4class{DataSet} instance.
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{RBM}}
#' @author Martin Drees
#' @include net.Class.R
#' @exportClass DArch
#' @rdname DArch-class
#' @family darch classes
#' @keywords internal
setClass(
  Class = "DArch",
  representation = representation(
    rbmList = "list",
    layers = "list",
    cancel = "logical",
    cancelMessage = "character",
    dropoutMasks = "list",
    dataSet = "ANY"
  ),
  contains = "Net"
)

setMethod("initialize","DArch",
           function(.Object){	
             .Object@epochs <- 0
             .Object@cancel <- FALSE
             .Object@cancelMessage <- "No reason specified."
             .Object@dataSet <- NULL
             .Object@parameters <- list()
             return(.Object)    
           }
)
