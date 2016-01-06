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

#' Returns a list with the states of the visible units.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getVisibleUnitStates(rbm)
#' @seealso \code{\link{RBM}}
#' @return The states of the visible units.
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getVisibleUnitStates-methods
setGeneric("getVisibleUnitStates",function(rbm){standardGeneric("getVisibleUnitStates")})

#' @rdname getVisibleUnitStates-methods
#' @aliases getVisibleUnitStates,RBM-method
setMethod(
  f="getVisibleUnitStates",
  signature="RBM",
  definition=function(rbm){
    return (rbm@visibleUnitStates)
  }
)

#' Returns a list with the states of the hidden units.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getHiddenUnitStates(rbm)
#' @return The states of the hidden units.
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getHiddenUnitStates-methods
setGeneric("getHiddenUnitStates",function(rbm){standardGeneric("getHiddenUnitStates")})

#' @rdname getHiddenUnitStates-methods
#' @aliases getHiddenUnitStates,RBM-method
setMethod(
  f="getHiddenUnitStates",
  signature="RBM",
  definition=function(rbm){
    return (rbm@hiddenUnitStates)
  }
)

#' Returns the output of the \code{\link{RBM}}
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getOutput(rbm)
#' @seealso \code{\link{RBM}}
#' @return The output of the \code{\link{RBM}}
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getOutput-methods
setGeneric("getOutput",function(rbm){standardGeneric("getOutput")})

#' @rdname getOutput-methods
#' @aliases getOutput,RBM-method
setMethod(
  f="getOutput",
  signature="RBM",
  definition=function(rbm)
  {
    return (rbm@output)
  }
)

#' Returns the number of hidden units of the \code{\link{RBM}}
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getNumHidden(rbm)
#' @seealso \code{\link{RBM}}
#' @return The number of hidden units of the \code{\link{RBM}}
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getNumHidden-methods
setGeneric("getNumHidden",function(rbm){standardGeneric("getNumHidden")})

#' @rdname getNumHidden-methods
#' @aliases getNumHidden,RBM-method
setMethod(
  f="getNumHidden",
  signature="RBM",
  definition=function(rbm){
    return (rbm@numHidden)
  }
)

#' Returns the number of visible units of the \code{\link{RBM}}
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getNumVisible(rbm)
#' @seealso \code{\link{RBM}}
#' @return The number of visible units of the \code{\link{RBM}}
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getNumVisible-methods
setGeneric("getNumVisible",function(rbm){standardGeneric("getNumVisible")})

#' @rdname getNumVisible-methods
#' @aliases getNumVisible,RBM-method
setMethod(
  f="getNumVisible",
  signature="RBM",
  definition=function(rbm){
    return (rbm@numVisible)
  }
)

#' Returns the learning rate for the visible biases.
#' 
#' 
#' @usage getLearnRateBiasVisible(rbm)
#' @seealso \code{\link{RBM}}
#' @return The learning rate for the visible biases
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getLearnRateBiasVisible-methods
setGeneric("getLearnRateBiasVisible",function(rbm){standardGeneric("getLearnRateBiasVisible")})

#' @rdname getLearnRateBiasVisible-methods
#' @aliases getLearnRateBiasVisible,RBM-method
setMethod(
  f="getLearnRateBiasVisible",
  signature="RBM",
  definition=function(rbm){
    return (rbm@learnRateBiasVisible)
  }
)

#' Returns the learning rate for the hidden biases.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getLearnRateBiasHidden(rbm)
#' @seealso \code{\link{RBM}}
#' @return The learning rate for the hidden biases
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getLearnRateBiasHidden-methods
setGeneric("getLearnRateBiasHidden",function(rbm){standardGeneric("getLearnRateBiasHidden")})

#' @rdname getLearnRateBiasHidden-methods
#' @aliases getLearnRateBiasHidden,RBM-method
setMethod(
  f="getLearnRateBiasHidden",
  signature="RBM",
  definition=function(rbm){
    return (rbm@learnRateBiasHidden)
  }
)

#' Returns the weights of the \code{\link{RBM}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getWeights(rbm)
#' @seealso \code{\link{RBM}}
#' @return The weigths of the \code{\link{RBM}}.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getWeights-methods
setGeneric("getWeights",function(rbm){standardGeneric("getWeights")})

#' @rdname getWeights-methods
#' @aliases getWeights,RBM-method
setMethod(
  f="getWeights",
  signature="RBM",
  definition=function(rbm)
  {
    return(rbm@weights)
  }
)

#' Returns the biases of the hidden units.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getHiddenBiases(rbm)
#' @seealso \code{\link{RBM}}
#' @return The biases of the hidden units.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getHiddenBiases-methods
setGeneric("getHiddenBiases",function(rbm){standardGeneric("getHiddenBiases")})

#' @rdname getHiddenBiases-methods
#' @aliases getHiddenBiases,RBM-method
setMethod(
  f="getHiddenBiases",
  signature="RBM",
  definition=function(rbm){
    return (rbm@hiddenBiases)
  }
)

#' Returns the update value for the biases of the hidden units.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getHiddenBiasesInc(rbm)
#' @seealso \code{\link{RBM}}
#' @return The update value for the biases of the hidden units.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getHiddenBiasesInc-methods
setGeneric("getHiddenBiasesInc",function(rbm){standardGeneric("getHiddenBiasesInc")})

#' @rdname getHiddenBiasesInc-methods
#' @aliases getHiddenBiasesInc,RBM-method
setMethod(
  f="getHiddenBiasesInc",
  signature="RBM",
  definition=function(rbm){
    return (rbm@hiddenBiasesInc)
  }
)

#' Returns the biases of the visible units.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getVisibleBiases(rbm)
#' @seealso \code{\link{RBM}}
#' @return The biases of the visible units.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getVisibleBiases-methods
setGeneric("getVisibleBiases",function(rbm){standardGeneric("getVisibleBiases")})

#' @rdname getVisibleBiases-methods
#' @aliases getVisibleBiases,RBM-method
setMethod(
  f="getVisibleBiases",
  signature="RBM",
  definition=function(rbm)
  {
    return (rbm@visibleBiases)
  }
)

#' Returns the update value for the biases of the visible units.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getVisibleBiasesInc(rbm)
#' @seealso \code{\link{RBM}}
#' @return The update value for the biases of the visible units.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getVisibleBiasesInc-methods
setGeneric("getVisibleBiasesInc",function(rbm){standardGeneric("getVisibleBiasesInc")})

#' @rdname getVisibleBiasesInc-methods
#' @aliases getVisibleBiasesInc,RBM-method
setMethod(
  f="getVisibleBiasesInc",
  signature="RBM",
  definition=function(rbm){
    return (rbm@visibleBiasesInc)
  }
)

#' Returns the update value for the weights.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getWeightInc(rbm)
#' @seealso \code{\link{RBM}}
#' @return The update value for the weights.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getWeightInc-methods
setGeneric("getWeightInc",function(rbm){standardGeneric("getWeightInc")})

#' @rdname getWeightInc-methods
#' @aliases getWeightInc,RBM-method
setMethod(
  f="getWeightInc",
  signature="RBM",
  definition=function(rbm){
    return (rbm@weightInc)
  }
)

#' Returns the data for the positive phase.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @usage getPosPhaseData(rbm)
#' @seealso \code{\link{RBM}}
#' @return The data for the positive phase.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getPosPhaseData-methods
setGeneric("getPosPhaseData",function(rbm){standardGeneric("getPosPhaseData")})

#' @rdname getPosPhaseData-methods
#' @aliases getPosPhaseData,RBM-method
setMethod(
  f="getPosPhaseData",
  signature="RBM",
  definition=function(rbm){
    return (rbm@posPhaseData)
  }
)
