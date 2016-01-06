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

#' Sets the states of the hidden units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The states of the hidden units
#' @usage setHiddenUnitStates(rbm) <- value
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setHiddenUnitStates-methods
#' @include rbm.R
setGeneric("setHiddenUnitStates<-",function(rbm,value){standardGeneric("setHiddenUnitStates<-")})

#' @rdname setHiddenUnitStates-methods
#' @aliases setHiddenUnitStates<-,RBM-method
#' @name setHiddenUnitStates
setReplaceMethod(
  f="setHiddenUnitStates",
  signature="RBM",
  definition=function(rbm,value){
    rbm@hiddenUnitStates <-value
    return (rbm)
  }
)

#' Sets the states of the visible units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The states of the visible units
#' @usage setVisibleUnitStates(rbm) <- value
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setVisibleUnitStates-methods
#' @include rbm.R
setGeneric("setVisibleUnitStates<-",function(rbm,value){standardGeneric("setVisibleUnitStates<-")})

#' @rdname setVisibleUnitStates-methods
#' @aliases setVisibleUnitStates<-,RBM-method
#' @name setVisibleUnitStates
setReplaceMethod(
  f="setVisibleUnitStates",
  signature="RBM",
  definition=function(rbm,value){
    rbm@visibleUnitStates <-value
    return (rbm)
  }
)

#' Sets the number of hidden units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The number of hidden units
#' @usage setNumHidden(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setNumHidden-methods
#' @include rbm.R
setGeneric("setNumHidden<-",function(rbm,value){standardGeneric("setNumHidden<-")})

#' @rdname setNumHidden-methods
#' @aliases setNumHidden<-,RBM-method
#' @name setNumHidden
setReplaceMethod(
  f="setNumHidden",
  signature="RBM",
  definition=function(rbm,value){
    rbm@numHidden <-value
    return (rbm)
  }
)

#' Sets the number of visible units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The number of visible units
#' @usage setNumVisible(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setNumVisible-methods
#' @include rbm.R
setGeneric("setNumVisible<-",function(rbm,value){standardGeneric("setNumVisible<-")})

#' @rdname setNumVisible-methods
#' @aliases setNumVisible<-,RBM-method
#' @name setNumVisible
setReplaceMethod(
  f="setNumVisible",
  signature="RBM",
  definition=function(rbm,value){
    rbm@numVisible <-value
    return (rbm)
  }
)

#' Sets the learnig rates of the biases for the  visible units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The learnig rates of the biases for the visible units
#' @usage setLearnRateBiasVisible(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setLearnRateBiasVisible-methods
#' @include rbm.R
setGeneric("setLearnRateBiasVisible<-",function(rbm,value){standardGeneric("setLearnRateBiasVisible<-")})

#' @rdname setLearnRateBiasVisible-methods
#' @aliases setLearnRateBiasVisible<-,RBM-method
#' @name setLearnRateBiasVisible
setReplaceMethod(
  f="setLearnRateBiasVisible",
  signature="RBM",
  definition=function(rbm,value){
    rbm@learnRateBiasVisible <-value
    return (rbm)
  }
)

#' Sets the learning rates of the biases for the hidden units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The learning rates of the biases for the hidden units
#' @usage setLearnRateBiasHidden(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setLearnRateBiasHidden-methods
#' @include rbm.R
setGeneric("setLearnRateBiasHidden<-",function(rbm,value){standardGeneric("setLearnRateBiasHidden<-")})

#' @rdname setLearnRateBiasHidden-methods
#' @aliases setLearnRateBiasHidden<-,RBM-method
#' @name setLearnRateBiasHidden
setReplaceMethod(
  f="setLearnRateBiasHidden",
  signature="RBM",
  definition=function(rbm,value){
    rbm@learnRateBiasHidden <-value
    return (rbm)
  }
)

#' Sets the weights of the \code{\link{RBM}} object
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The weights of the \code{\link{RBM}} object.
#' @usage setWeights(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setWeights-methods
#' @include rbm.R
setGeneric("setWeights<-",function(rbm,value){standardGeneric("setWeights<-")})

#' @rdname setWeights-methods
#' @aliases setWeights<-,RBM-method
#' @name setWeights
setReplaceMethod(
  f="setWeights",
  signature="RBM",
  definition=function(rbm,value){
    # weight normalization
    if (rbm@normalizeWeights)
    {
      nrows <- nrow(value)
      s <- sqrt(colSums(value^2))
      i <- which(s > rbm@normalizeWeightsBound)
      value[, i] <- (value[, i] / matrix(rep(s[i], nrows), nrow = nrows,
                     byrow = T) * rbm@normalizeWeightsBound)
    }
    
    rbm@weights <- value
    
    return (rbm)
  }
)

#' Sets the biases of the hidden units for the \code{\link{RBM}} object
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The biases of the hidden units for the \code{\link{RBM}} object.
#' @usage setHiddenBiases(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setHiddenBiases-methods
#' @include rbm.R
setGeneric("setHiddenBiases<-",function(rbm,value){standardGeneric("setHiddenBiases<-")})

#' @rdname setHiddenBiases-methods
#' @aliases setHiddenBiases<-,RBM-method
#' @name setHiddenBiases
setReplaceMethod(
  f="setHiddenBiases",
  signature="RBM",
  definition=function(rbm,value)
  {
      rbm@hiddenBiases <- value
    return (rbm)
  }
)

#' Sets the update value for the biases of the hidden units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The update value for the biases of the hidden units.
#' @usage setHiddenBiasesInc(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setHiddenBiasesInc-methods
#' @include rbm.R
setGeneric("setHiddenBiasesInc<-",function(rbm,value){standardGeneric("setHiddenBiasesInc<-")})

#' @rdname setHiddenBiasesInc-methods
#' @aliases setHiddenBiasesInc<-,RBM-method
#' @name setHiddenBiasesInc
setReplaceMethod(
  f="setHiddenBiasesInc",
  signature="RBM",
  definition=function(rbm,value){
    rbm@hiddenBiasesInc <-value
    return (rbm)
  }
)

#' Sets the biases of the visible units for the \code{\link{RBM}} object
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The biases of the visible units for the \code{\link{RBM}} object.
#' @usage setVisibleBiases(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setVisibleBiases-methods
#' @include rbm.R
setGeneric("setVisibleBiases<-",function(rbm,value){standardGeneric("setVisibleBiases<-")})

#' @rdname setVisibleBiases-methods
#' @aliases setVisibleBiases<-,RBM-method
#' @name setVisibleBiases
setReplaceMethod(
  f="setVisibleBiases",
  signature="RBM",
  definition=function(rbm,value)
  {
    rbm@visibleBiases <-value
    return (rbm)
  }
)

#' Sets the update value for the biases of the visible units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The update value for the biases of the visible units.
#' @usage setVisibleBiasesInc(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setVisibleBiasesInc-methods
#' @include rbm.R
setGeneric("setVisibleBiasesInc<-",function(rbm,value){standardGeneric("setVisibleBiasesInc<-")})

#' @rdname setVisibleBiasesInc-methods
#' @aliases setVisibleBiasesInc<-,RBM-method
#' @name setVisibleBiasesInc
setReplaceMethod(
  f="setVisibleBiasesInc",
  signature="RBM",
  definition=function(rbm,value){
    rbm@visibleBiasesInc <-value
    return (rbm)
  }
)

#' Sets the unit function of the hidden units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The unit function of the hidden units
#' @usage setHiddenUnitFunction(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setHiddenUnitFunction-methods
#' @include rbm.R
setGeneric("setHiddenUnitFunction<-",function(rbm,value){standardGeneric("setHiddenUnitFunction<-")})

#' @rdname setHiddenUnitFunction-methods
#' @aliases setHiddenUnitFunction<-,RBM-method
#' @name setHiddenUnitFunction
setReplaceMethod(
  f="setHiddenUnitFunction",
  signature="RBM",
  definition=function(rbm,value){
    rbm@hiddenUnitFunction <- value
    return (rbm)
  }
)

#' Sets the unit function of the visible units
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The unit function of the visible units
#' @usage setVisibleUnitFunction(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setVisibleUnitFunction-methods
#' @include rbm.R
setGeneric("setVisibleUnitFunction<-",function(rbm,value){standardGeneric("setVisibleUnitFunction<-")})

#' @rdname setVisibleUnitFunction-methods
#' @aliases setVisibleUnitFunction<-,RBM-method
#' @name setVisibleUnitFunction
setReplaceMethod(
  f="setVisibleUnitFunction",
  signature="RBM",
  definition=function(rbm,value){
    rbm@visibleUnitFunction <- value
    return (rbm)
  }
)

#' Sets the update function of the \code{\link{RBM}} object
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The update function of the \code{\link{RBM}} object.
#' @usage setUpdateFunction(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setUpdateFunction-methods
#' @include rbm.R
setGeneric("setUpdateFunction<-",function(rbm,value){standardGeneric("setUpdateFunction<-")})

#' @rdname setUpdateFunction-methods
#' @aliases setUpdateFunction<-,RBM-method
#' @name setUpdateFunction
setReplaceMethod(
  f="setUpdateFunction",
  signature="RBM",
  definition=function(rbm,value){
    rbm@updateFunction <- value
    return (rbm)
  }
)

#' Sets the update values for the weights 
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The update values for the weights 
#' @usage setWeightInc(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setWeightInc-methods
#' @include rbm.R
setGeneric("setWeightInc<-",function(rbm,value){standardGeneric("setWeightInc<-")})

#' @rdname setWeightInc-methods
#' @aliases setWeightInc<-,RBM-method
#' @name setWeightInc
setReplaceMethod(
  f="setWeightInc",
  signature="RBM",
  definition=function(rbm,value){
    rbm@weightInc <- value
    return (rbm)
  }
)

#' Sets the positive phase data for the training
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The positive phase data for the training
#' @usage setPosPhaseData(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setPosPhaseData-methods
#' @include rbm.R
setGeneric("setPosPhaseData<-",function(rbm,value){standardGeneric("setPosPhaseData<-")})

#' @rdname setPosPhaseData-methods
#' @aliases setPosPhaseData<-,RBM-method
#' @name setPosPhaseData
setReplaceMethod(
  f="setPosPhaseData",
  signature="RBM",
  definition=function(rbm,value){
    rbm@posPhaseData <- value
    return (rbm)
  }
)

#' Sets the output of the \code{\link{RBM}} object
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param value The output of the \code{\link{RBM}} object
#' @usage setOutput(rbm) <- value
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @export
#' @docType methods
#' @rdname setOutput-methods
#' @include rbm.R
setGeneric("setOutput<-",function(rbm,value){standardGeneric("setOutput<-")})

#' @rdname setOutput-methods
#' @aliases setOutput<-,RBM-method
#' @name setOutput
setReplaceMethod(
  f="setOutput",
  signature="RBM",
  definition=function(rbm,value){
    rbm@output <-value
    rbm
  }
)
