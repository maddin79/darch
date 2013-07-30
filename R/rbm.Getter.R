#' Returns a list with the states of the visible units.
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' 
#' @seealso \code{\link{RBM}}
#' 
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' 
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' 
#' @seealso \code{\link{RBM}}
#' 
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
  definition=function(rbm){
    if(rbm@ff){
      return(rbm@ffOutput[])
    }
    return (rbm@output)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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

#' BEARBEITEN
#' 
#' BEARBEITEN
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname getWeightCost-methods
setGeneric("getWeightCost",function(rbm){standardGeneric("getWeightCost")})

#' @rdname getWeightCost-methods
#' @aliases getWeightCost,RBM-method
setMethod(
  f="getWeightCost",
  signature="RBM",
  definition=function(rbm){
    return (rbm@weightCost)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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
  definition=function(rbm){
    if(rbm@ff){
      return(rbm@ffWeights[])
    }
    return(rbm@weights)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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
    if(rbm@ff){
      return(rbm@ffHiddenBiases[])
    }
    return (rbm@hiddenBiases)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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
  definition=function(rbm){
    if(rbm@ff){
      return(rbm@ffVisibleBiases[])
    }
    return (rbm@visibleBiases)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
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
