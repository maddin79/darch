#' Saves weights and biases of a RBM network into a ffData file.
#' 
#' Saves the weigths and the biases for the given RBM object to the filename 
#' given through the parameter \code{name}.
#' 
#' @details The weights and biases are saved in one file with the name given 
#' through the parameter \code{name} and the string "-WB". See \code{\link{ffsave}}
#' for more details.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param name The name for the file.
#' 
#' @usage saveRBMFFWeights(rbm,name="saveName")
#' 
#' @seealso \code{\link{ffsave}}, \code{\link{loadRBM}}, \code{\link{saveRBM}}, \code{\link{loadRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname saveRBMFFWeights-methods
setGeneric(
  name="saveRBMFFWeights",
  def=function(rbm,name="saveName"){standardGeneric("saveRBMFFWeights")}
)

#' @rdname saveRBMFFWeights-methods
#' @aliases saveRBMFFWeights,RBM-method
setMethod(
  f="saveRBMFFWeights",
  signature="RBM",
  definition=function(rbm,name="saveName"){
    w <- rbm@ffWeights
    h <- rbm@ffHiddenBiases
    v <- rbm@ffVisibleBiases
    file <- paste(name,"-WB",sep="")
    ffsave(w,h,v,file=file)
  }
)