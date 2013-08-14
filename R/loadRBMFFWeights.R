#' Loads weigths and biases for a RBM network from a ffData file.
#' 
#' Loads the weigths and the biases for the given RBM object from the filename 
#' given through the parameter \code{name}. See \code{\link{ffload}} for more
#' details
#'  
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param name The name of the file without the ending ".net".
#' @return \code{rbm} - The RBM with the loaded weights and biases.
#' @usage loadRBMFFWeights(rbm,name)
#' 
#' @seealso \code{\link{ffload}}, \code{\link{saveRBM}}, \code{\link{loadRBM}}, \code{\link{saveRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname loadRBMFFWeights
loadRBMFFWeights <- function(rbm,name){
    w <- 1
    v <- 1
    h <- 1
    ffload(paste(name,"-WB",sep=""),overwrite=TRUE)
    rbm@ffWeights <- w
    rbm@ffHiddenBiases <- h
    rbm@ffVisibleBiases <- v
    return(rbm)
  }
