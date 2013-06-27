#' Loads a RBM network
#' 
#' Loads the RBM object from the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details Make sure when you load a RBM object that every file written by 
#' the \code{\link{saveRBM}}-Funktion, specially when the parameter \code{ff}
#' of the saved RBM object is \code{TRUE}, are in the working directory
#' 
#' @param name The name of the file without the ending ".net".
#' @return \code{rbm} - The loaded RBM
#' @usage loadRBM(name="rbm")
#' 
#' @seealso \code{\link{saveRBM}}, \code{\link{loadRBMFFWeights}}, \code{\link{saveRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname loadRBM
loadRBM <- function(name="rbm"){
    load(paste(name,".net",sep=""))
    if(rbm@ff == TRUE){
      rbm <- loadRBMFFWeights(rbm,name)
    }
    return(rbm)
  }
