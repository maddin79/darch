#' Constructor function for \code{\link{DArch}} objects.
#' 
#' Generate a new \code{\link{DArch}} object with the given parameters.
#' 
#' @details
#' It is recommended to use this function for generating a new 
#' \code{\link{DArch}} object, because this function generates and sets all the
#' necessary parameters like the internally used \code{\link{RBM}} networks, 
#' the list of statistiks (\code{stats}) etc.
#' 
#' @param layers Array of layer sizes.
#' @param batchSize Size of the batches
#' @param ff Indicates whether the \code{\link[ff]{ff}} package is used for the
#'        weights, biases and outputs
#' @param logLevel The logging level. 
#'        See \code{\link{setLogLevel}} for details.
#' @param genWeightFunc The function for generating the weight matrices
#' 
#' @usage newDArch(layers,batchSize,ff=FALSE, 
#' logLevel=INFO, genWeightFunc=generateWeights)
#' 
#' @return The new DArch object
#' @include darch.R
#' @include darch.Setter.R
#' 
#' @export
newDArch <- function(layers,batchSize,ff=FALSE, 
                     logLevel=INFO, genWeightFunc=generateWeights){
  darch <- new("DArch")
  flog.threshold(logLevel)
  flog.info(paste("Constructing a darch with ",length(layers), " layers.", sep=""))
  setFF(darch) <- ff
  setBatchSize(darch) <- batchSize  
  setGenWeightFunction(darch) <- genWeightFunc
  setStats <- list()
  darch <- generateRBMs(darch,layers,genWeightFunc)
  return(darch)
}