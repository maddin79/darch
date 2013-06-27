#' Constructor function for DArch object.
#' 
#' TODO: Doc ...
#' 
#' @param layers Array of layer sizes.
#' @param batchSize Size of the batches
#' @param ff Indicates whether the \code{\link[ff]{ff}} package is used for the
#'        weights, biases and outputs
#' @param logfile The file for saving the log messages. The default prints the 
#'        log messages to the console. 
#' @param logLevel The logging level. 
#'        See \code{\link{setLogLevel}} for details.
#' @param genWeightFunc The function for generating the weight matrices
#' 
#' @return The new DArch object
#' @include darch.R
#' @include darch.Setter.R
#' @export
newDArch <- function(layers,batchSize,ff=FALSE,logfile="", logLevel=log4r:::INFO, genWeightFunc=generateWeights){
  darch <- new("DArch")
  logger <- create.logger(logfile,logLevel)
  log4r::info(logger,paste("Constructing a darch with ",length(layers), " layers.", sep=""))
  setLogger(darch) <- logger
  setFF(darch) <- ff
  setBatchSize(darch) <- batchSize  
  setGenWeightFunction(darch) <- genWeightFunc
  setStats <- list()
  darch <- generateRBMs(darch,layers,genWeightFunc)
  return(darch)
}