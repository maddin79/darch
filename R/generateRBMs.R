#' Generates the rbm's for the pre-training.
#' 
#' Used the layer sizes from the DArch object to create the RBM objects for the
#' pre-training. 
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param layers An array with the sizes of the layers
#' @param genWeightFunc The function for generating the weight matrices
#' @return The DArch object with the generated rbm's
#' 
#' @seealso \code{\link{DArch}}
#'          \code{\link{RBM}}
#' 
#' @docType methods
#' @rdname generateRBMs-methods
#' @include darch.R
#' @include rbm.R
#' @export
setGeneric(
  name="generateRBMs",
  def=function(darch,layers,genWeightFunc=generateWeights){standardGeneric("generateRBMs")}
)

#' @rdname generateRBMs-methods
#' @aliases generateRBMs,DArch-method
setMethod(
  f="generateRBMs",
  signature="DArch",
  definition=function(darch,layers,genWeightFunc=generateWeights){
    darch@rbmList <- list()
    logger <- getLogger(darch)			
    log4r::info(logger,"Generating RBMs.")
    for(i in 1:(length(layers)-1)){
      # generate the RBMs
      visible <- layers[[i]]
      hidden <- layers[[(i+1)]]
      rbm <- newRBM(visible,hidden,getBatchSize(darch),getFF(darch),logfile(logger),level(logger),genWeightFunc)
      darch@rbmList[i] <- rbm
      darch <- addLayer(darch,getWeights(rbm),getHiddenBiases(rbm),sigmoidUnit)				
    }
    return(darch)
  }
)
