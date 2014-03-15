#' Class for deep architectures
#' 
#' This class implements deep architectures and provides the ability to train 
#' them with a pre training using contrastive divergence and fine tuning with 
#' backpropagation, resilient backpropagation and conjugate gradients.
#' 
#' @details 
#' The class is inherits all attributes from the class \code{link{Net}}. 
#' When creating a new instance with the constructor \code{\link{newDArch}} 
#' (recommended), the darch-object contained the number of layers -1 restricted 
#' bolzmann machines (\code{\link{RBM}}), which are used for the unsupervised 
#' pre training of the network. The \code{\link{RBM}}s are saved in the 
#' attribute \code{rbmList} and can be fetched over the getter method (\code{\link{getRBMList}}. 
#' The two attributes \code{fineTuneFunction} and \code{executeFunction} 
#' containing the functions for the fine tuning (default: \code{\link{backpropagation}})
#' and for the execution (default: \code{\link{runDArch}}. The training of the 
#' network is performed by the two learning functions \code{\link{preTrainDArch}} 
#' and \code{\link{fineTuneDArch}}. The first function trains the network with 
#' the unsupervised method contrastive divergence. The second function used the 
#' function in the attribute \code{fineTuneFunction} for the fine tuning. After 
#' an execution of the network, the outputs of every layer can be found in the 
#' attribute \code{executeOutput}. 
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{rbmList}:}{A list which contains all rbm's for the pre-training.}
#'   \item{\code{layers}:}{A list with the layer information. In the first field 
#'   are the weights and in the second field is the unit function. }
#'   \item{\code{learnRateBiases}:}{The learning rate for the bias weights.}
#'   \item{\code{fineTuneFunction}:}{Contains the function for the fine tuning.}
#'   \item{\code{executeFunction}:}{Contains the function for executing the network.}
#'   \item{\code{executeOutput}:}{A list which contains the outputs of every layer 
#'   after an execution of the network.}
#'   \item{\code{cancel}:}{Boolean value which indicates if the network training 
#'   is canceled.}
#'   \item{\code{executeOutput}:}{A string containing the message why the network 
#'   training is stopped.}
#'   \item{\code{cancel}:}{Indicates if the execution is canceled.}
#'   \item{\code{cancelMessage}:}{The message when the execution is canceled.}
#' }
#' 
#' @include net.R
#' @exportClass DArch
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{RBM}}
#' @author Martin Drees
#' @name DArch 
#' @rdname DArch
#' @aliases DArch-class
setClass(
  Class="DArch",
  representation=representation(
    rbmList = "list",
    layers = "list",
    learnRateBiases = 'numeric',
    fineTuneFunction = "function",
    executeFunction = "function",
    executeOutput = "list",
    cancel = "logical",
    cancelMessage = "character"
  ),
  contains="Net"
)

setMethod ("initialize","DArch",
           function(.Object){	
             .Object@executeFunction <- runDArch
             .Object@genWeightFunction <- generateWeights
             .Object@fineTuneFunction <- backpropagation
             .Object@momentum <-0.5
             .Object@finalMomentum <- 0.9
             .Object@momentumSwitch <- 5
             .Object@learnRateBiases <- 0.001
             .Object@learnRateWeights <- 0.001
             .Object@errorFunction <- mseError
             .Object@cancel <- FALSE
             .Object@cancelMessage <- "No reason specified."
             return(.Object)    
           }
)
