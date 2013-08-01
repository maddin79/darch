setOldClass(c("logger","ff_matrix","ff_array"))

#' Abtract class for neural networks.
#' 
#' This is a abstract class for neural networks. It provides some 
#' functionalitys used in more than one network type.   
#' 
#' @section Slot: 
#' \describe{
#'   \item{\code{batchSize}:}{Object of class \code{"numeric"}. The batch size 
#'   for the training and test data during the learning.}
#'   \item{\code{errorFunction}:}{Object of class \code{"function"}. Function 
#'   for error calculation.}
#'   \item{\code{ff}:}{Object of class \code{"logical"}. Indicates if the 
#'   package \code{\link[ff]{ff}} is used to save the network data.}
#'   \item{\code{genWeightFunction}:}{Object of class \code{"function"}. A 
#'   function for generate random initialised weight matrix. }
#'   \item{\code{logger}:}{Object of class \code{"logger"}. The log4r object to 
#'   show the log messages.}
#' }
#' 
#' @exportClass Net
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{RBM}}
#' @author Martin Drees
#' @name Net 
#' @rdname Net
#' @aliases Net-class

setClass(
  Class="Net",
  representation=representation(
    batchSize = "numeric",
    errorFunction = "function",
    ff = "logical",
    genWeightFunction = "function",
    logger = "logger",
    learnRateWeights = "numeric",
    finalMomentum = "numeric",
    momentum = "numeric",
    momentumSwitch = "numeric",				
    stats = "list"
  )
)


