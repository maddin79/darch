#' Class for deep architectures
#' 
#' To the constructor function: \code{\link{newDArch}}
#' TODO: Description ...
#' 
#' TODO: Details ...
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{rbmList}:}{Object of class \code{"list"}. Contains all rbm's 
#'   for the pre-training.}
#'   \item{\code{layers}:}{Object of class \code{"list"}. In the first field 
#'   are the weights and in the second field is the unit function. }
#'   \item{\code{learnRateBiases}:}{Object of class \code{"list"}. Contains 
#'   }
#'   \item{\code{fineTuneFunction}:}{Object of class \code{"function"}. }
#'   \item{\code{executeFunction}:}{Object of class \code{"list"}. }
#'   \item{\code{executeOutput}:}{Object of class \code{"list"}. }
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
