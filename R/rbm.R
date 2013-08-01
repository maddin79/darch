#' Class for Restricted-Bolzmann-Machine
#' 
#' This class represents a Restricted-Bolzmann-Machine
#' 
#' @Details
#' For generating a RBM it is recommended to use the constructor function: 
#' \code{\link{newRBM}}. The RBM can be trained with the implementation of the 
#' contrastive divergence method \code{\link{trainRBM}}. 
#' The class inherits the attributes from the \code{\linkS4class{Net}}. The 
#' if the attribute ff is \code{TRUE}, the attributes with the ff-prefix are 
#' used to save the parameters of the RBM network. It is recommended to use the
#' setter and getter method for access the attributes, because then there is no 
#' need to request the ff attribute to access the right attribute.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{learnRateBiasVisible}:}{Object of class \code{"numeric"}.
#'   Learning rate of the visible biases.}
#'   \item{\code{learnRateBiasHidden}:}{Object of class \code{"numeric"}.
#'   Learning rate of the hidden biases.}
#'   \item{\code{weightCost}:}{Object of class \code{"numeric"}.
#'   Weigth cost for the update of the weigths.}
#'   \item{\code{numHidden}:}{Object of class \code{"numeric"}.
#'   Number of hidden units.}
#'   \item{\code{numVisible}:}{Object of class \code{"numeric"}.
#'   Number of visible units.}
#'   \item{\code{weights}:}{Object of class \code{"matrix"}.
#'   Weight matrix.}   
#'   \item{\code{weightInc}:}{Object of class \code{"matrix"}.
#'   Matrix of update values for the Weight.}
#'   \item{\code{output}:}{Object of class \code{"matrix"}.
#'   Output matrix of the RBM.}
#'   \item{\code{visibleBiases}:}{Object of class \code{"array"}.
#'   Visible biases array.}
#'   \item{\code{visibleBiasesInc}:}{Object of class \code{"array"}.
#'   Array of update values for the visible biases}
#'   \item{\code{visibleUnitFunction}:}{Object of class \code{"function"}.
#'   Unit function for the visible units.}
#'   \item{\code{visibleUnitStates}:}{Object of class \code{"list"}.
#'   States of the visible units.}
#'   \item{\code{hiddenBiases}:}{Object of class \code{"array"}.
#'   Hidden biases array.}
#'   \item{\code{hiddenBiasesInc}:}{Object of class \code{"array"}.
#'   Array of update values for the hidden biases.}
#'   \item{\code{hiddenUnitFunction}:}{Object of class \code{"function"}.
#'   Unit function for the hidden units.}
#'   \item{\code{hiddenUnitStates}:}{Object of class \code{"list"}.
#'   States of the hidden units.}
#'   \item{\code{updateFunction}:}{Object of class \code{"function"}.
#'   Function for updating the weights and biases.}
#'   \item{\code{posPhaseData}:}{Object of class \code{"list"}.
#'   Attribute to save the positive phase data during the training.}
#'   \item{\code{ffWeights}:}{Object of class \code{"ff_matrix"}.
#'   Weight ff matrix. Used when the ff attribute is \code{TRUE}.}
#'   \item{\code{ffOutput}:}{Object of class \code{"ff_matrix"}.
#'   Output ff matrix of the RBM. Used when the ff attribute is \code{TRUE}.}
#'   \item{\code{ffHiddenBiases}:}{Object of class \code{"ff_array"}.
#'   Hidden biases ff array. Used when the ff attribute is \code{TRUE}.}
#'   \item{\code{ffVisibleBiases}:}{Object of class \code{"ff_array"}.
#'   Hidden biases ff array. Used when the ff attribute is \code{TRUE}.}
#' }
#' 
#' @include net.R
#' @exportClass RBM
#' @seealso \code{\linkS4class{Net}}, 
#' \code{\linkS4class{DArch}},
#' \code{\link{trainRBM}}
#' @author Martin Drees
#' @name RBM 
#' @rdname RBM
#' @aliases RBM-class
setClass(
		Class="RBM",
		representation=representation(
				learnRateBiasVisible = "numeric",
				learnRateBiasHidden = "numeric",
				weightCost = "numeric",
				numHidden = "numeric",
				numVisible = "numeric",
				weights = "matrix",
				weightInc = "matrix",
				output = "matrix",
				visibleBiases = "array",
				visibleBiasesInc = "array",
				visibleUnitFunction = "function",
				visibleUnitStates = "list",
				hiddenBiases = "array",
				hiddenBiasesInc = "array",
				hiddenUnitFunction = "function",
				hiddenUnitStates = "list",
				updateFunction = "function",
				posPhaseData = "list",
				
				# ff slots
				ffWeights = "ff_matrix",
				ffOutput = "ff_matrix",
				ffHiddenBiases = "ff_array",
				ffVisibleBiases = "ff_array"
		),
		contains="Net"
)

setMethod ("initialize","RBM",
		function(.Object){
			
			.Object@learnRateWeights <- 0.1
			.Object@learnRateBiasVisible <- 0.1
			.Object@learnRateBiasHidden <- 0.1
			.Object@weightCost <- 0.0002
			.Object@momentum <- 0.5
			.Object@finalMomentum <- 0.9
			.Object@momentumSwitch <- 5
			.Object@visibleUnitFunction <- sigmUnitFunc
			.Object@hiddenUnitFunction <- sigmUnitFuncSwitch
			.Object@updateFunction <- rbmUpdate
			.Object@errorFunction <- mseError
			.Object@genWeightFunction <- generateWeights
			return(.Object)    
		}
)
