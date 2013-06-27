#' Class for Restricted-Bolzmann-Machine
#' 
#' To the constructor function: \code{\link{newRBM}}
#' TODO: Description ...
#' 
#' TODO: Details ...
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{rbmList}:}{Object of class \code{"list"}. Contains all rbm's 
#'   for the pre-training.}
#' }
#' 
#' @include net.R
#' @exportClass RBM
#' @seealso \code{\linkS4class{Net}}, \code{\linkS4class{DArch}}
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
