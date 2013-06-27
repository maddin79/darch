#' Generates a weight matrix. 
#' 
#' This function is the standard method for generating weights for instances of
#' \code{\link{Net}}. When using another function to generate weights, the 
#' function must be like this one.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @docType methods
#' @rdname generateWeights
#' @export
generateWeights <- function(numUnits1,numUnits2){
  ret <- matrix(rnorm(numUnits1*numUnits2)*0.1,numUnits1,numUnits2) #matrix(0.02,numUnits1,numUnits2)
  return(ret)
}