#' Quadratic error function
#' 
#' The function calculates the quadradic error from the \code{original} and 
#' \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' 
#' @usage quadraticError(original, estimate)
#' 
#' @seealso \code{\link{mseError}},
#'          \code{\link{crossEntropyError}}
#'  
#' @docType methods
#' @rdname quadraticError
#' @export
quadraticError <- function(original, estimate){
    ret <- list("Quadratic-Error",sum((original[] - estimate[])^2))
  return(ret)
}

#' Mean quared error function
#' 
#' The function calculates the mean quared error (MSE) from the \code{original} 
#' and \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' 
#' @usage mseError(original, estimate)
#' 
#' @seealso \code{\link{quadraticError}},
#'          \code{\link{crossEntropyError}}
#'  
#' @docType methods
#' @rdname mseError
#' @export
mseError <- function(original, estimate){
  if(is.null(dim(original[]))){
    mFunc <- mean
  }else{
    mFunc <- colMeans
  }
  ret <- list("Mean-Sqared-Error",sum(mFunc((original[] - estimate[])^2)))
  return(ret)
}

#' Cross entropy error function
#' 
#' The function calculates the cross entropy error from the \code{original} and 
#' \code{estimate} parameters.
#' 
#' @param original The original data matrix
#' @param estimate The calculated data matrix
#' @return A list with the name of the error function in the first entry and the
#' error value in the second entry
#' 
#' @usage crossEntropyError(original, estimate)
#' 
#' @seealso \code{\link{quadraticError}},
#'          \code{\link{mseError}}
#'  
#' @docType methods
#' @rdname crossEntropyError
#' @export
crossEntropyError <- function(original, estimate){
  # C = - sum [all cases and outputs] (d*log(y) + (1-d)*log(1-y) )
  c <- -sum(original[]*log(estimate[]) + (1-original[])*log(1-estimate[]))
  ret <- list("Cross-Entropy-Error",c)
  return(ret)
}