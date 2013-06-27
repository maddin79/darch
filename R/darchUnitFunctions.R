#' Sigmoid unit function.
#' 
#' The function calculates the activation and returns the result through the
#' sigmoid transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation of the unit in the first entry.
#' 
#' @usage sigmoidUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname sigmoidUnit
#' @include darch.R
#' @export
sigmoidUnit <- function(data,weights){
  ret <- list(1./(1 + exp(-data%*%weights)))
  return(ret)
}

#' Binary sigmoid unit function.
#' 
#' The function calculates the activation and the output from the sigmoid 
#' transfer function. It returns a binary matrix where a entry is 1 if the value 
#' is bigger than a random number generated with \code{\link{runif}}.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the binary activation of the unit in the first entry.
#' 
#' @usage binSigmoidUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname binSigmoidUnit
#' @include darch.R
#' @export
binSigmoidUnit <- function(data,weights){
  sig <- 1./(1 + exp(-data%*%weights))
  rows <- nrow(data)
  cols <- ncol(weights)
  ret <- list(sig > matrix(runif(rows*cols),rows,cols))
  return(ret)
}

#' Sigmoid unit function with unit derivatives.
#' 
#' The function calculates the activation and returns a list which the first 
#' entry is the result through the sigmoid transfer function and the second 
#' entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the activation in the first entry and the derivative of
#' the transfer function in the second entry
#' 
#' @usage sigmoidUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}},
#' 
#' @docType methods
#' @rdname sigmoidUnitDerivative
#' @include darch.R
#' @export
sigmoidUnitDerivative <- function(data,weights){
  ret <- list()
  ret[[1]] <- 1./(1 + exp(-data%*%weights))
  ret[[2]] <- ret[[1]]*(1-ret[[1]])
  return(ret)
}

#' Linear unit function.
#' 
#' The function calculates the activation of the units and returns it.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return  A list with the linear activation of the unit in the first entry.
#' 
#' @usage linearUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname linearUnit
#' @include darch.R
#' @export
linearUnit <- function(data,weights){
  ret <- list(data%*%weights)
  return(ret)
}

#' Linear unit function with unit derivatives.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the linear activation of the units and the second 
#' entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the linear activation in the first entry and the 
#' derivative of the activation in the second entry
#' 
#' @usage linearUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{softmaxUnit}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname linearUnitDerivative
#' @include darch.R
#' @export
linearUnitDerivative <- function(data,weights){
  ret <- list()
  ret[[1]] <- data%*%weights
  ret[[2]] <- matrix(1,nrow(ret[[1]]),ncol(ret[[1]]))
  return(ret)
}

#' Softmax unit function.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the softmax transfer function.
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the softmax activation in the first entry
#' 
#' @usage softmaxUnit(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnitDerivative}}
#' 
#' @docType methods
#' @rdname softmaxUnit
#' @include darch.R
#' @export
softmaxUnit <- function (data, weights) {
  ret <- list()
  x <- exp(data %*% weights)
  sums <- rep(rowSums(x),ncol(weights))
  ret[[1]] <- x/matrix(sums,nrow(x))
  return(ret)
}

#' Softmax unit function with unit derivatives.
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the softmax transfer function 
#' and the second entry is the derivative of the transfer function. 
#' 
#' @param data The data matrix for the calculation
#' @param weights The weight and bias matrix for the calculation
#' @return A list with the softmax activation in the first entry and the 
#' derivative of the transfer function in the second entry
#' 
#' @usage softmaxUnitDerivative(data,weights)
#' 
#' @seealso \code{\link{DArch}},
#'          \code{\link{sigmoidUnit}},
#'          \code{\link{binSigmoidUnit}},
#'          \code{\link{sigmoidUnitDerivative}},
#'          \code{\link{linearUnit}},
#'          \code{\link{linearUnitDerivative}},
#'          \code{\link{softmaxUnit}}
#' 
#' @docType methods
#' @rdname softmaxUnitDerivative
#' @include darch.R
#' @export
softmaxUnitDerivative <- function (data, weights) {
  ret <- list()
  x <- exp(data %*% weights)
  sums <- rep(rowSums(x),ncol(weights)) 
  y <- matrix(sums,nrow(x))
  ret[[1]] <- x/y
  ret[[2]] <- ret[[1]] * (1 - ret[[1]])
  return(ret)
}


# #' Binary unit function.
# #' 
# #' Returns the binary activation of the units. 
# #' 
# #' @param data The data matrix for the calculation
# #' @param weights The weight and bias matrix for the calculation
# #' @return A list with the binary activation in the first entry
# #' 
# #' @usage binaryUnit(data,weights)
# #' 
# #' @seealso \code{\link{DArch}}
# #'          \code{\link{sigmoidUnit}}
# #'          \code{\link{binSigmoidUnit}}
# #'          \code{\link{sigmoidUnitDerivative}}
# #'          \code{\link{linearUnit}}
# #'          \code{\link{linearUnitDerivative}}
# #'          \code{\link{softmaxUnit}}
# #'          \code{\link{binaryUnit}}
# #' 
# #' @docType methods
# #' @rdname binaryUnit
# #' @include darch.R
# #' @export
# binaryUnit <- function(data,weights){
#   ret <- data%*%weights
#   ret <- ret >= 0
#   return(list(1*ret))
# }