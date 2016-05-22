# Copyright (C) 2013-2016 Martin Drees
# Copyright (C) 2015-2016 Johannes Rueckert
#
# This file is part of darch.
#
# darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch. If not, see <http://www.gnu.org/licenses/>.

#' @include darch.Class.R
NULL

#' Sigmoid unit function with unit derivatives.
#' 
#' The function calculates the activation and returns a list which the first
#' entry is the result through the sigmoid transfer function and the second
#' entry is the derivative of the transfer function.
#' 
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.unitFunction = "sigmoidUnit")
#' }
#' @family darch unit functions
#' @export
sigmoidUnit <- function(input, ...)
{
  sigmoidUnitCpp(input)
}

#' Continuous Tan-Sigmoid unit function.
#' 
#' Calculates the unit activations and returns them in a list.
#' 
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the activation in the first entry and the derivative of
#'   the transfer function in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.unitFunction = "tanhUnit")
#' }
#' @export
tanhUnit <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- tanh(input)
  ret[[2]] <- 1 - ret[[1]] ^ 2
  ret
}

#' Linear unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the linear activation of the units and the second
#' entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the linear activation in the first entry and the
#' derivative of the activation in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.unitFunction = "linearUnit")
#' }
#' @export
linearUnit <- function(input, ...)
{
  ret <- list()
  ret[[1]] <- input
  ret[[2]] <- matrix(1, nrow(ret[[1]]), ncol(ret[[1]]))
  ret
}

#' Softmax unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the result through the softmax transfer function
#' and the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the softmax activation in the first entry and the
#' derivative of the transfer function in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris,
#'  darch.unitFunction = c("sigmoidUnit", "softmaxUnit"))
#' }
#' @references http://www.faqs.org/faqs/ai-faq/neural-nets/part2/section-12.html
#' @export
softmaxUnit <- function(input, ...)
{
  softmaxUnitCpp(input)
}

#' Maxout / LWTA unit function
#' 
#' The function calculates the activation of the units and returns a list, in 
#' which the first entry is the result through the maxout transfer function and 
#' the second entry is the derivative of the transfer function.
#' 
#' Maxout sets the activations of all neurons but the one with the highest
#' activation within a pool to \code{0}. If this is used without
#' \link{maxoutWeightUpdate}, it becomes the local-winner-takes-all algorithm,
#' as the only difference between the two is that outgoing weights are shared
#' for maxout.
#' 
#' @param input Input for the activation function.
#' @param ... Additional parameters, passed on to inner unit function.
#' @param poolSize The size of each maxout pool.
#' @param unitFunc Inner unit function for maxout.
#' @param dropoutMask Vector containing the dropout mask.
#' @return A list with the maxout activation in the first entry and the 
#'   derivative of the transfer function in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' # LWTA:
#' model <- darch(Species ~ ., iris, c(0, 50, 0),
#'  darch.unitFunction = c("maxoutUnit", "softmaxUnit"),
#'  darch.maxout.poolSize = 5, darch.maxout.unitFunction = "sigmoidUnit")
#' # Maxout:
#' model <- darch(Species ~ ., iris, c(0, 50, 0),
#'  darch.unitFunction = c("maxoutUnit", "softmaxUnit"),
#'  darch.maxout.poolSize = 5, darch.maxout.unitFunction = "sigmoidUnit",
#'  darch.weightUpdateFunction = c("weightDecayWeightUpdate", "maxoutWeightUpdate"))
#' }
#' @references
#'  Srivastava, Rupesh Kumar, Jonathan Masci, Sohrob Kazerounian,
#'  Faustino Gomez, and Juergen Schmidhuber (2013). "Compete to Compute". In:
#'  Advances in Neural Information Processing Systems 26. Ed. by C.J.C. Burges,
#'  L. Bottou, M. Welling, Z. Ghahramani, and K.Q. Weinberger.
#'  Curran Associates, Inc., pp. 2310-2318.
#'  URL: http://papers.nips.cc/paper/5059-compete-to-compute.pdf
#'  
#'  Goodfellow, Ian J., David Warde-Farley, Mehdi Mirza, Aaron C. Courville,
#'  and Yoshua Bengio (2013). "Maxout Networks". In: Proceedings of the 30th
#'  International Conference on Machine Learning, ICML 2013, Atlanta, GA, USA,
#'  16-21 June 2013, pp. 1319-1327.
#'  URL: http://jmlr.org/proceedings/papers/v28/goodfellow13.html
#' @export
maxoutUnit <- function(input, ..., poolSize =
  getParameter(".darch.maxout.poolSize", 2, ...), unitFunc =
  getParameter(".darch.maxout.unitFunction", linearUnit, ...),
  dropoutMask = vector())
{  
  ret <- unitFunc(input, ...)
  
  ncols <- ncol(ret[[1]])
  
  # Abort if number of neurons in the current layer invalid
  # TODO check once in the beginning of the training
  if (ncols %% poolSize != 0)
  {
    stop(futile.logger::flog.error(paste("Number of neurons in the current",
      "layer not divisible by pool size (%d %% %d)"), ncols, poolSize))
  }
  
  maxoutUnitCpp(ret[[1]], ret[[2]], poolSize, dropoutMask)
  
  ret
}

#' Rectified linear unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the rectified linear activation of the units and
#' the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the rectified linear activation in the first entry and
#'  the derivative of the activation in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.unitFunction = "rectifiedLinearUnit")
#' }
#' @references Glorot, Xavier, Antoine Bordes, and Yoshua Bengio (2011). "Deep
#'  Sparse Rectifier Neural Networks". In: Proceedings of the Fourteenth
#'  International Conference on Artificial Intelligence and Statistics
#'  (AISTATS-11). Ed. by Geoffrey J. Gordon and David B. Dunson. Vol. 15.
#'  Journal of Machine Learning Research - Workshop and Conference Proceedings,
#'  pp. 315-323.
#'  URL : http://www.jmlr.org/proceedings/papers/v15/glorot11a/glorot11a.pdf
#' @export
rectifiedLinearUnit <- function(input, ...)
{
  rectifiedLinearUnitCpp(input)
}

#' Exponential linear unit (ELU) function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the exponential linear activation of the units and
#' the second entry is the derivative of the transfer function.
#'
#' @param input Input for the activation function.
#' @param alpha ELU hyperparameter.
#' @param ... Additional parameters.
#' @return A list with the ELU activation in the first entry and
#'  the derivative of the activation in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.unitFunction = "exponentialLinearUnit",
#'  darch.elu.alpha = 2)
#' }
#' @references Clevert, Djork-Arne, Thomas Unterthiner, and Sepp Hochreiter
#'  (2015). "Fast and Accurate Deep Network Learning by Exponential Linear Units
#'  (ELUs)". In: CoRR abs/1511.07289. URL : http://arxiv.org/abs/1511.07289
#' @export
exponentialLinearUnit <- function(input, alpha =
  getParameter(".darch.elu.alpha", 1, ...), ...)
{
  exponentialLinearUnitCpp(input, alpha)
}

#' Softplus unit function with unit derivatives.
#'
#' The function calculates the activation of the units and returns a list, in
#' which the first entry is the softmax activation of the units and the second
#' entry is the derivative of the transfer function. Softplus is a smoothed
#' version of the rectified linear activation function.
#'
#' @param input Input for the activation function.
#' @param ... Additional parameters, not used.
#' @return A list with the softplus activation in the first entry and
#'  the derivative of the activation in the second entry.
#' @family darch unit functions
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.unitFunction = "softplusUnit")
#' }
#' @references Dugas, Charles, Yoshua Bengio, Francois Belisle, Claude Nadeau,
#'  and Rene Garcia (2001). "Incorporating Second-Order Functional Knowledge for
#'  Better Option Pricing". In: Advances in Neural Information Processing
#'  Systems, pp. 472-478.
#' @export
softplusUnit <- function(input, ...)
{
  softplusUnitCpp(input)
}
