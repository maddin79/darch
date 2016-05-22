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

# TODO documentation on weight generation functions in general (minimum params)

#' Generates a weight matrix using \link{rnorm}. 
#' 
#' This function is the standard method for generating weights for instances of
#' \code{\linkS4class{Net}}. It uses \code{\link{rnorm}} to do so.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param weights.mean \code{mean} parameter to the \link{rnorm} function.
#' @param weights.sd \code{sd} parameter to the \link{rnorm} function.
#' @param ... Additional parameters, used for parameter resolution.
#' @return Weight matrix.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsNormal",
#'  weights.mean = .1, weights.sd = .05)
#' }
#' @family weight generation functions
#' @export
generateWeightsNormal <- function(numUnits1,numUnits2,
  weights.mean = getParameter(".weights.mean", 0, ...),
  weights.sd = getParameter(".weights.sd", .01, ...), ...)
{
  matrix(rnorm(numUnits1*numUnits2, weights.mean, weights.sd),
                nrow = numUnits1, ncol = numUnits2)
}

#' Generates a weight matrix using \link{runif}
#' 
#' This function is used to generate random weights and biases using
#' \link{runif}.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param weights.min \code{min} parameter to the \link{runif} function.
#' @param weights.max \code{max} parameter to the \link{runif} function.
#' @param ... Additional parameters, used for parameter resolution.
#' @return Weight matrix.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsUniform",
#'  weights.min = -.1, weights.max = .5)
#' }
#' @family weight generation functions
#' @export
generateWeightsUniform <- function(numUnits1,numUnits2,
  weights.min = getParameter(".weights.min", -.1, ...),
  weights.max = getParameter(".weights.max", .1, ...), ...)
{
  matrix(runif(numUnits1*numUnits2, weights.min, weights.max),
         nrow = numUnits1, ncol = numUnits2)
}

#' Glorot normal weight initialization
#' 
#' This function is used to generate random weights and biases using
#' Glorot normal weight initialization as described in
#' Glorot & Bengio, AISTATS 2010.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param weights.mean \code{mean} parameter to the \link{rnorm} function.
#' @param ... Additional parameters, used for parameter resolution and passed
#'   to \code{\link{generateWeightsNormal}}.
#' @return Weight matrix.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsGlorotNormal",
#'  weights.mean = .1)
#' }
#' @references Glorot, Xavier and Yoshua Bengio (2010). "Understanding the
#'  difficulty of training deep feedforward neural networks".
#'  In: International conference on artificial intelligence and statistics, pp. 249-256
#' @family weight generation functions
#' @export
generateWeightsGlorotNormal <- function(numUnits1, numUnits2,
  weights.mean = getParameter(".weights.mean", 0, ...), ...)
{
  scale <- sqrt(2. / (numUnits1 + numUnits2))
  generateWeightsNormal(numUnits1, numUnits2, weights.mean, scale, ...)
}

#' Glorot uniform weight initialization
#' 
#' This function is used to generate random weights and biases using
#' Glorot uniform weight initialization as described in
#' Glorot & Bengio, AISTATS 2010.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param ... Additional parameters, used for parameter resolution and passed
#'   to \code{\link{generateWeightsUniform}}.
#' @return Weight matrix.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsGlorotUniform")
#' }
#' @references Glorot, Xavier and Yoshua Bengio (2010). "Understanding the
#'  difficulty of training deep feedforward neural networks".
#'  In: International conference on artificial intelligence and statistics, pp. 249-256
#' @family weight generation functions
#' @export
generateWeightsGlorotUniform <- function(numUnits1, numUnits2, ...)
{
  scale <- sqrt(6. / (numUnits1 + numUnits2))
  generateWeightsUniform(numUnits1, numUnits2, weights.min = -scale,
                        weights.max = scale, ...)
}

#' He normal weight initialization
#' 
#' This function is used to generate random weights and biases using
#' He normal weight initialization as described in
#' He et al., \url{http://arxiv.org/abs/1502.01852}.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param weights.mean \code{mean} parameter to the \link{rnorm} function.
#' @param ... Additional parameters, used for parameter resolution and passed
#'   to \code{\link{generateWeightsNormal}}.
#' @return Weight matrix.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsHeNormal",
#'  weights.mean = .1)
#' }
#' @references He, Kaiming, Xiangyu Zhang, Shaoqing Ren, and Jian Sun (2015).
#'  "Delving Deep into Rectifiers: Surpassing Human-Level Performance on
#'  ImageNet Classification". In: CoRR abs/1502.01852.
#'  URL: http://arxiv.org/abs/1502.01852
#' @family weight generation functions
#' @export
generateWeightsHeNormal <- function(numUnits1, numUnits2,
  weights.mean = getParameter(".weights.mean", 0, ...), ...)
{
  scale <- sqrt(2. / numUnits1)
  generateWeightsNormal(numUnits1, numUnits2, weights.mean, scale, ...)
}

#' He uniform weight initialization
#' 
#' This function is used to generate random weights and biases using
#' He uniform weight initialization as described in
#' He et al., \url{http://arxiv.org/abs/1502.01852}.
#' 
#' @param numUnits1 Number of units in the lower layer.
#' @param numUnits2 Number of units in the upper layer.
#' @param ... Additional parameters, used for parameter resolution and passed
#'   to \code{\link{generateWeightsUniform}}.
#' @return Weight matrix.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsHeUniform")
#' }
#' @references He, Kaiming, Xiangyu Zhang, Shaoqing Ren, and Jian Sun (2015).
#'  "Delving Deep into Rectifiers: Surpassing Human-Level Performance on
#'  ImageNet Classification". In: CoRR abs/1502.01852.
#'  URL: http://arxiv.org/abs/1502.01852
#' @family weight generation functions
#' @export
generateWeightsHeUniform <- function(numUnits1, numUnits2, ...)
{
  scale <- sqrt(6. / numUnits1)
  generateWeightsUniform(numUnits1, numUnits2, weights.min = -scale,
                         weights.max = scale, ...)
}