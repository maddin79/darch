# Copyright (C) 2016 Johannes Rueckert
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

#' Creates a custom caret model for \code{darch}.
#' 
#' This function creates a \code{caret} model description to enable training
#' \code{DArch} instances with the \code{\link[caret]{train}} function. See the
#' documentation on custom caret models for further information and examples on
#' how to create valid \code{params} and \code{grid} values.
#'
#' @seealso \href{https://topepo.github.io/caret/custom_models.html}{Caret custom models}
#' @param params \code{\link{data.frame}} of parameters or \code{NULL} to use a
#'   simple default (bp.learnRate).
#' @param grid Function which procuces a \code{\link{data.frame}} containing a
#'   grid of parameter combinations or \code{NULL} to use a simple default.
#' @return A valid \code{caret}  model which can be passed to
#'   \code{\link[caret]{train}}.
#' @examples
#' \dontrun{
#' data(iris)
#' tc <- trainControl(method = "boot", number = 5, allowParallel = F,
#'   verboseIter = T)
#' 
#' parameters <- data.frame(parameter = c("layers", "bp.learnRate", "darch.unitFunction"),
#'   class = c("character", "numeric", "character"),
#'   label = c("Network structure", "Learning rate", "unitFunction"))
#' 
#' grid <- function(x, y, len = NULL, search = "grid")
#' {
#'   df <- expand.grid(layers = c("c(0,20,0)","c(0,10,10,0)","c(0,10,5,5,0)"),
#'    bp.learnRate = c(1,2,5,10))
#'   
#'   df[["darch.unitFunction"]] <- rep(c("c(tanhUnit, softmaxUnit)",
#'    "c(tanhUnit, tanhUnit, softmaxUnit)",
#'    "c(tanhUnit, tanhUnit, tanhUnit, softmaxUnit)"), 4)
#'   
#'   df
#' }
#' 
#' caretModel <- train(Species ~ ., data = iris, tuneLength = 12, trControl = tc,
#'   method = darchModelInfo(parameters, grid), preProc = c("center", "scale"),
#'   darch.numEpochs = 15, darch.batchSize = 6, testing = T, ...)
#' }
#' @export
darchModelInfo <- function(params = NULL, grid = NULL)
{
  r <- list(type = c("Classification", "Regression"),
            library = "darch")
            #loop = NULL)
  
  if (is.null(params))
  {
    params <- data.frame(parameter = c("bp.learnRate"),
                         class = c("numeric"),
                         label = c("Learn rate"))
  }
  
  r$parameters <- params
  
  if (is.null(grid))
  {
    grid <- function(x, y, len = NULL, search = "grid")
    {
      if (search == "grid")
      {
        out <- data.frame(darch.learnRate = seq.int(.1,5,.1)[1:len])
      }
      else
      {
        out <- data.frame(darch.learnRate = runif(len, .01, 2))
      }
      
      out
    }
  }
  
  r$grid <- grid
  
  r$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...)
  {
    darch(x, y, paramsList = as.list(param), darch.isClass = !classProbs, ...)
  }
  
  r$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  {
    # TODO switch between class and raw when not dealing with classification?
    predict(modelFit, newdata = newdata, type = "character")
  }
  
  r$prob <- function(darch, newdata, preProc = NULL, submodels = NULL)
  {
    predict(darch, newdata = newdata, type = "raw")
  }
  
  r$levels <- function(darch)
  {
    if (is.null(darch@dataSet@parameters$dummyVarsTargets$lvls)) NULL else
      darch@dataSet@parameters$dummyVarsTargets$lvls[[1]]
  }
  
  r
}

#' Wrapper for \code{\link[caret]{contr.ltfr}}
#' 
#' Simply redirects the call to \code{\link[caret]{contr.ltfr}}, this is done
#' to avoid errors when the caret namespace is not attached.
#' 
#' @param ... \code{\link[caret]{contr.ltfr}} parameters.
#' @export
#' @keywords internal
contr.ltfr <- function(...)
{
  caret::contr.ltfr(...)
}