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

# TODO further parameters like na.action etc.

#' Forward-propagate data.
#' 
#' Forward-propagate given data through the deep neural network.
#' 
#' @param object \code{\linkS4class{DArch}} instance
#' @param ... Further parameters, if \code{newdata} is \code{NULL}, the first
#'   unnamed parameter will be used for \code{newdata} instead.
#' @param newdata New data to predict, \code{NULL} to return latest network
#'   output
#' @param inputLayer Layer number (\code{> 0}). The data given in
#'   \code{newdata} will be fed into this layer.
#'   Note that absolute numbers count from the input layer, i.e. for
#'   a network with three layers, \code{1} would indicate the input layer.
#' @param outputLayer Layer number (if \code{> 0}) or offset (if \code{<= 0})
#'   relative to the last layer. The output of the given layer is returned.
#'   Note that absolute numbers count from the input layer, i.e. for
#'   a network with three layers, \code{1} would indicate the input layer.
#' @param type Output type, one of: \code{raw}, \code{bin}, \code{class}, or
#'   \code{character}. \code{raw} returns the layer output, \code{bin} returns
#'   \code{1} for every layer output \code{>0.5}, \code{0} otherwise, and
#'   \code{class} returns \code{1} for the output unit with the highest
#'   activation, otherwise \code{0}. Additionally, when using \code{class},
#'   class labels are returned when available. \code{character} is the same as
#'   \code{class}, except using character vectors instead of factors.
#' @return Vector or matrix of networks outputs, output type depending on the 
#'   \code{type} parameter.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, retainData = T)
#' predict(model)
#' }
#' @family darch interface functions
#' @export
predict.DArch <- function(object, ..., newdata = NULL,
  type = "raw", inputLayer = 1, outputLayer = 0)
{
  oldLogLevel <- futile.logger::flog.threshold()
  on.exit(futile.logger::flog.threshold(oldLogLevel))
  setLogLevel(futile.logger::WARN)
  
  darch <- object
  numLayers <- length(darch@layers)
  additionalParams = list(...)
  
  if (length(additionalParams) > 0 &&
      (is.null(names(additionalParams)) || "" %in% names(list(...))) &&
      is.null(newdata))
  {
    newdata <- list(...)[[1]]
  }
  
  if (is.null(newdata))
  {
    if (!getParameter(".retainData") || inputLayer > 1)
    {
      stop(futile.logger::flog.error("No data available for prediction"))
    }
    
    newdata <- darch@dataSet@data
  }
  else if (inputLayer == 1)
  {
    dataSet <- createDataSet(data = newdata,
      targets = if (is.null(darch@dataSet@formula)) NULL else F,
      dataSet = darch@dataSet)
    newdata <- dataSet@data
  }
  
  outputLayer <- (if (outputLayer <= 0) max(numLayers + outputLayer, 0)
    else min(outputLayer - 1, numLayers))
  
  if (inputLayer > outputLayer && outputLayer > 0)
  {
    stop(futile.logger::flog.error(paste("Invalid combination of parameters",
      "for inputLayer (%s) and outputLayer (%s)"), inputLayer, outputLayer))
  }
  
  execOut <- getParameter(".darch.executeFunction")(darch, newdata,
    inputLayer = inputLayer, outputLayer = outputLayer)[,, drop = T]
  
  if (inherits(darch@dataSet@parameters$preProcessTargets, "preProcess") &&
        (outputLayer == 0 || outputLayer >= length(darch@layers)))
  {
    if (!is.null(dataSet@parameters$preProcessTargets$std))
    {
      if (!is.null(dim(execOut)))
      {
        execOut[, attr(dataSet@parameters$preProcessTargets$std, "names")] <-
          t(t(execOut[, attr(dataSet@parameters$preProcessTargets$std,
          "names")]) * dataSet@parameters$preProcessTargets$std
          + dataSet@parameters$preProcessTargets$mean)
      }
      else
      {
        execOut <- execOut * dataSet@parameters$preProcessTargets$std +
          dataSet@parameters$preProcessTargets$mean
      }
    }
  }
  
  ret <- switch(type, raw = execOut, bin = (execOut > .5)*1,
    class = ,
    character =
    {
      if (outputLayer != 0 && outputLayer != length(darch@layers))
      {
        stop(futile.logger::flog.error(paste("Only \"raw\" or \"bin\" output",
          "types supported when not using last layer as output layer.")))
      }
      
      if (is.null(dataSet@parameters$preProcess) ||
        is.null(dataSet@parameters$dummyVarsTargets$lvls))
      {
        if (!is.null(dim(execOut)))
        {
          ret <- diag(ncol(execOut))[max.col(execOut, ties.method = "first"),]
        }
        else
        {
          ret <- (execOut > .5) * 1
        }
      }
      else
      {
        if (!is.null(dim(execOut)))
        {
          if (length(dataSet@parameters$dummyVarsTargets$vars) > 1)
          {
            stop(futile.logger::flog.error(paste("Prediction using multiple",
              "output variables not yet supported, use \"raw\" or \"bin\"",
              "output type.")))
          }
          
          ret <- dataSet@parameters$dummyVarsTargets$lvls[[1]][max.col(execOut,
            ties.method = "first")]
          
          # convert to factor
          if (type == "class")
          {
            ret <- factor(ret,
              levels = dataSet@parameters$dummyVarsTargets$lvls[[1]])
          }
        }
        else
        {
          ret <- dataSet@parameters$ylevels[1 + (execOut > .5)]
        }
        
        ret
      }
    }, stop(futile.logger::flog.error("Invalid type argument \"%s\"", type)))
  
  ret
}