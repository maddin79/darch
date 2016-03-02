# Copyright (C) 2013-2016 Martin Drees
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
#' @param ... Further parameters, not used.
#' @param newdata New data to predict, \code{NULL} to return latest network
#'   output
#' @param outputLayer Layer number (if \code{> 0}) or offset (if \code{<= 0})
#'   relative to the last layer. The output of the given layer is returned.
#'   Note that absolute numbers count from the first non-input layer, i.e. for
#'   a network with three layers, \code{1} would indicate the hidden layer.
#' @param type Output type, one of: \code{raw}, \code{bin}, \code{class}, or
#'   \code{character}. \code{raw} returns the layer output, \code{bin} returns
#'   \code{1} for every layer output \code{>0.5}, \code{0} otherwise, and
#'   \code{class} returns \code{1} for the output unit with the highest
#'   activation, otherwise \code{0}. Additionally, when using \code{class},
#'   class labels are returned when available. \code{character} is the same as
#'   \code{class}, except using character vectors instead of factors.
#' @return Vector or matrix of networks outputs, output type depending on the 
#'   \code{type} parameter
#' @family darch interface functions
#' @export
predict.DArch <- function (object, ..., newdata = NULL, type = "raw",
                           outputLayer = 0)
{
  oldLogLevel <- futile.logger::flog.threshold()
  on.exit(futile.logger::flog.threshold(oldLogLevel))
  setLogLevel(futile.logger::WARN)
  
  darch <- object
  
  if (is.null(newdata))
  {
    if (!getDarchParam("darch.retainData"))
    {
      stop(futile.logger::flog.error("No data available for prediction"))
    }
    
    dataSet <- darch@dataSet
  }
  else
  {
    dataSet <- createDataSet(data = newdata,
      targets = if (is.null(darch@dataSet@formula)) NULL else F,
      dataSet = darch@dataSet)
  }
  
  execOut <- darch@executeFunction(darch, dataSet@data,
    outputLayer = outputLayer)[,, drop=T]
  
  if (inherits(dataSet@parameters$preProcessTargets, "preProcess") &&
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
    class=,
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
          ret <- diag(ncol(execOut))[max.col(execOut, ties.method="first"),]
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
            ties.method="first")]
          
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