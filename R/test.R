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

#' Test classification network.
#' 
#' Forward-propagate given data through the deep neural network and return
#' classification accuracy using the given labels.
#' 
#' This is primarily a convenience function similar to \link{predict.DArch} with
#' classification performance measurements instead of network output,
#' and it returns a list of accuracy indicators (raw network error, percentage
#' of incorrect classifications and absolute number of incorrect
#' classifications).
#' 
#' @param darch \code{\linkS4class{DArch}} instance.
#' @param newdata New data to use, \code{NULL} to use training data.
#' @param targets Labels for the \code{data}, \code{NULL} to use training
#'  labels (only possible when \code{data} is \code{NULL} as well).
#' @return Vector containing error function output, percentage of incorrect
#'  classifications and absolute number of incorrect classifications.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, retainData = T)
#' classificationStats <- darchTest(model)
#' }
#' @export
#' @family darch interface functions
darchTest <- function(darch, newdata = NULL, targets = T)
{
  if (is.null(newdata))
  {
    if (!getParameter(".retainData"))
    {
      stop(futile.logger::flog.error("No data available for prediction"))
    }
    
    dataSet <- darch@dataSet
  }
  else
  {
    if (is.null(darch@dataSet@formula) && is.logical.length1(targets, T))
    {
      stop(futile.logger::flog.error(
        "No target data provided for classification test"))
    }
    
    dataSet <- createDataSet(data = newdata,
      targets = targets,
      dataSet = darch@dataSet)
  }
  
  e <- testDArch(darch, dataSet@data, dataSet@targets, "All Data",
    getParameter(".darch.isClass"))
  list("error" = e[1], "percentIncorrect" = e[2], "numIncorrect" = e[3])
}

# TODO documentation
testDArch <- function(darch, data, targets, dataType, isClass)
{
  execOut <- getParameter(".darch.executeFunction")(darch, data)
  
  tError <- getParameter(".darch.errorFunction")(targets, execOut)
  classError <- NA
  numIncorrect <- NA
  if (isClass)
  {
    rows <- nrow(targets)
    cols <- ncol(targets)
    execOut <-
      (if (cols > 1) diag(cols)[max.col(execOut, ties.method = "first"),]
       else (execOut > .5)*1)
    numIncorrect <- sum(rowMeans(execOut == targets) < 1)
    classError <- numIncorrect / rows * 100
    futile.logger::flog.info("Classification error on %s: %s%% (%s/%s)",
      dataType, round(classError, 2), numIncorrect, rows)
  }
  
  futile.logger::flog.info("%s %s: %.3f", dataType, tError[[1]], tError[[2]])
  
  c(tError[[2]], classError, numIncorrect)
}