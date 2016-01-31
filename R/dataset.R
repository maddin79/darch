# Copyright (C) 2013-2016 Martin Drees
#
# Based partly on code from nnet.
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
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

#' Class for specifying datasets.
#' 
#' @details This class deals with data (input) and targets (output) for neural 
#' networks, including conversion of ordinal and nominal data, validation of
#' data, and application of model formulae to the data.
#' 
#' @slot data Input data.
#' @slot targets Target output.
#' @slot formula \code{\link{formula}} for the data.
#' @slot parameters Fit parameters.
#' @exportClass DataSet
#' @aliases DataSet dataset
setClass(
  Class="DataSet",
  representation=representation(
    data = "matrix",
    targets = "ANY",
    formula = "ANY",
    parameters = "ANY"
  )
)

setMethod ("initialize","DataSet",
           function(.Object){	
             .Object@data <- matrix()
             .Object@targets <- NULL
             .Object@formula <- NULL
             .Object@parameters <- NULL
             return(.Object)
           }
)

#' Create data set using data, targets, a formula, and possibly an existing data
#' set.
#' 
#' @param data Input data, possibly also target data if a formula is used.
#' @param targets Target data.
#' @param formula Model \code{\link{formula}}.
#' @param dataSet \code{\linkS4class{DataSet}} to be used as the basis for the 
#'   new one
#' @param ... Further parameters.
#' @return New \code{\linkS4class{DataSet}}
#' @seealso \link{createDataSet.default}, \link{createDataSet.formula},
#'  \link{createDataSet.DataSet}
#' @export
setGeneric(
  name="createDataSet",
  def=function(data, targets, formula, dataSet, ...) { standardGeneric("createDataSet") }
)

createDataSet.formula <- function(data, formula, ..., na.action = na.omit,
  contrasts = NULL)
{
  numRows <- nrow(data)
  m <- model.frame(formula = formula, data = data, na.action = na.action)
  
  if (nrow(m) < numRows)
  {
    futile.logger::flog.info(
      "%s rows containing NAs were dropped from the dataset", numRows - nrow(m))
  }
  
  # Split into x and y matrices
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts, drop2nd = T)
  cons <- attr(x, "contrast")
  assign <- attr(x, "assign")
  y <- model.response(m)
  
  # Remove intercept column if necessary
  xint <- match("(Intercept)", colnames(x), nomatch=0)
  if (xint > 0)
  {
    x <- x[, -xint, drop=F]
  }

  dataSet <- new("DataSet")
  dataSet@data <- as.matrix(x)
  dataSet@targets <- as.matrix(y)
  dataSet@formula <- formula
  # TODO which ones are needed?
  dataSet@parameters$coefnames <- colnames(x)
  dataSet@parameters$assign <- assign
  dataSet@parameters$contrasts <- cons
  dataSet@parameters$terms <- Terms
  dataSet@parameters$xlevels <- .getXlevels(Terms, m)
  dataSet@parameters$ylevels <- levels(y)
  dataSet@parameters$na.action <- na.action
  
  dataSet <- preProcessDataSet(dataSet, ...)
  
  dataSet
}

#' Constructor function for \code{\linkS4class{DataSet}} objects.
#' 
#' Generates a new \code{\linkS4class{DataSet}} object with the given 
#' parameters.
#' 
#' @details Initializes a \code{\linkS4class{DataSet}} with data using a
#'   \code{\link{formula}} and saving its parameters for subsequent data.
#'   
#' @inheritParams darch.formula
#' @inheritParams createDataSet,ANY,ANY,missing,missing-method
#' @param formula Model formula.
#' @param subset Row indexing vector, \strong{not} parameter to
#'   \code{\link{model.frame}}
#' @param na.action \code{\link{model.frame}} parameter
#' @param contrasts \code{\link{model.frame}} parameter
#' @return The new \code{\linkS4class{DataSet}} object
#' @seealso \link{darch.formula}, \link{createDataSet}
#' @aliases createDataSet.formula
#' @export
setMethod(
  "createDataSet",
  signature(data="ANY", targets="missing", formula="formula", dataSet="missing"),
  definition=createDataSet.formula
)

createDataSet.default <- function(data, targets, ..., scale=F)
{
  data <- as.matrix(data)
  targets <- if (!is.null(targets)) as.matrix(targets) else NULL
  if(!is.null(targets) && dim(data)[1L] != dim(targets)[1L])
    stop("nrows of 'data' and 'targets' must match")
  
  dataSet <- new("DataSet")
  dataSet@data <- data
  dataSet@targets <- targets
  
  dataSet <- preProcessDataSet(dataSet, ...)
  
  dataSet
}

#' Create \code{\linkS4class{DataSet}} using data and targets.
#' 
#' @inheritParams createDataSet
#' @param scale Logical indicating whether to scale the data.
#' @seealso \link{createDataSet}
#' @aliases createDataSet.default
#' @export
setMethod(
  "createDataSet",
  signature(data="ANY", targets="ANY", formula="missing", dataSet="missing"),
  definition=createDataSet.default
)

createDataSet.DataSet <- function(data, targets, dataSet, ...)
{
  y <- NULL
  
  # formula fit
  if (!is.null(dataSet@formula))
  {
    # Remove response from formula if no target data provided
    Terms <- dataSet@parameters$terms
    if (targets == F)
    {
      Terms <- delete.response(dataSet@parameters$terms)
    }
    
    numRows <- nrow(data)
    # work hard to predict NA for rows with missing data
    m <- model.frame(Terms, data, na.action = dataSet@parameters$na.action,
                     xlev = dataSet@parameters$xlevels)
    
    if (nrow(m) < numRows)
    {
      futile.logger::flog.info(
        "%s rows containing NAs were dropped from the dataset",
        numRows - nrow(m))
    }
    
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    x <- model.matrix(Terms, m, contrasts = dataSet@parameters$contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch=0)
    if (xint > 0) x <- x[, -xint, drop=FALSE]
    
    if (targets != F)
    {
      y <- model.response(m)
    }
  }
  else
  {
    # matrix fit
    if (is.null(dim(data)))
      dim(data) <- c(1L, length(data)) # a row vector
    x <- as.matrix(data) # to cope with dataframes
    #if (any(is.na(x))) stop("missing values in 'data'")
    
    if (!is.null(targets) && (length(targets) > 1 || targets != F))
    {
      y <- targets
    }
  }
  
  dataSet@data <- x
  dataSet@targets <- y
  
  dataSet <- preProcessDataSet(dataSet,
    caret.preProcessParams = dataSet@parameters$preProcessParams)
  
  dataSet
}

#' Create new \code{\linkS4class{DataSet}} by filling an existing one with new 
#' data.
#' 
#' @param dataSet \code{\linkS4class{DataSet}} that is used as a basis for the
#'   new one
#' @inheritParams createDataSet
#' @aliases createDataSet.DataSet
#' @seealso \link{createDataSet}
#' @export
setMethod(
  "createDataSet",
  signature(data="ANY", targets="ANY", formula="missing", dataSet="DataSet"),
  definition=createDataSet.DataSet
)

#' Validate \code{\linkS4class{DataSet}}
#' 
#' Validates the \code{\linkS4class{DataSet}} for the given
#' \code{\linkS4class{DArch}} object.
#' 
#' Validates the data dimensions and data types of the
#' \code{\linkS4class{DataSet}}.
#'   
#' @param dataSet \code{\linkS4class{DataSet}} to validate
#' @param darch \code{\linkS4class{DArch}} object to validate this 
#'   \code{\linkS4class{DataSet}} against.
#' @return Logical indicating whether the \code{\linkS4class{DataSet}} is valid.
#' @export
setGeneric("validateDataSet",function(dataSet, darch){standardGeneric("validateDataSet")})

#' Validate \code{\linkS4class{DataSet}}
#' 
#' @inheritParams validateDataSet
#' @seealso \link{validateDataSet}
#' @export
setMethod(
  f="validateDataSet",
  signature="DataSet",
  definition=function(dataSet, darch)
  {
    # first check whether non-numeric data exists in the data
    if (any(!is.numeric(dataSet@data)) || (!is.null(dataSet@data)
        && any(!is.numeric(dataSet@targets))))
    {
      flog.error(paste("Dataset contains non-numeric or NULL data, please",
                       "convert to numeric or install the caret package."))
      
      return(F)
    }
    
    # if we have targets, validate network structure
    if (!is.null(dataSet@targets))
    {
      # compare number of neurons in input and output layer to columns in data set
      neuronsInput <- dim(darch@layers[[1]][["weights"]])[1]-1
      neuronsOutput <- dim(darch@layers[[length(darch@layers)]][["weights"]])[2]
      if (!all(neuronsInput == ncol(dataSet@data),
               neuronsOutput == ncol(dataSet@targets)))
      {
        flog.error(paste0("DataSet incompatible with DArch, number of neurons ",
                         "in the first and last layer have to equal the ",
                         "number of columns in the data (", ncol(dataSet@data),
                         ") and columns or classes in the targets (",
                         ncol(dataSet@targets), ")."))
        
        return(F)
      }
    }
    
    return(T)
  }
)

preProcessDataSet <- function(dataSet, ..., caret.preProcessParams = F)
{
  # TODO would prefer requireNamespace here, but caret registers its functions
  # globally without namespace, will result in errors
  if (!suppressMessages(require("caret", quietly = T)))
  {
    futile.logger::flog.info(
      "\"caret\" package not installed, skipped pre-processing")
    
    return (dataSet)
  }
  
  # Create caret parameters during the initial run
  if (is.null(dataSet@parameters$caret))
  {
    futile.logger::flog.info(
      "Start initial caret pre-processing.")
    
    dataSet@parameters$dummyVarsData <- caret::dummyVars(~ ., dataSet@data, ...)
    
    futile.logger::flog.info("Result of dummyVars:")
    futile.logger::flog.info({ print(dataSet@parameters$dummyVarsData); NULL })
    
    if (is.list(caret.preProcessParams))
    {
      caret.preProcessParams$x <- dataSet@data
      caret.preProcessParams$verbose <-
        (names(futile.logger::DEBUG) == futile.logger::flog.threshold())
      dataSet@parameters$preProcessData <-
        eval(as.call(c(list(quote(caret::preProcess)), caret.preProcessParams)))
      
      futile.logger::flog.info("Result of preProcess:")
      futile.logger::flog.info(
        { print(dataSet@parameters$preProcessData); NULL })
    }
    
    if (!is.null(dataSet@targets))
    {
      dataSet@parameters$dummyVarsTargets <-
        caret::dummyVars(~ ., dataSet@targets)
    }
    
    dataSet@parameters$caret <- T
    dataSet@parameters$preProcessParams <- caret.preProcessParams
  }
  
  futile.logger::flog.info(
    "Pre-processing dataset.")
  
  # Pre-process data
  if (is.list(caret.preProcessParams))
  {  
    dataSet@data <-
      predict(dataSet@parameters$preProcessData, newdata = dataSet@data)
  }
  
  dataSet@data <-
    predict(dataSet@parameters$dummyVarsData, newdata = dataSet@data)
  
  if (!is.null(dataSet@targets))
  { 
    dataSet@targets <-
      predict(dataSet@parameters$dummyVarsTargets, newdata = dataSet@targets)
  }
  
  dataSet
}