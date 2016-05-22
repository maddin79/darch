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
#' @rdname DataSet
#' @family darch classes
#' @keywords internal
setClass(
  Class = "DataSet",
  representation = representation(
    data = "ANY",
    targets = "ANY",
    formula = "ANY",
    parameters = "ANY"
  )
)

setMethod("initialize","DataSet",
  function(.Object)
  {
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
#' Based partly on code from nnet.
#  copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
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
#' @keywords internal
setGeneric(
  name="createDataSet",
  def=function(data, targets, formula, dataSet, ...) { standardGeneric("createDataSet") }
)

createDataSet.formula <- function(data, formula, ..., na.action = na.pass,
                                  previous.dataSet = new("DataSet"))
{
  numRows <- nrow(data)
  m <- model.frame(formula = formula, data = data, na.action = na.action)
  
  # TODO remove both na.action and this, caret can handle all of that
  if (nrow(m) < numRows)
  {
    futile.logger::flog.warn(
      "%s rows containing NAs were dropped from the dataset", numRows - nrow(m))
  }
  
  # Split into x and y data frames
  Terms <- attr(m, "terms")
  x <- m[,attr(Terms, "term.labels")]
  y <- if (length(model.response(m)) > 0) model.response(m) else NULL
  
  previous.dataSet@formula <- stats::formula(Terms)
  previous.dataSet@parameters$terms <- Terms
  previous.dataSet@parameters$na.action <- na.action
  dataSet <- preProcessData(x, y, ..., previous.dataSet = previous.dataSet)
  
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
#' @param data \code{\link{data.frame}} containing the dataset.
#' @param formula Model formula.
#' @param na.action \code{\link{model.frame}} parameter
#' @param previous.dataSet Existing \code{\linkS4class{DataSet}} from which
#'   parameters are extrated and re-used.
#' @return The new \code{\linkS4class{DataSet}} object
#' @seealso \link{darch.formula}, \link{createDataSet}
#' @aliases createDataSet.formula
#' @keywords internal
setMethod(
  "createDataSet",
  signature(data = "ANY", targets = "missing", formula = "formula",
    dataSet = "missing"),
  definition = createDataSet.formula
)

createDataSet.default <- function(data, targets, ...)
{
  data <- as.matrix(data)
  targets <- if (!is.null(targets) && is.null(dim(targets)))
    data.frame(y = targets) else targets
  if (!is.null(targets) && dim(data)[1] != dim(targets)[1])
    stop(futile.logger::flog.error(
      "Number of rows of 'data' and 'targets' must match"))

  preProcessData(data, targets, ...)
}

#' Create \code{\linkS4class{DataSet}} using data and targets.
#' 
#' @inheritParams createDataSet
#' @seealso \link{createDataSet}
#' @aliases createDataSet.default
#' @keywords internal
setMethod(
  "createDataSet",
  signature(data = "ANY", targets = "ANY", formula = "missing", dataSet = "missing"),
  definition = createDataSet.default
)

createDataSet.DataSet <- function(data, targets, dataSet, ...)
{
  # formula fit
  if (!is.null(dataSet@formula))
  {
    # Remove response from formula if no target data provided
    Terms <- dataSet@parameters$terms
    
    if (!missing(targets) && is.logical.length1(targets, F))
    {
      Terms <- delete.response(dataSet@parameters$terms)
    }
    
    dataSet <- createDataSet(data = data, formula = stats::formula(Terms), ...,
      na.action = dataSet@parameters$na.action, previous.dataSet = dataSet)
  }
  else
  {
    if (is.null(dim(data))) dim(data) <- c(1, length(data))
    y <- NULL
    
    if (!missing(targets) && !is.null(targets))
    {
      y <- if (!is.null(dim(targets))) targets else data.frame(y = targets)
    }
    
    dataSet <- createDataSet(data = data, targets = y, ...,
      previous.dataSet = dataSet)
  }
  
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
#' @keywords internal
setMethod(
  "createDataSet",
  signature(data = "ANY", targets = "ANY", formula = "missing",
    dataSet = "DataSet"),
  definition = createDataSet.DataSet
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
#' @keywords internal
setGeneric("validateDataSet",function(dataSet, darch){standardGeneric("validateDataSet")})

#' Validate \code{\linkS4class{DataSet}}
#' 
#' @inheritParams validateDataSet
#' @seealso \link{validateDataSet}
#' @keywords internal
setMethod(
  f = "validateDataSet",
  signature = "DataSet",
  definition = function(dataSet, darch)
  { 
    # first check whether non-numeric data exists in the data
    if (any(is.na(dataSet@data)) || (!is.null(dataSet@targets)
        && any(is.na(dataSet@targets))))
    {
      futile.logger::flog.error(paste("Dataset contains NA data, please",
                       "convert manually or use the caret package."))
      
      return(F)
    }

    # if we have targets, validate network structure
    if (!is.null(dataSet@targets))
    {
      # compare number of neurons in input and output layer to columns in data set
      neuronsInput <- dim(darch@layers[[1]][["weights"]])[1] - 1
      neuronsOutput <- dim(darch@layers[[length(darch@layers)]][["weights"]])[2]
      if (!all(neuronsInput == ncol(dataSet@data),
               neuronsOutput == ncol(dataSet@targets) || !getParameter(".darch.isClass")))
      {
        futile.logger::flog.error(paste(
          "DataSet incompatible with DArch, number of neurons in the first",
          "and last layer have to equal the number of columns in the data",
          "(%s) and columns or classes in the targets (%s)"),
          ncol(dataSet@data), ncol(dataSet@targets))
        
        return(F)
      }
    }
    
    return(T)
  }
)

# Dataset pre-processing
preProcessData <- function(x, y, ..., previous.dataSet = new("DataSet"),
  preProc.params = F, preProc.targets = F, preProc.factorToNumeric = F,
  preProc.factorToNumeric.targets = F, preProc.fullRank = T,
  preProc.fullRank.targets = F, preProc.orderedToFactor.targets = T)
{
  dataSet <- previous.dataSet
  x <- simplifyDataFrame(as.data.frame(x))
  
  if (!is.null(y))
  {
    y <- simplifyDataFrame(as.data.frame(y), preProc.orderedToFactor.targets)
  }
  
  # Seek ordered factors
  columnsToBeConverted <- attr(which(sapply(names(x), FUN = function(n) {
    (is.factor(x[[n]]) && preProc.factorToNumeric)
    })), "names")
  
  columnsToBeConvertedY <- if (!is.null(y)) attr(which(sapply(names(y),
    FUN = function(n)
    {
      (is.factor(y[[n]]) && preProc.factorToNumeric.targets)
    })), "names") else c()
  
  # Create caret parameters during the initial run
  if (is.null(dataSet@parameters$preProcess))
  {
    futile.logger::flog.info(
      "Start initial caret pre-processing.")
    
    # Convert ordered factors to numeric first
    
    if (length(columnsToBeConverted) > 0)
    {
      # TODO solver cleaner
      # These are converted here to not fall victim to dummyVars
      x[, columnsToBeConverted] <-
        sapply(x[, columnsToBeConverted], FUN=function(n) { as.numeric(n) })
    }
    
    if (is.list(preProc.params))
    {
      preProc.params$x <- x
      
      if (is.null(preProc.params$verbose))
      {
        preProc.params$verbose <-
          (names(futile.logger::DEBUG) == futile.logger::flog.threshold())
      }
      
      dataSet@parameters$preProcess <-
        eval(as.call(c(list(quote(caret::preProcess)), preProc.params)))
      
      # Remove data again
      preProc.params$x <- NULL
      
      futile.logger::flog.info("Result of preProcess for data:")
      futile.logger::flog.info(
        { print(dataSet@parameters$preProcess); NULL })
    }
    else
    {
      dataSet@parameters$preProcess <- T
    }
    
    dataSet@parameters$dummyVarsData <- caret::dummyVars(~., x,
      fullRank = preProc.fullRank)
    
    futile.logger::flog.info(
      "Converting non-numeric columns in data (if any)...")
    
    if (length(columnsToBeConverted) > 0)
    {
      futile.logger::flog.info("Converting %s columns (%s) to numeric",
        length(columnsToBeConverted),
        paste(columnsToBeConverted, collapse = ", "))
    }
    
    printDummyVarsFactors(dataSet@parameters$dummyVarsData,
                          attr(dataSet@parameters$terms, "term.labels"))
    futile.logger::flog.debug("Result of dummyVars for data:")
    futile.logger::flog.debug({ print(dataSet@parameters$dummyVarsData); NULL })
    
    if (!is.null(y))
    {
      if (length(columnsToBeConvertedY) > 0)
      {
        # TODO solver cleaner, extract into method
        # These are converted here to not fall victim to dummyVars
        y[, columnsToBeConvertedY] <-
          sapply(y[, columnsToBeConvertedY], FUN = function(n) { as.numeric(n) })
      }
      
      if (preProc.targets)
      {
        dataSet@parameters$preProcessTargets <-
          caret::preProcess(y, method = c("center", "scale"))
        
        futile.logger::flog.info("Result of preProcess for targets:")
        futile.logger::flog.info(
          { print(dataSet@parameters$preProcessTargets); NULL })
      }
      
      dataSet@parameters$dummyVarsTargets <- caret::dummyVars(~., y,
        fullRank = preProc.fullRank.targets)
      
      futile.logger::flog.info(
        "Converting non-numeric columns in targets (if any)...")
      
      if (length(columnsToBeConvertedY) > 0)
      {
        futile.logger::flog.info("Converting %s columns (%s) to numeric",
         length(columnsToBeConvertedY),
         paste(columnsToBeConvertedY, collapse = ", "))
      }
      
      printDummyVarsFactors(dataSet@parameters$dummyVarsTargets,
        as.character(attr(dataSet@parameters$terms,
        "variables")[-1])[attr(dataSet@parameters$terms, "response")],
        "Dependent factor")
      futile.logger::flog.debug("Result of dummyVars for targets:")
      futile.logger::flog.debug(
        { print(dataSet@parameters$dummyVarsTargets); NULL })
    }
    
    dataSet@parameters$preProcessParams <- preProc.params
  }
  
  if (length(columnsToBeConverted) > 0)
  {
    # TODO prevent double conversion if this is the initial processing
    x[, columnsToBeConverted] <-
      sapply(x[, columnsToBeConverted], FUN = function(n) { as.numeric(n) })
  }
  
  # Pre-process data
  if (inherits(dataSet@parameters$preProcess, "preProcess"))
  {
    futile.logger::flog.info("Pre-processing data set.")
    # TODO call with preProc.params instead
    x <- predict(dataSet@parameters$preProcess, newdata = x, verbose = T)
  }
  
  dataSet@data <-
    predict(dataSet@parameters$dummyVarsData, newdata = x)
  
  if (!is.null(y))
  {
    if (inherits(dataSet@parameters$preProcessTargets, "preProcess"))
    {
      y <- predict(dataSet@parameters$preProcessTargets, newdata = y,
        verbose = T)
    }
    
    if (length(columnsToBeConvertedY) > 0)
    {
      # TODO preven double conversion
      y[, columnsToBeConvertedY] <-
        sapply(y[, columnsToBeConvertedY], FUN = function(n) { as.numeric(n) })
    }
    
    dataSet@targets <-
      predict(dataSet@parameters$dummyVarsTargets, newdata = y)
  }
  
  dataSet
}

# Prints which variables were converted from factors to 1:n coding
printDummyVarsFactors <- function(dV, originalFactorNames = NULL,
                                  prefix = "Factor")
{
  if (length(dV$lvls) == 0)
  {
    return(NULL)
  }
  
  originalFactorNames <-
    if (is.null(originalFactorNames)) names(dV$lvls) else originalFactorNames
  factorNames <- names(dV$lvls)
  for (i in 1:length(dV$lvls))
  {
    futile.logger::flog.info(
      "%s \"%s\" converted to %s new variables (1-of-n coding)", prefix,
      originalFactorNames[i], length(dV$lvls[[factorNames[i]]]))
  }
}

# Remove data from dataset if necessary
# TODO better name?
postProcessDataSet <- function(dataSet = get("dataSet", envir = parent.frame()),
  darch = get("darch", envir = parent.frame()))
{
  if (!getParameter(".retainData", F))
  {
    dataSet@data <- NULL
    dataSet@targets <- NULL
  }
  
  dataSet
}

# Simplifies a data.frame to only contain numeric or factor columns
simplifyDataFrame <- function(df, orderedToFactor = F)
{
  # TODO solve with apply?
  for (c in names(df))
  {
    if (is.numeric(df[[c]]))
    {
      df[[c]] <- as.numeric(df[[c]])
    }
    else if (!is.factor(df[[c]]))
    {
      levels <- unique(c(df[[c]]))
      df[[c]] <- factor(df[[c]], labels = make.names(levels))
      #df[[c]] <- as.factor(df[[c]]
    }
    else if (is.ordered(df[[c]]) && orderedToFactor)
    {
      futile.logger::flog.debug("Converting ordered target column %s to factor",
        c)
      class(df[[c]]) <- class(df[[c]])[class(df[[c]]) != "ordered"]
    }
  }
  
  df
}
