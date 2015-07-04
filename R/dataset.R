# Copyright (C) 2015 darch2
#
# This file is part of darch2.
#
# Darch2 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch2.  If not, see <http://www.gnu.org/licenses/>.
#
#
#' Class for specifying datasets.
#' 
#' @details 
#' This class deals with data (input) and targets (output) for neural
#' networks, including conversion of ordinal data, validation of data, and
#' application of model formulae to the data.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{data}:}{Input data.}
#'   \item{\code{targets}:}{Target output.}
#'   \item{\code{formula}:}{\code{\link{formula}} for the data.}
#'   \item{\code{parameters}:}{Fit parameters.}
#' }
#' 
#' @exportClass DataSet
#' @author Johannes Rueckert
#' @name DataSet
#' @rdname DataSet
#' @aliases DataSet-class
setClass(
  Class="DataSet",
  representation=representation(
    data = "matrix",
    targets = "matrix",
    formula = "ANY",
    parameters = "ANY"
  )
)

setMethod ("initialize","DataSet",
           function(.Object){	
             .Object@data <- matrix()
             .Object@targets <- matrix()
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
#' @return New \code{\linkS4class{DataSet}}
#' @export
setGeneric(
  name="createDataSet",
  def=function(data, targets, formula, dataSet, ...) { standardGeneric("createDataSet") }
)

#' Constructor function for \code{\linkS4class{DataSet}} objects.
#' 
#' Generates a new \code{\linkS4class{DataSet}} object with the given 
#' parameters.
#' 
#' @details Initializes a \code{\linkS4class{DataSet}} with data using a
#'   \code{\link{formula}} and saving its parameters for subsequent data.
#'   
#' @inheritParams darch.formula
#' @param subset Row indexing vector, \strong{not} parameter to
#'   \code{\link{model.frame}}
#' @param na.action \code{\link{model.frame}} parameter
#' @param contrasts \code{\link{model.frame}} parameter
#'   
#' @return The new \code{\linkS4class{DataSet}} object
#' @seealso \code{\link{darch.formula}}
#' 
#' @export
createDataSet.formula <- function(data, formula, ..., subset, na.action, contrasts=NULL)
{  
  if (is.matrix(data))
  {
    data <- as.data.frame(data)
  }
  
  m <- model.frame(formula=formula, data=data, na.action=na.action)
  
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  y <- model.response(m)
  
  # remove intercept column if necessary
  xint <- match("(Intercept)", colnames(x), nomatch=0)
  if (xint > 0)
  {
    x <- x[, -xint, drop=F]
  }
  
  # convert y to numeric
  if(is.factor(y))
  {
    y <- factorToNumeric(y) 
  }

  dataSet <- new("DataSet")
  dataSet@data <- as.matrix(x)
  dataSet@targets <- as.matrix(y)
  dataSet@formula <- formula
  # TODO which ones are needed?
  dataSet@parameters$terms <- Terms
  dataSet@parameters$coefnames <- colnames(x)
  dataSet@parameters$call <- match.call()
  dataSet@parameters$na.action <- attr(m, "na.action")
  dataSet@parameters$contrasts <- cons
  dataSet@parameters$xlevels <- .getXlevels(Terms, m)
  
  # TODO move somewhere else?
  if (!missing(subset))
  {
    dataSet@data <- dataSet@data[subset,,drop=F]
    dataSet@targets <- dataSet@targets[subset,,drop=F]
  }
  
  return(dataSet)
}

#' Create \code{\linkS4class{DataSet}} from \code{\link{formula}}.
#' 
#' @inheritParams createDataSet
#' @export
setMethod(
  "createDataSet",
  signature(data="ANY", targets="missing", formula="formula", dataSet="missing"),
  definition=createDataSet.formula
)

#' Create \code{\linkS4class{DataSet}} using data and targets.
#' 
#' @inheritParams createDataSet
#' @export
createDataSet.default <- function(data, targets, ...)
{
  data <- as.matrix(data)
  targets <- as.matrix(targets)
  if(any(is.na(data))) stop("missing values in 'data'")
  if(any(is.na(data))) stop("missing values in 'targets'")
  if(dim(data)[1L] != dim(targets)[1L])
    stop("nrows of 'data' and 'targets' must match")
  
  dataSet <- new("DataSet")
  dataSet@data <- data
  dataSet@targets <- targets
  
  return(dataSet)
}

#' @keywords internal
#' @export
setMethod(
  "createDataSet",
  signature(data="ANY", targets="ANY", formula="missing", dataSet="missing"),
  definition=createDataSet.default
)

#' Create new \code{\linkS4class{DataSet}} by filling an existing one with new 
#' data.
#' 
#' @export
createDataSet.DataSet <- function(data, targets, dataSet, ...)
{
  if (!is.null(dataSet@formula))
  {
    # formula fit
    data <- as.data.frame(data)
    # TODO remove
    #rn <- row.names(data)
    # remove targets if they were not provided
    # TODO solve implicitly?
    if (!is.null(targets) && targets == F)
    {
      Terms <- delete.response(dataSet@parameters$terms)
    }
    # work hard to predict NA for rows with missing data
    m <- model.frame(Terms, data, na.action = na.omit,
                     xlev = dataSet@parameters$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    # TODO remove
    #keep <- match(row.names(m), rn)
    x <- model.matrix(Terms, m, contrasts = dataSet@parameters$contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch=0)
    if(xint > 0) x <- x[, -xint, drop=FALSE]
  }
  else
  {
    # matrix fit
    if(is.null(dim(data)))
      dim(data) <- c(1L, length(data)) # a row vector
    x <- as.matrix(data) # to cope with dataframes
    if(any(is.na(x))) stop("missing values in 'data'")
    # TODO remove
    #keep <- 1L:nrow(x)
    rn <- rownames(x)
    
    if (!is.null(targets) && targets != F)
    {
      if(is.factor(targets))
      {
        targets <- factorToNumeric(targets) 
      }
      
      dataSet@targets <- targets
    }
  }
  
  dataSet@data <- x
  
  return(dataSet)
}

#' @keywords internal
#' @export
setMethod(
  "createDataSet",
  signature(data="ANY", targets="ANY", formula="missing", dataSet="DataSet"),
  definition=createDataSet.DataSet
)

# TODO documentation
#' @keywords internal
#' @export
convertToNumeric <- function(y)
{
  # TODO documentation
  class.ind <- function(cl)
  {
    n <- length(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
  }
  
  lev <- levels(y)
  counts <- table(y)
  
  if(any(counts == 0L))
  {
    empty <- lev[counts == 0L]
    flog.warn("Empty groups:")
    print(empty)
    
    y <- factor(y, levels=lev[counts > 0L])
  }
  
  if(length(lev) == 2L)
  {
    y <- as.vector(unclass(y)) - 1
  }
  else
  {
    y <- class.ind(y)
  }
  
  return(y)
}

#' Validates the \code{\link{DataSet}} for the given \code{\link{DArch}} object.
#' 
#' @details Validates the data dimensions and data types of the 
#'   \code{\link{DataSet}}.
#'   
#' @param dataSet \code{\link{DataSet}} to validate
#' @param darch \code{\link{DArch}} object to validate this 
#'   \code{\link{DataSet}} against.
#'   
#' @return Whether the \code{\link{DataSet}} is valid.
#'   
#' @export
setGeneric("validateDataSets",function(dataSets, darch){standardGeneric("validateDataSets")})

#' @keywords internal
#' @export
setMethod(
  f="validateDataSets",
  signature="list",
  definition=function(dataSets, darch){
    for (i in names(dataSets))
    {
      dataSet <- dataSets[[i]]
      
      # first check whether non-numeric data exists in the data
      if (!all(is.numeric(dataSet@data), is.numeric(dataSet@targets)))
      {
        flog.error(paste("DataSet is not numeric, please convert ordinal or",
                         "nominal data to numeric first."))
        
        return(F)
      }
      
      # compare number of neurons in input and output layer to columns in data set
      rbmFirstLayer <- getRBMList(darch)[[1]]
      rbmLastLayer <- getRBMList(darch)[[length(getRBMList(darch))]]
      neuronsInput <- getNumVisible(rbmFirstLayer)
      neuronsOutput <- getNumHidden(rbmLastLayer)
      if (!all(neuronsInput == ncol(dataSet@data),
               neuronsOutput == ncol(dataSet@targets)))
      {
        flog.error(paste("DataSet \"", i, "\" incompatible with DArch,",
                         "number of neurons in the first and last layer have",
                         "to equal the number of columns in the training data",
                         "and targets, respectively."))
        
        return(F)
      }
    }
    
    return(T)
  }
)