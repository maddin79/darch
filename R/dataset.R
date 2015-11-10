# Copyright (C) 2013-2015 Martin Drees
#
# Based partly on code from nnet.
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
#
# Scaling code based on code from package e1071.
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

createDataSet.formula <- function(data, formula, ..., subset, na.action, contrasts=NULL, scale=F)
{
  if (is.matrix(data))
  {
    data <- as.data.frame(data)
  }
  
  m <- model.frame(formula=formula, data=data, na.action=na.action)
  
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  assign <- attr(x, "assign")
  y <- model.response(m)
  
  # remove intercept column if necessary
  xint <- match("(Intercept)", colnames(x), nomatch=0)
  if (xint > 0)
  {
    x <- x[, -xint, drop=F]
  }
  colnames(x) <- attr(Terms, "term.labels")
  
  res <- NULL
  lev <- NULL
  yscale = T
  
  # convert y to numeric
  if(is.factor(y))
  {
    res <- factorToNumeric(y)
    y <- res$y
    lev <- res$lev
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
  dataSet@parameters$assign <- assign
  dataSet@parameters$xlevels <- .getXlevels(Terms, m)
  dataSet@parameters$ylevels <- lev
  dataSet <- scaleData(dataSet, scale)
  
  # TODO move somewhere else?
  if (!missing(subset))
  {
    dataSet@data <- dataSet@data[subset,,drop=F]
    dataSet@targets <- dataSet@targets[subset,,drop=F]
  }
  
  return(dataSet)
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
  if(any(is.na(data))) stop("missing values in 'data'")
  if(any(is.na(data))) stop("missing values in 'targets'")
  if(!is.null(targets) && dim(data)[1L] != dim(targets)[1L])
    stop("nrows of 'data' and 'targets' must match")
  
  dataSet <- new("DataSet")
  dataSet@data <- data
  dataSet@targets <- targets
  dataSet <- scaleData(dataSet, scale)
  
  return(dataSet)
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
  y <- matrix(0)
  
  if (!is.null(dataSet@formula))
  {
    # formula fit
    data <- as.data.frame(data)
    # TODO remove
    #rn <- row.names(data)
    
    # Remove response from formula if no target data provided
    Terms <- dataSet@parameters$terms
    if (targets == F)
    {
      Terms <- delete.response(dataSet@parameters$terms)
    }

    # work hard to predict NA for rows with missing data
    m <- model.frame(Terms, data, na.action = dataSet@parameters$na.action,
                     xlev = dataSet@parameters$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    # TODO remove
    #keep <- match(row.names(m), rn)
    x <- model.matrix(Terms, m, contrasts = dataSet@parameters$contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch=0)
    if (xint > 0) x <- x[, -xint, drop=FALSE]
    
    if (targets != F)
    {
      y <- model.response(m)
      # convert y to numeric
      if(is.factor(y))
      {
        y <- factorToNumeric(y, dataSet@parameters$ylevels)$y
      }
    }
  }
  else
  {
    # matrix fit
    if (is.null(dim(data)))
      dim(data) <- c(1L, length(data)) # a row vector
    x <- as.matrix(data) # to cope with dataframes
    if (any(is.na(x))) stop("missing values in 'data'")
    
    if (!is.null(targets) && (length(targets) > 1 || targets != F))
    {
      y <- targets
      
      if (is.factor(y))
      {
        y <- factorToNumeric(y, dataSet@parameters$ylevels)$y
      }
    }
  }
  
  # If scaling parameters exist, rescale new data according to them
  if (any(dataSet@parameters$scaled))
  {
    x[,dataSet@parameters$scaled] <-
      scale(x[,dataSet@parameters$scaled, drop = F],
            center = dataSet@parameters$xscale$"scaled:center",
            scale = dataSet@parameters$xscale$"scaled:scale")
    
    if (!is.null(dataSet@parameters$yscale))
    {
      y <- scale(y, center = dataSet@parameters$yscale$"scaled:center",
                 scale = dataSet@parameters$yscale$"scaled:scale")
    }
  }
  
  dataSet@data <- x
  dataSet@targets <- y
  
  return(dataSet)
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

factorToNumeric <- function(y, lev=NULL)
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
  
  lev <- if (!is.null(lev)) lev else levels(y)
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
  
  res <- NULL
  res$y <- y
  res$lev <- lev
  
  return(res)
}

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
    if (!all(is.numeric(dataSet@data), is.null(dataSet@targets) || is.numeric(dataSet@targets)))
    {
      flog.error(paste("DataSet is not numeric, please convert ordinal or",
                       "nominal data to numeric first."))
      
      return(F)
    }
    
    # if we have targets, validate network structure
    if (!is.null(dataSet@targets))
    {
      # compare number of neurons in input and output layer to columns in data set
      neuronsInput <- dim(getLayerWeights(darch, 1))[1]-1
      neuronsOutput <- dim(getLayerWeights(darch, length(getLayers(darch))))[2]
      if (!all(neuronsInput == ncol(dataSet@data),
               neuronsOutput == ncol(dataSet@targets)))
      {
        flog.error(paste("DataSet incompatible with DArch,",
                         "number of neurons in the first and last layer have",
                         "to equal the number of columns in the data and",
                         "targets, respectively."))
        
        return(F)
      }
    }
    
    return(T)
  }
)

scaleData <- function(dataSet, scale)
{
  x <- dataSet@data
  y <- dataSet@targets
  
  if (length(scale) == 1)
  {
    scale <- rep(scale, ncol(x))
  }
  
  yscale <- scale[1]
  
  if (any(scale))
  {
    if (!is.null(dataSet@formula))
    {
      remove <- unique(c(which(labels(dataSet@parameters$terms)
                               %in% dataSet@parameters$contrasts),
                         which(!scale)))
      scale <- !dataSet@parameters$assign %in% remove
      yscale <- scale[1]
      scale <- scale[2:length(scale)]
    }
    
    co <- !apply(x[,scale, drop = FALSE], 2, var)
    # if we only have one row, or some fields are NA for some reason
    co[which(is.na(co))] = T
    if (any(co))
    {
      warning(paste("Variable(s)",
                    paste(sQuote(colnames(x[,scale, drop = FALSE])[co]),
                          sep="", collapse=" and "),
                    "constant. Cannot scale data.")
      )
      scale <- rep(FALSE, ncol(x))
    }
    else
    {
      xtmp <- scale(x[,scale])
      x[,scale] <- xtmp
      dataSet@parameters$xscale <- attributes(xtmp)[c("scaled:center","scaled:scale")]
    }
    
    if (yscale && !is.null(dataSet@parameters$ylevels) && is.numeric(y)
        && ncol(as.matrix(y)) == 1)
    {
      y <- scale(y)
      dataSet@parameters$yscale <- attributes(y)[c("scaled:center","scaled:scale")]
      y <- as.vector(y)
    }
  }
  
  dataSet@data <- x
  dataSet@targets <- y
  dataSet@parameters$scaled <- scale
  
  return (dataSet)
}