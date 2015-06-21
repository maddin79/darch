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
#'   \item{\code{trainData}:}{Input data for the training phase.}
#'   \item{\code{trainTargets}:}{Target output for the training phase.}
#'   \item{\code{validData}:}{Input validation data.}
#'   \item{\code{validTargets}:}{Validation target output.}
#'   \item{\code{testData}:}{Input test data.}
#'   \item{\code{testTargets}:}{Test target output.}
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
    trainData = "matrix",
    trainTargets = "matrix",
    # TODO work with empty data frames instead of NULL
    validData = "ANY",
    validTargets = "ANY",
    testData = "ANY",
    testTargets = "ANY"
  )
)

setMethod ("initialize","DataSet",
           function(.Object){	
             .Object@trainData <- matrix()
             .Object@trainTargets <- matrix()
             .Object@validData <- NULL
             .Object@validTargets <- NULL
             .Object@testData <- NULL
             .Object@testTargets <- NULL
             return(.Object)    
           }
)

#' Constructor function for \code{\link{DataSet}} objects.
#' 
#' Generates a new \code{\link{DataSet}} object with the given parameters.
#' 
#' @details
#' Initializes a \code{\link{DataSet}} with train (and, optionally, validation
#' and test) input and output (label) data to be used with a
#' \code{\link{DArch}} object-
#' 
#' @param trainData Input data for the training phase.
#' @param trainTargets Target output for the training phase.
#' @param validData Optional validation input data.
#' @param validTargets Optional validation target output.
#' @param testData Optional test input data.
#' @param testTargets Optional test target output.
#' @param formulaData Optional formula for input data.
#' @param formulaTargets Optional formula for target output.
#' @param ordinalMappingData Optional list containing vectors or matrices
#'        providing information on ordinal data (its ordering) in the input
#'        data, see examples for more details on how to use this.
#' @param ordinalMappingTargets Optional list containing vectors or matrices
#'        providing information on ordinal data (its ordering) in the target
#'        data, see examples for more details on how to use this.
#'        
#' @usage createDataSet(trainData, trainTargets, validData, validTargets,
#'                      testData, testTargets, formulaData, formulaTargets,
#'                      ordinalMappingData, ordinalMappingTargets)
#' 
#' @return The new DataSet object
#' 
#' @export
createDataSet <- function(trainData, trainTargets, validData=NULL, validTargets=NULL, testData=NULL, testTargets=NULL, formulaData=~ ., formulaTargets=~ ., ordinalMappingData=list(), ordinalMappingTargets=list())
{
  dataSet <- new("DataSet")
  trainData <- model.frame(formulaData, as.data.frame(trainData))
  trainTargets <- model.frame(formulaTargets, as.data.frame(trainTargets))
  
  if (!is.null(validData))
  {
    validData <- model.frame(formulaData, as.data.frame(validData))
    validTargets <- model.frame(formulaTargets, as.data.frame(validTargets))
  }
  
  if (!is.null(testData))
  {
    testData <- model.frame(formulaData, as.data.frame(testData))
    testTargets <- model.frame(formulaTargets, as.data.frame(testTargets))
  }
  
  # TODO extract into method
  for (column in names(ordinalMappingData))
  {
    trainData[column] <- match(trainData[[column]],ordinalMappingData[[column]])
    
    if (!is.null(validData))
    {
      validData[column] <-
        match(validData[[column]],ordinalMappingData[[column]])
    }
    
    if (!is.null(testData))
    {
      testData[column] <-
        match(validData[[column]],ordinalMappingData[[column]])
    }
  }
  
  for (column in names(ordinalMappingTargets))
  {
    trainTargets[column] <- match(trainTargets[[column]],
                                    ordinalMappingTargets[[column]])
    
    if (!is.null(validData))
    {
      validTargets[column] <- match(validTargets[[column]],
                                      ordinalMappingTargets[column])
    }
    
    if (!is.null(testData))
    {
      testTargets[column] <- match(validTargets[[column]],
                                     ordinalMappingTargets[[column]])
    }
  }
  
  # now all non-numeric values should be taken care of and we can convert
  # the data frames to matrices (note that if there are still non-numeric values
  # present, validation will fail later on, we don't have to worry about that
  # here)
  dataSet@trainData <- as.matrix(trainData)
  dataSet@trainTargets <- as.matrix(trainTargets)
  
  if (!is.null(validData))
  {
    dataSet@validData <- as.matrix(validData)
    dataSet@validTargets <- as.matrix(validTargets)
  }
  
  if (!is.null(testData))
  {
    dataSet@testData <- as.matrix(testData)
    dataSet@testTargets <- as.matrix(testTargets)
  }
  
  return(dataSet)
}

#' Validates the \code{\link{DataSet}} for the given \code{\link{DArch}} object.
#' 
#' @details
#' Validates the data dimensions and data types of the \code{\link{DataSet}}.
#' 
#' @param dataSet \code{\link{DataSet}} to validate
#' @param darch \code{\link{DArch}} object to validate this
#'              \code{\link{DataSet}} against.
#'
#' @usage validateDataSet(dataSet, darch)
#' 
#' @return Whether the \code{\link{DataSet}} is valid.
#' 
#' @export
#' @docType methods
#' @rdname validateDataSet-methods
setGeneric("validateDataSet",function(dataSet, darch){standardGeneric("validateDataSet")})

#' @rdname validateDataSet-methods
#' @aliases validateDataSet,DataSet-method
setMethod(
  f="validateDataSet",
  signature="DataSet",
  definition=function(dataSet, darch){
    # first check wheter non-numeric data exists in the data
    if (!all(is.numeric(dataSet@trainData),
             is.numeric(dataSet@trainTargets),
             is.numeric(dataSet@validData) || is.null(dataSet@validData),
             is.numeric(dataSet@validTargets) || is.null(dataSet@validTargets),
             is.numeric(dataSet@testData) || is.null(dataSet@testData),
             is.numeric(dataSet@testTargets) || is.null(dataSet@testTargets)
             ))
    {
      flog.error(paste("DataSet is not numeric, please provide mappings for",
                       "ordinal data; nominal data is not supported."))
      
      return(F)
    }
    
    # test that all data dimensions are the same
    if (!all(ncol(dataSet@trainData) == ncol(dataSet@validData)
             || length(dataSet@validData) == 0,
             ncol(dataSet@trainData) == ncol(dataSet@testData)
             || length(dataSet@testData) == 0,
             ncol(dataSet@trainTargets) == ncol(dataSet@validTargets)
             || length(dataSet@validTargets) == 0,
             ncol(dataSet@trainTargets) == ncol(dataSet@testTargets)
             || length(dataSet@testTargets) == 0
             ))
    {
      flog.error("Varying number of columns in data or targets.")
      
      return(F)
    }
    
    # compare number of neurons in input and output layer to columns in data set
    rbmFirstLayer <- getRBMList(darch)[[1]]
    rbmLastLayer <- getRBMList(darch)[[length(getRBMList(darch))]]
    neuronsInput <- getNumVisible(rbmFirstLayer)
    neuronsOutput <- getNumHidden(rbmLastLayer)
    if (!all(neuronsInput == ncol(dataSet@trainData),
             neuronsOutput == ncol(dataSet@trainTargets)))
    {
      flog.error(paste("DataSet incompatible with DArch, number of neurons in",
                       "the first and last layer have to equal the number of",
                       "columns in the training data and targets,",
                       "respectively."))
      
      return(F)
    }
    
    return(T)
  }
)