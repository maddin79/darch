# Copyright (C) 2015 darch2
#
# Based on code from nnet.
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
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

#' Fit a deep neural network with optional pre-training and one of various
#' fine-tuning algorithms.
#' 
#' TODO details
#' 
#' @export
darch <- function(x, ...)
{
  UseMethod("darch")
}

#' Fit a deep neural network using a formula and a single data frame or matrix.
#' 
#' @param formula The formula specifying the model.
#' @param data Data frame or matrix.
#' @param ... additional parameters
#'   
#' @seealso \code{\link{model.frame}}
#' @seealso \code{\link{createDataSet.formula}}
#' @export
darch.formula <- function(formula, data, ...)
{
  dataSet <- createDataSet(data=data, formula=formula, ...)
  
  res <- darch(dataSet, ...)
  
  return(res)
}

#' Create and train DArch object using a \code{\linkS4class{DataSet}}.
#'
#' Convencience method which calls \code{\link{darch.default}}
#' 
#' @param dataSet \code{\linkS4class{DataSet}}
#' @return Fitted \code{\linkS4class{DArch}} instance
#' 
#' @seealso \code{\link{darch.default}}
#' @export
darch.DataSet <- function(dataSet, ...)
{
  res <- darch.default(x=NULL, y=NULL, ..., dataSet=dataSet)
}

#' Fit deep neural network with optional pre-training and fine-tuning.
#' 
#' TODO documentation parameters / details
#' 
#' @return Fitted \code{\linkS4class{DArch}} instance
#' @export
darch.default <- function(
  x,
  y,
  darch.layers,
  # RBM configuration
  rbm.learnRateWeights = .1,
  rbm.learnRateBiasVisible = .1,
  rbm.learnRateBiasHidden = .1,
  rbm.weightCost = .0002,
  rbm.momentum = .9,
  rbm.finalMomentum = .5,
  rbm.momentumSwitch = 5,
  rbm.visibleUnitFunction = sigmUnitFunc,
  rbm.hiddenUnitFunction = sigmUnitFuncSwitch,
  rbm.updateFunction = rbmUpdate,
  rbm.errorFunction = mseError,
  rbm.genWeightFunction = generateWeights,
  # pre-train configuration.
  # higher values make everything much slower
  rbm.numCD = 1,
  rbm.maxEpoch = 1,
  
  # DArch constructor arguments.
  # existing DArch instance
  darch = NULL,
  darch.batchSize = 1,
  darch.genWeightFunc = generateWeights,
  # change to DEBUG if needed
  darch.logLevel = INFO,
  # DArch configuration
  darch.fineTuneFunction = backpropagation,
  darch.momentum = .9,
  darch.finalMomentum = .5,
  darch.momentumSwitch = 5,
  # higher for sigmoid activation
  darch.learnRateWeights = .001,
  darch.learnRateBiases = .001,
  darch.errorFunction = mseError,
  darch.dropoutInput = 0.,
  darch.dropoutHidden = 0.,
  # layer configuration.
  # activation function
  darch.layerFunctionDefault = sigmoidUnitDerivative,
  # custom activation functions
  darch.layerFunctions = list(),
  # maps to the global option darch.unitFunction.maxout.poolSize
  darch.layerFunction.maxout.poolSize = NULL,
  # fine-tune configuration
  darch.isBin = F,
  darch.isClass = T,
  darch.stopErr = -Inf,
  darch.stopClassErr = 101,
  darch.stopValidErr = -Inf,
  darch.stopValidClassErr = 101,
  darch.maxEpoch = 1,
  dataSetTrain = NULL,
  additionalDataSets = list())
{
  # create data set if none was provided
  if (is.null(dataSetTrain))
  {
    dataSetTrain <- createDataSet(data=x, targets=y)
  }
  
  # check existence of required fields
  if (!all(!is.null(darch.layers)))
  {
    stop("Missing required configuration parameters.")
  }
  
  numLayers = length(darch.layers)
  
  # TODO add parameter for re-configuration of DArch instance? update function?
  if (is.null(darch))
  {
    darch <- newDArch(
      layers=darch.layers,
      batchSize=darch.batchSize,
      genWeightFunc=darch.genWeightFunc,
      logLevel=darch.logLevel)
    
    # Adjust RBM parameters
    rbmList <- getRBMList(darch)
    for(i in 1:length(rbmList)){
      setLearnRateWeights(rbmList[[i]]) <- rbm.learnRateWeights
      setLearnRateBiasVisible(rbmList[[i]]) <-  rbm.learnRateBiasVisible
      setLearnRateBiasHidden(rbmList[[i]]) <-  rbm.learnRateBiasHidden
      setWeightCost(rbmList[[i]]) <- rbm.weightCost
      setMomentum(rbmList[[i]]) <- rbm.momentum
      setFinalMomentum(rbmList[[i]]) <- rbm.finalMomentum
      setMomentumSwitch(rbmList[[i]]) <- rbm.momentumSwitch
      setVisibleUnitFunction(rbmList[[i]]) <- rbm.visibleUnitFunction
      setHiddenUnitFunction(rbmList[[i]]) <- rbm.hiddenUnitFunction
      setUpdateFunction(rbmList[[i]]) <- rbm.updateFunction
      setErrorFunction(rbmList[[i]]) <- rbm.errorFunction
      setGenWeightFunction(rbmList[[i]]) <- rbm.genWeightFunction
      rbmList[[i]] <- resetRBM(rbmList[[i]])
    }
    setRBMList(darch) <- rbmList
    
    # DArch configuration
    setFineTuneFunction(darch) <- darch.fineTuneFunction
    setMomentum(darch) <- darch.momentum
    setFinalMomentum(darch) <- darch.finalMomentum
    setMomentumSwitch(darch) <- darch.momentumSwitch
    setLearnRateWeights(darch) <- darch.learnRateWeights
    setLearnRateBiases(darch) <- darch.learnRateBiases
    setErrorFunction(darch) <- darch.errorFunction
    setDropoutInputLayer(darch) <- darch.dropoutInput
    setDropoutHiddenLayers(darch) <- darch.dropoutHidden
    
    # Layer configuration
    if (!is.null(darch.layerFunction.maxout.poolSize))
    {
      options(darch.unitFunction.maxout.poolSize=
                darch.layerFunction.maxout.poolSize)
    }
    
    # activation function
    for (i in 1:(numLayers-1))
    {
      if (!is.null(darch.layerFunctions[[as.character(i)]]))
      {
        setLayerFunction(darch,i) <-
          darch.layerFunctions[[as.character(i)]]
      }
      else
      {
        setLayerFunction(darch,i) <- darch.layerFunctionDefault
      }
    }
  }
  
  if (rbm.maxEpoch > 0)
  {
    darch <- preTrainDArch(darch, dataSetTrain, maxEpoch=rbm.maxEpoch,
                           numCD=rbm.numCD)
  }
  
  darch <- fineTuneDArch(darch,dataSetTrain,
                         additionalDataSets=additionalDataSets,
                         maxEpoch=darch.maxEpoch,
                         isBin=darch.isBin,
                         isClass=darch.isClass,
                         stopErr=darch.stopErr,
                         stopClassErr=darch.stopClassErr,
                         stopValidErr=darch.stopValidErr,
                         stopValidClassErr=darch.stopValidClassErr
  )
  
  return(darch)
}

# TODO further parameters like na.action etc.
#' Forward-propagate given data through the deep neural network.
#' 
#' TODO details
#'
#' @param darch \code{linkS4class{DArch}} instance
#' @param newdata New data to predict,  to return latest network output
#' @return Vector of networks outputs, depending on the \code{type} parameter
#' @export
predict.DArch <- function (darch, newdata = NULL, type="raw")
{
  if (is.null(newdata))
  {
    newdata <- darch@dataSet@data
  }
  
  dataSet <- createDataSet(data=newdata, targets=F, dataSet=darch@dataSet)
  validateDataSets(list("predict"=dataSet), darch) 
  
  darch <- getExecuteFunction(darch)(darch,dataSet@data)
  execOut <- getExecOutput(darch)
  
  # TODO class
  return(switch(type, raw = execOut, bin = (execOut>.5)*1))
}