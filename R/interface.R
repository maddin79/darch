# Copyright (C) 2013-2015 darch
#
# Based on code from nnet.
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

#' Fit a deep neural network.
#' 
#' Fit a deep neural network with optional pre-training and one of various 
#' fine-tuning algorithms.
#' 
#' 
#' The darch-package implements Deep Architecture Networks and restricted 
#' Boltzmann machines.
#' 
#' The creation of this package is motivated by the papers from G. Hinton et. 
#' al. from 2006 (see references for details) and from the MATLAB source code 
#' developed in this context. This package provides the possibility to generate 
#' deep architecture networks (darch) like the deep belief networks from Hinton 
#' et. al.. The deep architectures can then be trained with the contrastive 
#' divergence method. After this pre-training it can be fine tuned with several 
#' learning methods like backpropagation, resilient backpropagation and 
#' conjugate gradients as well as more recent techniques like dropout and
#' maxout.
#' 
#' \tabular{ll}{ Package: \tab darch\cr Type: \tab Package\cr Version: \tab 
#' 0.9.2.9000\cr Date: \tab 2015-08-02\cr License: \tab GPL-2 or later\cr 
#' LazyLoad: \tab yes\cr }
#' 
#' @import ff futile.logger methods
#'   
#' @author Martin Drees \email{mdrees@@stud.fh-dortmund.de} and contributors.
#'   
#'   Maintainer: Johannes Rueckert \email{j.rueckert@@gmx.net}
#' @name darch
#' @keywords package Neural Networks darch Deep-Belief-Networks Restricted 
#'   Bolzmann Machines Contrastive Divergence Deep Architectures NN Neural Nets 
#'   Resilient Backpropagation Backpropagation Conjugate Gradient Dropout Maxout
#'   
#'   TODO add references
#' @references Hinton, G. E., S. Osindero, Y. W. Teh, A fast learning algorithm 
#'   for deep belief nets, Neural Computation 18(7), S. 1527-1554, DOI: 
#'   10.1162/neco.2006.18.7.1527 2006.
#'   
#'   Hinton, G. E., R. R. Salakhutdinov, Reducing the dimensionality of data 
#'   with neural networks, Science 313(5786), S. 504-507, DOI: 
#'   10.1126/science.1127647, 2006.
#'   
#' @examples source(paste0(system.file(package="darch"), "/examples/examples.R"))
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
  rbm.maxEpoch = 0,
  
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
  darch.dropoutInput = 0,
  darch.dropoutHidden = 0,
  # layer configuration.
  # activation function
  darch.layerFunctionDefault = linearUnitDerivative,
  # custom activation functions
  # TODO offset +1, otherwise entry i will affect layer i+1
  darch.layerFunctions = list(),
  # maps to the global option darch.unitFunction.maxout.poolSize
  darch.layerFunction.maxout.poolSize =
    getOption("darch.unitFunction.maxout.poolSize", NULL),
  # fine-tune configuration
  darch.isBin = F,
  darch.isClass = T,
  darch.stopErr = -Inf,
  darch.stopClassErr = 101,
  darch.stopValidErr = -Inf,
  darch.stopValidClassErr = 101,
  darch.maxEpoch = 0,
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
  
  if (darch.maxEpoch > 0)
  {
    darch <- fineTuneDArch(darch,dataSetTrain,
                         additionalDataSets=additionalDataSets,
                         maxEpoch=darch.maxEpoch,
                         isBin=darch.isBin,
                         isClass=darch.isClass,
                         stopErr=darch.stopErr,
                         stopClassErr=darch.stopClassErr,
                         stopValidErr=darch.stopValidErr,
                         stopValidClassErr=darch.stopValidClassErr)
  }
  
  return(darch)
}

# TODO further parameters like na.action etc.

#' Forward-propagate data.
#' 
#' Forward-propagate given data through the deep neural network.
#' 
#' @param darch \code{\linkS4class{DArch}} instance
#' @param newdata New data to predict,  to return latest network output
#' @param type Output type, one of: \code{raw}, \code{bin}, \code{class}.
#' @return Vector or matrix of networks outputs, output type depending on the
#'   \code{type} parameter
#' @export
#' @aliases predict.darch
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
  
  return(switch(type, raw = execOut, bin = (execOut>.5)*1,
          class =
          {
            if (is.null(dataSet@parameters$ylevels))
            {
              flog.error("Inappropriate fit for class.")
              stop("Unrecoverable error.")
            }
            
            if (ncol(execOut) > 1) as.matrix(dataSet@parameters$ylevels[max.col(execOut)])
            else as.matrix(dataSet@parameters$ylevels[1 + (execOut > .5)])
          }))
}