# Copyright (C) 2015 darch2
# based on code by Martin Drees, copyright (C) 2013 Darch
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
#' Creates a new \code{\link{DArch}} object using the given configuration.
#' 
#' @return A new \code{\link{DArch}} instance.
#' 
#' @param config A list of configuration parameters.
#' @usage createDArchFromConfig(config)
#' @seealso \code{\link{getDefaultDArchConfig}}
#' 
#' @include darch.Getter.R
#' @include darch.R
#' @include darch.Setter.R
#' @include newDArch.R
#' @include rbm.Reset.R
#' 
#' @export
createDArchFromConfig <- function(config)
{
  config <- mergeDefaultDArchConfig(config)
  
  # check existence of required fields
  if (!all(!is.null(config[["darch.layers"]])))
  {
    stop("Missing required configuration values.")
  }
  
  numLayers = length(config[["darch.layers"]])
  
  darch <- newDArch(
    layers=config[["darch.layers"]],
    batchSize=config[["darch.batchSize"]],
    genWeightFunc=config[["darch.genWeightFunc"]],
    logLevel=config[["darch.logLevel"]])
  
  # Adjust RBM parameters
  rbmList <- getRBMList(darch)
  for(i in 1:length(rbmList)){
  setLearnRateWeights(rbmList[[i]]) <- config[["rbm.learnRateWeights"]]
  setLearnRateBiasVisible(rbmList[[i]]) <-  config[["rbm.learnRateBiasVisible"]]
  setLearnRateBiasHidden(rbmList[[i]]) <-  config[["rbm.learnRateBiasHidden"]]
  setWeightCost(rbmList[[i]]) <- config[["rbm.weightCost"]]
  setMomentum(rbmList[[i]]) <- config[["rbm.momentum"]]
  setFinalMomentum(rbmList[[i]]) <- config[["rbm.finalMomentum"]]
  setMomentumSwitch(rbmList[[i]]) <- config[["rbm.momentumSwitch"]]
  setVisibleUnitFunction(rbmList[[i]]) <- config[["rbm.visibleUnitFunction"]]
  setHiddenUnitFunction(rbmList[[i]]) <- config[["rbm.hiddenUnitFunction"]]
  setUpdateFunction(rbmList[[i]]) <- config[["rbm.updateFunction"]]
  setErrorFunction(rbmList[[i]]) <- config[["rbm.errorFunction"]]
  setGenWeightFunction(rbmList[[i]]) <- config[["rbm.genWeightFunction"]]
  rbmList[[i]] <- resetRBM(rbmList[[i]])
  }
  setRBMList(darch) <- rbmList
  
  # DArch configuration
  setFineTuneFunction(darch) <- config[["darch.fineTuneFunction"]]
  setMomentum(darch) <- config[["darch.momentum"]]
  setFinalMomentum(darch) <- config[["darch.finalMomentum"]]
  setMomentumSwitch(darch) <- config[["darch.momentumSwitch"]]
  setLearnRateWeights(darch) <- config[["darch.learnRateWeights"]]
  setLearnRateBiases(darch) <- config[["darch.learnRateBiases"]]
  setErrorFunction(darch) <- config[["darch.errorFunction"]]
  setDropoutInputLayer(darch) <- config[["darch.dropoutInput"]]
  setDropoutHiddenLayers(darch) <- config[["darch.dropoutHidden"]]
  
  # Layer configuration
  if (!is.null(config[["darch.layerFunction.maxout.poolSize"]]))
  {
    options(darch.unitFunction.maxout.poolSize=
              config[["darch.layerFunction.maxout.poolSize"]])
  }
  
  # activation function
  for (i in 1:(numLayers-1))
  {
    if (!is.null(config[["darch.layerFunctions"]][[as.character(i)]]))
    {
      setLayerFunction(darch,i) <-
        config[["darch.layerFunctions"]][[as.character(i)]]
    }
    else
    {
      setLayerFunction(darch,i) <- config[["darch.layerFunctionDefault"]]
    }
  }
  
  return(darch)
}

#' Returns the list of default configuration values.
#' 
#' @return List of default configuration values.
#' 
#' @usage getDefaultDArchConfig(config)
#' 
#' @include backpropagation.R
#' @include darchUnitFunctions.R
#' @include errorFunctions.R
#' @include generateWeights.R
#' @include rbmUnitFunctions.R
#' @include rbmUpdate.R
#' 
#' @export
getDefaultDArchConfig <- function()
{
  return(list(
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
    
    # DArch constructor arguments
    #darch.layers = c(2,3,1), # required
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
    darch.maxEpoch = 1
  ))
}

#' Merges the default configuration into the given configuration, effectively
#' adding missing values to the provided configuration list.
#' 
#' @return A complete configuration list.
#' 
#' @param config A(n incomplete) list of configuration parameters.
#' @seealso \code{\link{getDefaultDArchConfig}}
#' 
#' @export
mergeDefaultDArchConfig <- function(config)
{
  config.default <- getDefaultDArchConfig()
  
  # check unknown config-keys
  for (key in names(config))
  {
    if (!(key %in% names(config.default)))
    {
      flog.warn("Unknown configuration key %s", key)
    }
  }
  
  # add missing default values
  config <- merge.list(config, config.default)
  
  return(config)
}
#' Trains a given \code{\link{DArch}} object based on the given configuration.
#' 
#' @return The trained \code{\link{DArch}} instance.
#' 
#' @param darch A \code{\link{DArch}} instance.
#' @param dataSet A \code{\link{DataSet}} instance
#' @param config A list of configuration parameters.
#' @usage createDArchFromConfig(config)
#' @seealso \code{\link{getDefaultDArchConfig}}
#' 
#' @include darch.R
#' @include dataset.R
#' @include darch.Learn.R
#' 
#' @export
#' @rdname trainDArchFromConfig-methods
setGeneric(
name="trainDArchFromConfig",
def=function(darch,dataSet,config){standardGeneric("trainDArchFromConfig")}
)

#' @rdname trainDArchFromConfig-methods
#' @aliases DArch-method
setMethod(
  f="trainDArchFromConfig",
  signature=signature("DArch", "DataSet"),
  definition=function(darch,dataSet,config)
  {
    if (config[["rbm.maxEpoch"]] > 0)
    {
      preTrainDArch(darch, dataSet, maxEpoch=config[["rbm.maxEpoch"]],
                    numCD=config[["rbm.numCD"]])
    }
    
    darch <- fineTuneDArch(darch,dataSet,
                           maxEpoch=config[["darch.maxEpoch"]],
                           isBin=config[["darch.isBin"]],
                           isClass=config[["darch.isClass"]],
                           stopErr=config[["darch.stopErr"]],
                           stopClassErr=config[["darch.stopClassErr"]],
                           stopValidErr=config[["darch.stopValidErr"]],
                           stopValidClassErr=config[["darch.stopValidClassErr"]]
    )
    
    return(darch)
  })