# Copyright (C) 2013-2015 Martin Drees
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
#' fine-tuning algorithms. See \link{darch.default} for a full list of
#' parameters.
#' 
#' The darch package implements Deep Architecture Networks and restricted 
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
#' See \url{https://github.com/maddin79/darch} for further information,
#' documentation, and releases.
#' 
#' \tabular{ll}{ Package: \tab darch\cr Type: \tab Package\cr Version: \tab 
#' 0.10.0\cr Date: \tab 2015-11-12\cr License: \tab GPL-2 or later\cr 
#' LazyLoad: \tab yes\cr }
#' 
#' @import futile.logger methods stats
#'   
#' @author Martin Drees \email{mdrees@@stud.fh-dortmund.de} and contributors.
#' @keywords package Neural Networks darch Deep-Belief-Networks Restricted 
#'   Boltzmann Machines Contrastive Divergence Deep Architectures NN Neural Nets 
#'   Resilient Backpropagation Backpropagation Conjugate Gradient Dropout Maxout
#'   
#' @references Hinton, G. E., S. Osindero, Y. W. Teh, A fast learning algorithm 
#'   for deep belief nets, Neural Computation 18(7), S. 1527-1554, DOI: 
#'   10.1162/neco.2006.18.7.1527 2006.
#'   
#'   Hinton, G. E., R. R. Salakhutdinov, Reducing the dimensionality of data 
#'   with neural networks, Science 313(5786), S. 504-507, DOI: 
#'   10.1126/science.1127647, 2006.
#'   
#'   Hinton, Geoffrey E. et al. (2012). "Improving neural networks by
#'   preventing coadaptation of feature detectors". In: Clinical Orthopaedics
#'   and Related Research abs/1207.0580. URL : http://arxiv.org/abs/1207.0580.
#'   
#'   Goodfellow, Ian J. et al. (2013). "Maxout Networks". In: Proceedings of
#'   the 30th International Conference on Machine Learning, ICML 2013, Atlanta,
#'   GA, USA, 16-21 June 2013, pp. 1319-1327.
#'   URL: http://jmlr.org/proceedings/papers/v28/goodfellow13.html.
#'   
#'   Drees, Martin (2013). "Implementierung und Analyse von tiefen Architekturen
#'   in R". German. Master's thesis. Fachhochschule Dortmund.
#'   
#'   Rueckert, Johannes (2015). "Extending the Darch library for deep
#'   architectures". Project thesis. Fachhochschule Dortmund.
#'   URL: http://static.saviola.de/publications/rueckert_2015.pdf.
#'
#' @example examples/examples.R
#' @example examples/example.xor.R
#' @example examples/example.xor_nominal.R
#' @example examples/example.iris.R
#' @example examples/example.mnist.R
#'
#' @param x Input data.
#' @param ... additional parameters, see \link{darch.default}
#' @return Fitted \code{\linkS4class{DArch}} instance
#' @family darch interface functions
#' @export
darch <- function(x, ...)
{
  UseMethod("darch")
}

#' Fit a deep neural network using a formula and a single data frame or matrix.
#' 
#' @param x The formula specifying the model.
#' @param data Data frame or matrix.
#' @param dataValid Data frame or matrix of validation data.
#' @param ... additional parameters, see \link{darch.default}.
#'   
#' @seealso \link{model.frame}
#' @family darch interface functions
#' @export
darch.formula <- function(x, data, dataValid=NULL, ...)
{
  dataSet <- createDataSet(data=data, formula=x, ...)
  dataSetValid <- NULL
  
  if (!is.null(dataValid))
  {
    dataSetValid <- createDataSet(dataValid, T, dataSet)
  }
  
  res <- darch(dataSet, dataSetValid=dataSetValid, ...)
  
  return(res)
}

#' Create and train DArch object using a \code{\linkS4class{DataSet}}.
#'
#' Convencience method which calls \code{\link{darch.default}}
#' 
#' @param x \code{\linkS4class{DataSet}}.
#' @param ... Additional parameters for \link{darch.default}.
#' @return Fitted \code{\linkS4class{DArch}} instance.
#' 
#' @family darch interface functions
#' @export
darch.DataSet <- function(x, ...)
{
  res <- darch.default(x=NULL, y=NULL, ..., dataSet=x)
}

#' Fit deep neural network.
#' 
#' Fit deep neural network with optional pre-training and fine-tuning.
#' 
#' @param x Input data.
#' @param y Target data.
#' @param layers Vector containing one integer for the number of neurons of each
#'   layer. Defaults to c(\code{a}, 10, \code{b}), where \code{a} is the number
#'   of columns in the training data and \code{b} the number of columns in the
#'   targets.
#' @param ... additional parameters
#' @param xValid Validation input data.
#' @param yValid Validation target data.
#' @param scale Logical or logical vector indicating whether or which columns to
#'   scale.
#' @param normalizeWeights Logical indicating whether to normalize weights (L2
#'   norm = 1).
#' @param normalizeWeightsBound Upper bound on the L2 norm of incoming weight
#'  vectors. Used only if \code{normalizeWeights} is \code{TRUE}.
#' @param rbm.batchSize Pre-training batch size.
#' @param rbm.lastLayer \code{Numeric} indicating at which layer to stop the
#'  pre-training. Possible values include \code{0}, meaning that all layers
#'  are trained; positive integers, meaning to stop training after the RBM
#'  where \code{rbm.lastLayer} forms the visible layer; negative integers,
#'  meaning to stop the training at \code{rbm.lastLayer} RBMs from the top RBM.
#' @param rbm.learnRate Learning rate during pre-training.
#' @param rbm.learnRateScale The learn rates will be multiplied with this
#'  value after each epoch.
#' @param rbm.weightDecay Pre-training weight decay. Weights will be multiplied
#'  by (1 - \code{rbm.weightDecay}) prior to each weight update.
#' @param rbm.initialMomentum Initial momentum during pre-training.
#' @param rbm.finalMomentum Final momentum during pre-training.
#' @param rbm.momentumRampLength After how many epochs, relative to
#'  \code{rbm.numEpochs}, should the momentum reach \code{rbm.finalMomentum}?
#'  A value of 1 indicates that the \code{rbm.finalMomentum} should be reached
#'  in the final epoch, a value of 0.5 indicates that \code{rbm.finalMomentum}
#'  should be reached after half of the training is complete.
#' @param rbm.visibleUnitFunction Visible unit function during pre-training.
#' @param rbm.hiddenUnitFunction Hidden unit function during pre-training.
#' @param rbm.updateFunction Update function during pre-training.
#' @param rbm.errorFunction Error function during pre-training.
#' @param rbm.numCD Number of full steps for which contrastive divergence is
#'   performed.
#' @param rbm.numEpochs Number of pre-training epochs.
#' @param darch Existing \code{\linkS4class{DArch}} instance for which training
#'   is to be resumed.
#' @param darch.batchSize Batch size, i.e. the number of training samples that
#'   are presented to the network before weight updates are performed (for both
#'   pre-training and fine-tuning).
#' @param darch.bootstrap Logical indicating whether to use bootstrapping to
#'   create a training and validation data set from the given data.
#' @param darch.genWeightFunc Function to generate the initial weights of the
#'   DBN.
#' @param darch.logLevel Log level. \code{futile.logger::INFO} by default.
#' @param darch.fineTuneFunction Fine-tuning function.
#' @param darch.initialMomentum Initial momentum during fine-tuning.
#' @param darch.finalMomentum Final momentum during fine-tuning.
#' @param darch.momentumRampLength After how many epochs, relative to
#'  the \strong{overall} number of epochs trained, should the momentum reach
#'  \code{darch.finalMomentum}?
#'  A value of 1 indicates that the \code{darch.finalMomentum} should be reached
#'  in the final epoch, a value of 0.5 indicates that \code{darch.finalMomentum}
#'  should be reached after half of the training is complete. Note that this
#'  will lead to bumps in the momentum ramp if training is resumed with the
#'  same parameters for \code{darch.initialMomentum} and
#'  \code{darch.finalMomentum}. Set \code{darch.momentumRampLength} to 0 to
#'  avoid this problem when resuming training.
#' @param darch.learnRate Learning rate during fine-tuning.
#' @param darch.learnRateScale The learning rates are multiplied by this value
#'  after each epoch.
#' @param darch.errorFunction Error function during fine-tuning.
#' @param darch.dropoutInput Dropout rate on the network input.
#' @param darch.dropoutHidden Dropout rate on the hidden layers.
#' @param darch.dropout.dropConnect Whether to use DropConnect instead of
#'  dropout for the hidden layers. Will use \code{darch.dropoutHidden} as the
#'  DropConnect rate.
#' @param darch.dropout.momentumMatching How many iterations to perform during
#'  moment matching for dropout inference, 0 to disable moment matching.
#' @param darch.dropout.oneMaskPerEpoch Whether to generate a new mask for each
#'   batch (\code{FALSE}, default) or for each epoch (\code{TRUE}).
#' @param darch.layerFunction Layer function or vector of layer functions of
#'  length \code{number of layers} - 1. Note that the first entry signifies the
#'  layer function between layers 1 and 2, i.e. the output of layer 2. Layer 1
#'  does not have a layer function, since the input values are used directly.
#' @param darch.weightUpdateFunction Weight update function or vector of weight
#'  update functions, very similar to \code{darch.layerFunction}.
#' @param darch.layerFunction.maxout.poolSize Pool size for maxout units, when
#'   using the maxout acitvation function. See \link{maxoutUnitDerivative}.
#' @param darch.isClass Whether classification errors should be printed
#'   during fine-tuning. For this, network outputs are treated as binary.
#' @param darch.stopErr When the value of the error function is lower than or
#'   equal to this value, training is stopped.
#' @param darch.stopClassErr When the classification error is lower than or
#'   equal to this value, training is stopped (0..100).
#' @param darch.stopValidErr When the value of the error function on the
#'   validation data is lower than or equal to this value, training is stopped.
#' @param darch.stopValidClassErr When the classification error on the
#'   validation data is lower than or equal to this value, training is stopped
#'   (0..100).
#' @param darch.numEpochs Number of epochs of fine-tuning.
#' @param darch.retainData Logical indicating whether to store the training
#'  data in the \code{\linkS4class{DArch}} instance after training.
#' @param darch.returnBestModel Logical indicating whether to return the best
#'  model at the end of training, instead of the last.
#' @param dataSet \code{\linkS4class{DataSet}} instance, passed from
#'  darch.DataSet(), may be specified manually.
#' @param dataSetValid \code{\linkS4class{DataSet}} instance containing
#'  validation data.
#' @param gputools Logical indicating whether to use gputools for matrix
#'   multiplication, if available.
#' @return Fitted \code{\linkS4class{DArch}} instance
#' @family darch interface functions
#' @export
darch.default <- function(
  x,
  y,
  layers = NULL,
  ...,
  xValid = NULL,
  yValid = NULL,
  scale=F,
  normalizeWeights = F,
  normalizeWeightsBound = 15,
  # RBM configuration
  rbm.batchSize = 1,
  rbm.lastLayer = 0,
  rbm.learnRate = 1,
  rbm.learnRateScale = 1,
  rbm.weightDecay = .0002,
  rbm.initialMomentum = .5,
  rbm.finalMomentum = .9,
  rbm.momentumRampLength = 1,
  rbm.visibleUnitFunction = tanSigmUnitFunc,
  rbm.hiddenUnitFunction = tanSigmUnitFunc,
  rbm.updateFunction = rbmUpdate,
  rbm.errorFunction = mseError,
  # pre-train configuration.
  # higher values make everything much slower
  rbm.numCD = 1,
  rbm.numEpochs = 0,
  
  # DArch constructor arguments.
  # existing DArch instance
  darch = NULL,
  darch.batchSize = 1,
  darch.bootstrap = T,
  darch.genWeightFunc = generateWeightsRunif,
  # change to DEBUG if needed
  darch.logLevel = INFO,
  # DArch configuration
  darch.fineTuneFunction = backpropagation,
  darch.initialMomentum = .5,
  darch.finalMomentum = .9,
  darch.momentumRampLength = 1,
  # higher for sigmoid activation
  darch.learnRate = 1,
  darch.learnRateScale = 1,
  darch.errorFunction = mseError,
  darch.dropoutInput = 0.,
  darch.dropoutHidden = 0.,
  darch.dropout.dropConnect = F,
  darch.dropout.momentMatching = 0,
  darch.dropout.oneMaskPerEpoch = F,
  darch.dither = F,
  darch.weightDecay = 0,
  # layer configuration.
  # activation function
  # custom activation functions
  darch.layerFunction = tanSigmoidUnitDerivative,
  darch.layerFunction.maxout.poolSize = NULL,
  darch.weightUpdateFunction = weightDecayWeightUpdate,
  # fine-tune configuration
  darch.isClass = T,
  darch.stopErr = -Inf,
  darch.stopClassErr = -Inf,
  darch.stopValidErr = -Inf,
  darch.stopValidClassErr = -Inf,
  darch.numEpochs = 0,
  darch.retainData = T,
  darch.returnBestModel = T,
  dataSet = NULL,
  dataSetValid = NULL,
  gputools = T)
{  
  params <- c(list(...), mget(ls()))
  params[["matMult"]] <- `%*%`
  
  if (gputools)
  {
    if ((length(find.package("gputools", quiet=T)) == 0))
    {
      futile.logger::flog.warn(paste("gputools package not available, using",
        "CPU matrix multiplication."))
    }
    else
    {
      params[["matMult"]] <- gputools::gpuMatMult
    }
  }
  
  # create data set if none was provided
  if (is.null(dataSet))
  {
    dataSet <- createDataSet(data=x, targets=y, scale=scale)
    
    if (!is.null(xValid))
    {
      dataSetValid <- createDataSet(data=xValid, targets=yValid,
                                    dataSet=dataSet)
    }
  }
  
  # create default layers parameter if missing
  if (is.null(layers))
  {
    layers = c(ncol(dataSet@data), 10, ncol(dataSet@targets))
  }
  
  numLayers = length(layers)
  
  # TODO add parameter for re-configuration of DArch instance? update function?
  if (is.null(darch))
  {
    flog.info("Creating new DArch instance.")
    
    darch <- newDArch(
      layers=layers,
      batchSize=rbm.batchSize, # batch size for RBMs is set upon creation
      genWeightFunc=darch.genWeightFunc,
      logLevel=darch.logLevel)
    
    # Adjust RBM parameters
    rbmList <- getRBMList(darch)
    for(i in 1:length(rbmList))
    {
      rbmList[[i]]@initialLearnRate <- rbm.learnRate
      rbmList[[i]]@learnRate <- rbm.learnRate
      rbmList[[i]]@learnRateScale <- rbm.learnRateScale
      rbmList[[i]]@weightDecay <- rbm.weightDecay
      setInitialMomentum(rbmList[[i]]) <- rbm.initialMomentum
      setFinalMomentum(rbmList[[i]]) <- rbm.finalMomentum
      rbmList[[i]]@momentumRampLength <- rbm.momentumRampLength
      setVisibleUnitFunction(rbmList[[i]]) <- rbm.visibleUnitFunction
      setHiddenUnitFunction(rbmList[[i]]) <- rbm.hiddenUnitFunction
      setUpdateFunction(rbmList[[i]]) <- rbm.updateFunction
      setErrorFunction(rbmList[[i]]) <- rbm.errorFunction
      setNormalizeWeights(rbmList[[i]]) <- normalizeWeights
      rbmList[[i]]@normalizeWeightsBound <- normalizeWeightsBound
      rbmList[[i]]@epochsScheduled <- rbm.numEpochs
      rbmList[[i]] <- resetRBM(rbmList[[i]])
    }
    setRBMList(darch) <- rbmList
    
    # DArch configuration
    setFineTuneFunction(darch) <- darch.fineTuneFunction
    setInitialMomentum(darch) <- darch.initialMomentum
    setFinalMomentum(darch) <- darch.finalMomentum
    darch@momentumRampLength <- darch.momentumRampLength
    darch@initialLearnRate <- darch.learnRate
    darch@learnRate <- darch.learnRate
    darch@learnRateScale <- darch.learnRateScale
    setErrorFunction(darch) <- darch.errorFunction
    setDropoutInputLayer(darch) <- darch.dropoutInput
    setDropoutHiddenLayers(darch) <- darch.dropoutHidden
    setDropoutOneMaskPerEpoch(darch) <- darch.dropout.oneMaskPerEpoch
    darch@dropConnect <- darch.dropout.dropConnect
    darch@dither <- darch.dither
    setNormalizeWeights(darch) <- normalizeWeights
    darch@normalizeWeightsBound <- normalizeWeightsBound
    
    layerFunctions <- (if (length(darch.layerFunction) == 1)
      replicate(length(layers), darch.layerFunction) else
      darch.layerFunction)
    weightUpdateFunctions <- (if (length(darch.weightUpdateFunction) == 1)
      replicate(length(layers), darch.weightUpdateFunction) else
        darch.weightUpdateFunction)
    
    # per-layer configuration
    for (i in 1:(numLayers-1))
    {
      # Layer functions
      setLayerFunction(darch, i) <- layerFunctions[[i]]
      
      # Weight update functions
      setWeightUpdateFunction(darch, i) <- weightUpdateFunctions[[i]]
    }
  }
  
  darch@params <- params
  
  if (rbm.numEpochs > 0)
  {
    darch <- preTrainDArch(darch, dataSet, numEpochs = rbm.numEpochs,
                           numCD = rbm.numCD,
                           lastLayer = rbm.lastLayer, ...)
  }
  
  if (darch.numEpochs > 0)
  {
    darch@epochsScheduled <- darch@epochs + darch.numEpochs
    # TODO move into dataset validation?
    if (darch.isClass && is.null(dataSet@targets))
    {
      flog.error("darch.isClass was set to TRUE while no targets were ",
                 "provided, aborting.")
      stop("Invalid combination of parameters encountered.")
    }
    
    setBatchSize(darch) <- darch.batchSize
    darch <- fineTuneDArch(darch, dataSet, dataSetValid=dataSetValid,
                         numEpochs=darch.numEpochs,
                         bootstrap=darch.bootstrap,
                         isClass=darch.isClass,
                         stopErr=darch.stopErr,
                         stopClassErr=darch.stopClassErr,
                         stopValidErr=darch.stopValidErr,
                         stopValidClassErr=darch.stopValidClassErr, ...)
  }
  
  if (!darch.retainData)
  {
    darch@dataSet@data <- darch@dataSet@data[1,, drop = F]
    darch@dataSet@targets <- darch@dataSet@targets[1,, drop = F]
  }
  
  return(darch)
}

# TODO further parameters like na.action etc.

#' Forward-propagate data.
#' 
#' Forward-propagate given data through the deep neural network.
#' 
#' @param object \code{\linkS4class{DArch}} instance
#' @param ... Further parameters, not used.
#' @param newdata New data to predict, \code{NULL} to return latest network
#'   output
#' @param type Output type, one of: \code{raw}, \code{bin}, \code{class}.
#'   \code{raw} returns the network output (as is, or with scaling reversed, if
#'   the input data were scaled), \code{bin} returns \code{1} for every network
#'   output \code{>0.5}, \code{0} otherwise, and \code{class} returns \code{1}
#'   for the output unit with the highest activation, otherwise \code{0}.
#'   Additionally, when using \code{class}, class labels are returned when
#'   available.
#' @return Vector or matrix of networks outputs, output type depending on the
#'   \code{type} parameter
#' @export
#' @aliases predict.darch
#' @family darch interface functions
predict.DArch <- function (object, ..., newdata = NULL, type="raw")
{
  darch <- object
  
  if (is.null(newdata))
  {
    dataSet <- darch@dataSet
  }
  else
  {
    dataSet <- createDataSet(data=newdata, targets=F, dataSet=darch@dataSet)
  }
  
  darch <- getExecuteFunction(darch)(darch, dataSet@data)
  execOut <- getExecOutput(darch)
  
  if (any(dataSet@parameters$scaled) && !is.null(dataSet@parameters$yscale))
  {
    execOutScaled <- (execOut * dataSet@parameters$yscale$"scaled:scale"
                      + dataSet@parameters$yscale$"scaled:center")
  }
  else
  {
    execOutScaled <- execOut
  }
  
  return(switch(type, raw = execOutScaled, bin = (execOut > .5)*1,
          class =
          {
            if (is.null(dataSet@parameters$ylevels))
            {
              if (ncol(execOut) > 1)
                diag(ncol(execOut))[max.col(execOut, ties.method="first"),]
              else (execOut > .5)*1
            }
            else
            {
              if (ncol(execOut) > 1)
                as.matrix(dataSet@parameters$ylevels[max.col(execOut,
                  ties.method="first")])
              else as.matrix(dataSet@parameters$ylevels[1 + (execOut > .5)])
            }
          }))
}

test.DArch <- function(darch, data=NULL, targets=NULL)
{
  if (is.null(data))
  {
    data <- darch@dataSet@data
    targets <- if (is.null(targets)) darch@dataSet@targets else targets
  }
  
  testDArch(darch, data, targets, "test data", T)
}

#' Print \linkS4class{DArch} details.
#'
#' Print verbose information about a \linkS4class{DArch} instance.
#' 
#' @param x \code{\linkS4class{DArch}} instance
#' @param ... Further parameters, not used.
#' @export
#' @aliases print.darch
#' @family darch interface functions
print.DArch <- function(x, ...)
{
  darch <- x
  
  # Find function in a list of function names by comparing function bodies;
  # returns function name if found, its body otherwise
  findFunctionName <- function(needle)
  {
    needleBody <- body(needle)
    needleBodyLength <- length(needleBody)
    
    for (functionName in utils::lsf.str("package:darch"))
    {
      functionBody <- body(functionName)
      
      if (needleBodyLength == length(functionBody)
          && length(intersect(as.character(needleBody), as.character(functionBody)))
          == needleBodyLength)
      { 
        return (functionName)
      }
    }
    
    return (deparse(needle))
  }
  
  # helper function for parameter concatenation
  pasteArg <- function(...)
  {
    paste0(paste(..., sep=" = "), "\n")
  }
  
  cat("darch() parameters (see ?darch for documentation).\n")
    
  numLayers <- length(getLayers(darch))

  if (length(getRBMList(darch)) > 0)
  {
    rbm <- getRBMList(darch)[[1]]
    cat(pasteArg("rbm.initialLearnRate", rbm@initialLearnRate))
    cat(pasteArg("rbm.learnRate", rbm@learnRate * (1 - getMomentum(rbm))))
    cat(pasteArg("rbm.weightDecay", rbm@weightDecay))
    cat(pasteArg("rbm.initialMomentum", getInitialMomentum(rbm)))
    cat(pasteArg("rbm.finalMomentum", getFinalMomentum(rbm)))
    cat(pasteArg("rbm.momentum", getMomentum(rbm)))
    cat(pasteArg("rbm.momentumRampLength", rbm@momentumRampLength))
    cat(pasteArg("rbm.visibleUnitFunction", findFunctionName(rbm@visibleUnitFunction)))
    cat(pasteArg("rbm.hiddenUnitFunction", findFunctionName(rbm@hiddenUnitFunction)))
    cat(pasteArg("rbm.updateFunction", findFunctionName(rbm@updateFunction)))
    cat(pasteArg("rbm.errorFunction", findFunctionName(getErrorFunction(rbm))))
    cat(pasteArg("rbm.genWeightFunction", findFunctionName(getGenWeightFunction(rbm))))
  }
  
  layerSizes = c()
  layerFunctions = c()
  for (i in c(1:numLayers))
  {
    layerSizes <- c(layerSizes, nrow(getLayerWeights(darch, i))-1)
    layerFunctions <- c(layerFunctions, findFunctionName(getLayerFunction(darch, i)))
  }
  layerSizes <- c(layerSizes, ncol(getLayerWeights(darch, numLayers)))
  
  cat(pasteArg("layers", deparse(layerSizes)))
  cat(pasteArg("normalizeWeights", getNormalizeWeights(darch)))
  cat(pasteArg("darch.batchSize", getBatchSize(darch)))
  cat(pasteArg("darch.initialMomentum", getInitialMomentum(darch)))
  cat(pasteArg("darch.finalMomentum", getFinalMomentum(darch)))
  cat(pasteArg("darch.momentum", getMomentum(darch)))
  cat(pasteArg("darch.momentumRampLength", darch@momentumRampLength))
  cat(pasteArg("darch.initialLearnRate", darch@initialLearnRate))
  cat(pasteArg("darch.learnRate", darch@learnRate * (1 - getMomentum(darch))))
  cat(pasteArg("darch.learnRateScale", darch@learnRateScale))
  cat(pasteArg("darch.weightDecay", darch@weightDecay))
  cat(pasteArg("darch.dropoutInput", getDropoutInputLayer(darch)))
  cat(pasteArg("darch.dropoutHidden", getDropoutHiddenLayers(darch)))
  cat(pasteArg("darch.layerFunction.maxout.poolSize",
    getParam("darch.unitFunction.maxout.poolSize", NULL)))
  cat(pasteArg("darch.logLevel", futile.logger::flog.threshold()))
  cat(pasteArg("darch.genWeightFunc",
               findFunctionName(getGenWeightFunction(darch))))
  cat(pasteArg("darch.fineTuneFunction",
               findFunctionName(getFineTuneFunction(darch))))
  cat(pasteArg("darch.errorFunction",
               findFunctionName(getErrorFunction(darch))))
  cat(pasteArg("darch.layerFunctions", deparse(layerFunctions)))
  
  cat("Pre-train parameters:\n")
  print(darch@preTrainParameters)
  
  cat("Fine-tuning parameters:\n")
  print(darch@fineTuningParameters)
  
  cat(pasteArg("darch.retainData", nrow(darch@dataSet@data)>1))
  cat("Data set parameters:\n")
  print(darch@dataSet@parameters)
}