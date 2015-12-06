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

# create the darch environment, used to determine which matrix multiplication
# function to use
darch.env <- new.env()
assign("matMult", `%*%`, darch.env)

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
#' @import ff futile.logger methods stats
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
#' @param rbm.batchSize Pre-training batch size.
#' @param rbm.trainOutputLayer Logical indicating whether to train the output
#'   layer RBM as well (only useful for unsupervised fine-tuning).
#' @param rbm.learnRateWeights Learn rate for the weights during pre-training.
#' @param rbm.learnRateBiasVisible Learn rate for the weights of the visible
#'   bias.
#' @param rbm.learnRateBiasHidden Learn rate for the weights of the hidden bias.
#' @param rbm.weightCost Pre-training weight cost. Higher values result in
#'   lower weights.
#' @param rbm.initialMomentum Initial momentum during pre-training.
#' @param rbm.finalMomentum Final momentum during pre-training.
#' @param rbm.momentumSwitch Epoch during which momentum is switched from the
#'   initial to the final value.
#' @param rbm.visibleUnitFunction Visible unit function during pre-training.
#' @param rbm.hiddenUnitFunction Hidden unit function during pre-training.
#' @param rbm.updateFunction Update function during pre-training.
#' @param rbm.errorFunction Error function during pre-training.
#' @param rbm.genWeightFunction Function to generate the initial RBM weights.
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
#' @param darch.momentumSwitch Epoch at which to switch from the intial to the
#'   final momentum value.
#' @param darch.learnRateWeights Learn rate for the weights during fine-tuning.
#' @param darch.learnRateBiases Learn rate for the biases during fine-tuning.
#' @param darch.errorFunction Error function during fine-tuning.
#' @param darch.dropoutInput Dropout rate on the network input.
#' @param darch.dropoutHidden Dropout rate on the hidden layers.
#' @param darch.dropoutOneMaskPerEpoch Whether to generate a new mask for each
#'   batch (\code{FALSE}, default) or for each epoch (\code{TRUE}).
#' @param darch.layerFunctionDefault Default activation function for the DBN
#'   layers.
#' @param darch.layerFunctions A list of activation functions, names() should be
#'   a character vector of layer numbers. Note that layer 1 signifies the layer
#'   function between layers 1 and 2, i.e. the output of layer 2. Layer 1 does
#'   not have a layer function, since the input values are used directly.
#' @param darch.layerFunction.maxout.poolSize Pool size for maxout units, when
#'   using the maxout acitvation function.
#' @param darch.isBin Whether network outputs are to be treated as binary
#'   values.
#' @param darch.isClass Whether classification errors should be printed
#'   during fine-tuning.
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
#'   data in the \code{\linkS4class{DArch}} instance after training.
#' @param dataSet \code{\linkS4class{DataSet}} instance, passed from
#'   darch.DataSet(), may be specified manually.
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
  # RBM configuration
  rbm.batchSize = 1,
  rbm.trainOutputLayer = T,
  rbm.learnRateWeights = .1,
  rbm.learnRateBiasVisible = .1,
  rbm.learnRateBiasHidden = .1,
  rbm.weightCost = .0002,
  rbm.initialMomentum = .5,
  rbm.finalMomentum = .9,
  rbm.momentumSwitch = 5,
  rbm.visibleUnitFunction = sigmUnitFunc,
  rbm.hiddenUnitFunction = sigmUnitFuncSwitch,
  rbm.updateFunction = rbmUpdate,
  rbm.errorFunction = mseError,
  rbm.genWeightFunction = generateWeights,
  # pre-train configuration.
  # higher values make everything much slower
  rbm.numCD = 1,
  rbm.numEpochs = 0,
  
  # DArch constructor arguments.
  # existing DArch instance
  darch = NULL,
  darch.batchSize = 1,
  darch.bootstrap = T,
  darch.genWeightFunc = generateWeights,
  # change to DEBUG if needed
  darch.logLevel = INFO,
  # DArch configuration
  darch.fineTuneFunction = backpropagation,
  darch.initialMomentum = .5,
  darch.finalMomentum = .9,
  darch.momentumSwitch = 5,
  # higher for sigmoid activation
  darch.learnRateWeights = .1,
  darch.learnRateBiases = .1,
  darch.errorFunction = mseError,
  darch.dropoutInput = 0.,
  darch.dropoutHidden = 0.,
  darch.dropoutOneMaskPerEpoch = F,
  # layer configuration.
  # activation function
  darch.layerFunctionDefault = sigmoidUnitDerivative,
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
  darch.stopClassErr = -Inf,
  darch.stopValidErr = -Inf,
  darch.stopValidClassErr = -Inf,
  darch.numEpochs = 0,
  darch.retainData = T,
  dataSet = NULL,
  dataSetValid = NULL,
  gputools = T)
{
  if (gputools)
  {
    if ((length(find.package("gputools", quiet=T)) == 0))
    {
      futile.logger::flog.warn(paste("gputools package not available, using",
        "CPU matrix multiplication."))
    }
    else
    {
      assign("matMult", gputools::gpuMatMult, darch.env)
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
  
  # check existence of required fields
  if (is.null(layers))
  {
    layers = c(ncol(dataSet@data), 10, ncol(dataSet@targets))
  }
  
  numLayers = length(layers)
  
  # TODO add parameter for re-configuration of DArch instance? update function?
  if (is.null(darch))
  {
    darch <- newDArch(
      layers=layers,
      batchSize=rbm.batchSize, # batch size for RBMs is set upon creation
      genWeightFunc=darch.genWeightFunc,
      logLevel=darch.logLevel)
    
    # Adjust RBM parameters
    rbmList <- getRBMList(darch)
    for(i in 1:length(rbmList)){
      setLearnRateWeights(rbmList[[i]]) <- rbm.learnRateWeights
      setLearnRateBiasVisible(rbmList[[i]]) <-  rbm.learnRateBiasVisible
      setLearnRateBiasHidden(rbmList[[i]]) <-  rbm.learnRateBiasHidden
      setWeightCost(rbmList[[i]]) <- rbm.weightCost
      setInitialMomentum(rbmList[[i]]) <- rbm.initialMomentum
      setFinalMomentum(rbmList[[i]]) <- rbm.finalMomentum
      setMomentumSwitch(rbmList[[i]]) <- rbm.momentumSwitch
      setVisibleUnitFunction(rbmList[[i]]) <- rbm.visibleUnitFunction
      setHiddenUnitFunction(rbmList[[i]]) <- rbm.hiddenUnitFunction
      setUpdateFunction(rbmList[[i]]) <- rbm.updateFunction
      setErrorFunction(rbmList[[i]]) <- rbm.errorFunction
      setGenWeightFunction(rbmList[[i]]) <- rbm.genWeightFunction
      setNormalizeWeights(rbmList[[i]]) <- normalizeWeights
      rbmList[[i]] <- resetRBM(rbmList[[i]])
    }
    setRBMList(darch) <- rbmList
    
    # DArch configuration
    setFineTuneFunction(darch) <- darch.fineTuneFunction
    setInitialMomentum(darch) <- darch.initialMomentum
    setFinalMomentum(darch) <- darch.finalMomentum
    setMomentumSwitch(darch) <- darch.momentumSwitch
    setLearnRateWeights(darch) <- darch.learnRateWeights
    setLearnRateBiases(darch) <- darch.learnRateBiases
    setErrorFunction(darch) <- darch.errorFunction
    setDropoutInputLayer(darch) <- darch.dropoutInput
    setDropoutHiddenLayers(darch) <- darch.dropoutHidden
    setDropoutOneMaskPerEpoch(darch) <- darch.dropoutOneMaskPerEpoch
    setNormalizeWeights(darch) <- normalizeWeights
    
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
  
  if (rbm.numEpochs > 0)
  {
    darch <- preTrainDArch(darch, dataSet, numEpochs = rbm.numEpochs,
                           numCD = rbm.numCD,
                           trainOutputLayer = rbm.trainOutputLayer, ...)
  }
  
  if (darch.numEpochs > 0)
  {
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
                         isBin=darch.isBin,
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
  
  darch <- getExecuteFunction(darch)(darch,dataSet@data)
  execOut <- getExecOutput(darch)
  
  if (any(dataSet@parameters$scaled) && !is.null(dataSet@parameters$yscale))
  {
    execOutScaled <- execOut * dataSet@parameters$yscale$"scaled:scale"
                      + dataSet@parameters$yscale$"scaled:center"
  }
  else
  {
    execOutScaled <- execOut
  }
  
  return(switch(type, raw = execOutScaled, bin = (execOut>.5)*1,
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
    cat(pasteArg("rbm.learnRateWeights", getLearnRateWeights(rbm)))
    cat(pasteArg("rbm.learnRateBiasVisible", getLearnRateBiasVisible(rbm)))
    cat(pasteArg("rbm.learnRateBiasHidden", getLearnRateBiasHidden(rbm)))
    cat(pasteArg("rbm.weightCost", getWeightCost(rbm)))
    cat(pasteArg("rbm.initialMomentum", getInitialMomentum(rbm)))
    cat(pasteArg("rbm.finalMomentum", getFinalMomentum(rbm)))
    cat(pasteArg("rbm.momentumSwitch", getMomentumSwitch(rbm)))
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
  cat(pasteArg("darch.momentumSwitch", getMomentumSwitch(darch)))
  cat(pasteArg("darch.learnRateWeights", getLearnRateWeights(darch)))
  cat(pasteArg("darch.learnRateBiases", getLearnRateBiases(darch)))
  cat(pasteArg("darch.dropoutInput", getDropoutInputLayer(darch)))
  cat(pasteArg("darch.dropoutHidden", getDropoutHiddenLayers(darch)))
  cat(pasteArg("darch.layerFunction.maxout.poolSize",
    getOption("darch.unitFunction.maxout.poolSize", NULL)))
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