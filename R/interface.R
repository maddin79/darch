# Copyright (C) 2013-2016 Martin Drees
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

# TODO remove @importFrom Rcpp when no longer needed

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
#' @import methods stats
#' @importFrom Rcpp sourceCpp
#' @useDynLib darch
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
#' @example examples/example.xor.R
#' @example examples/example.xor_nominal.R
#' @example examples/example.iris.R
#' @example examples/example.mnist.R
#' @example examples/examples.R
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
darch.formula <- function(x, data, layers, ..., dataValid=NULL, logLevel = NULL,
                          paramsList = list())
{
  paramsList$.oldLogLevel <- futile.logger::flog.threshold()
  setLogLevel(logLevel)
  
  # TODO would prefer requireNamespace here, but caret registers its functions
  # globally without namespace, will result in errors
  if (!suppressMessages(require("caret", quietly = T)))
  {
    stop(
      "Formula interface only supported with \"caret\" package installed.")
  }
  
  dataSet <- createDataSet(data = data, formula = x, ...)
  dataSetValid <- NULL
  
  if (!is.null(dataValid))
  {
    dataSetValid <- createDataSet(dataValid, T, previous.dataSet = dataSet, ...)
  }
  
  darch <- darch(dataSet, dataSetValid=dataSetValid, ...,
    layers = if (missing(layers)) 10 else layers, paramsList = paramsList)
  
  darch
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
#'   targets. If this has length 1, it is used as the number of neurons in the
#'   hidden layer, not as the number of layers!
#' @param ... additional parameters
#' @param xValid Validation input data.
#' @param yValid Validation target data.
#' @param caret.preProcessParams List of parameters to pass to the
#'   \code{\link{preProcess}} function for the input data or false to disable
#'   input data pre-processing.
#' @param normalizeWeights Logical indicating whether to normalize weights (L2
#'   norm = 1).
#' @param normalizeWeightsBound Upper bound on the L2 norm of incoming weight
#'   vectors. Used only if \code{normalizeWeights} is \code{TRUE}.
#' @param shuffleTrainData Logical indicating whether to shuffle training data
#'   before each epoch.
#' @param generateWeightsFunction Function to generate the initial weights of
#'   the DBN.
#' @param rbm.batchSize Pre-training batch size.
#' @param rbm.lastLayer \code{Numeric} indicating at which layer to stop the
#'   pre-training. Possible values include \code{0}, meaning that all layers
#'   are trained; positive integers, meaning to stop training after the RBM
#'   where \code{rbm.lastLayer} forms the visible layer; negative integers,
#'   meaning to stop the training at \code{rbm.lastLayer} RBMs from the top RBM.
#' @param rbm.learnRate Learning rate during pre-training.
#' @param rbm.learnRateScale The learn rates will be multiplied with this
#'   value after each epoch.
#' @param rbm.weightDecay Pre-training weight decay. Weights will be multiplied
#'   by (1 - \code{rbm.weightDecay}) prior to each weight update.
#' @param rbm.initialMomentum Initial momentum during pre-training.
#' @param rbm.finalMomentum Final momentum during pre-training.
#' @param rbm.momentumRampLength After how many epochs, relative to
#'   \code{rbm.numEpochs}, should the momentum reach \code{rbm.finalMomentum}?
#'   A value of 1 indicates that the \code{rbm.finalMomentum} should be reached
#'   in the final epoch, a value of 0.5 indicates that \code{rbm.finalMomentum}
#'   should be reached after half of the training is complete.
#' @param rbm.unitFunction Unit function during pre-training.
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
#' @param darch.fineTuneFunction Fine-tuning function.
#' @param darch.initialMomentum Initial momentum during fine-tuning.
#' @param darch.finalMomentum Final momentum during fine-tuning.
#' @param darch.momentumRampLength After how many epochs, relative to
#'   the \strong{overall} number of epochs trained, should the momentum reach
#'   \code{darch.finalMomentum}?
#'   A value of 1 indicates that the \code{darch.finalMomentum} should be 
#'   reached in the final epoch, a value of 0.5 indicates that
#'   \code{darch.finalMomentum} should be reached after half of the training is
#'   complete. Note that this will lead to bumps in the momentum ramp if
#'   training is resumed with the same parameters for
#'   \code{darch.initialMomentum} and \code{darch.finalMomentum}. Set
#'   \code{darch.momentumRampLength} to 0 to avoid this problem when resuming
#'   training.
#' @param darch.nesterovMomentum Whether to use Nesterov Accelerated Momentum
#'   (NAG) for gradient descent based fine-tuning algorithms.
#' @param darch.learnRate Learning rate during fine-tuning.
#' @param darch.learnRateScale The learning rates are multiplied by this value
#'   after each epoch.
#' @param darch.errorFunction Error function during fine-tuning.
#' @param darch.dropout Dropout rates. If this is a vector it will be treated as
#'   the dropout rates for each individual layer. If one element is missing, the
#'   input dropout will be set to 0. When enabling
#'   \code{darch.dropout.dropConnect}, this vector needs an additional element
#'   (one element per weight matrix between two layers as opposed to one
#'   element per layer excluding the last layer).
#' @param darch.dropout.dropConnect Whether to use DropConnect instead of
#'   dropout for the hidden layers. Will use \code{darch.dropout} as the
#'   DropConnect rates.
#' @param darch.dropout.momentumMatching How many iterations to perform during
#'   moment matching for dropout inference, 0 to disable moment matching.
#' @param darch.dropout.oneMaskPerEpoch Whether to generate a new mask for each
#'   batch (\code{FALSE}, default) or for each epoch (\code{TRUE}).
#' @param darch.unitFunction Layer function or vector of layer functions of
#'   length \code{number of layers} - 1. Note that the first entry signifies the
#'   layer function between layers 1 and 2, i.e. the output of layer 2. Layer 1
#'   does not have a layer function, since the input values are used directly.
#' @param darch.weightUpdateFunction Weight update function or vector of weight
#'   update functions, very similar to \code{darch.unitFunction}.
#' @param darch.unitFunction.maxout.poolSize Pool size for maxout units, when
#'   using the maxout acitvation function. See \code{\link{maxoutUnit}}.
#' @param darch.isClass Whether output should be treated as class labels
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
#' @param rbm.consecutive Logical indicating whether to train the RBMs one at
#'   a time for \code{rbm.numEpochs} epochs (\code{TRUE}, default) or
#'   alternatingly training each RBM for one epoch at a time (\code{FALSE}).
#' @param darch.retainData Logical indicating whether to store the training
#'   data in the \code{\linkS4class{DArch}} instance after training.
#' @param darch.returnBestModel Logical indicating whether to return the best
#'   model at the end of training, instead of the last.
#' @param autosave Logical indicating whether to activate automatically saving
#'   the \code{\linkS4class{DArch}} instance to a file during fine-tuning.
#' @param autosave.location Path and filename of the autosave file, the file
#'   type ".net" will be appended.
#' @param autosave.epochs After how many epochs should auto-saving happen, by
#'   default after every 5% of overall progress. If this number is smaller than
#'   1, the network will only be saved once when thee fine-tuning is done.
#' @param dataSet \code{\linkS4class{DataSet}} instance, passed from
#'   darch.DataSet(), may be specified manually.
#' @param dataSetValid \code{\linkS4class{DataSet}} instance containing
#'   validation data.
#' @param gputools Logical indicating whether to use gputools for matrix
#'   multiplication, if available.
#' @param gputools.deviceId Integer specifying the device to use for GPU
#'   matrix multiplication. See \code{\link{chooseGpu}}.
#' @param paramsList List of parameters, can include and does overwrite
#'   specified parameters listed above. Primary for convenience.
#' @param logLevel futile.lgoger log level. Uses the currently set log level by
#'   default, which is \code{futile.logger::flog.info} if it was not changed.
#'   Other available levels include, from least to most verbose,
#'   \code{FATAL}, \code{ERROR}, \code{WARN}, \code{DEBUG}, and \code{TRACE}.
#' @return Fitted \code{\linkS4class{DArch}} instance
#' @family darch interface functions
#' @export
darch.default <- function(
  x,
  y,
  layers = 10,
  ...,
  xValid = NULL,
  yValid = NULL,
  caret.preProcessParams = F,
  normalizeWeights = F,
  normalizeWeightsBound = 15,
  shuffleTrainData = T,
  generateWeightsFunction = generateWeightsGlorotUniform,
  # RBM configuration
  rbm.batchSize = 1,
  rbm.lastLayer = 0,
  rbm.learnRate = 1,
  rbm.learnRateScale = 1,
  rbm.weightDecay = .0002,
  rbm.initialMomentum = .5,
  rbm.finalMomentum = .9,
  rbm.momentumRampLength = 1,
  rbm.unitFunction = sigmoidUnitRbm,
  rbm.updateFunction = rbmUpdate,
  rbm.errorFunction = mseError,
  # higher values make everything much slower
  rbm.numCD = 1,
  rbm.numEpochs = 0,
  rbm.consecutive = T,
  
  # DArch constructor arguments.
  # existing DArch instance
  darch = NULL,
  darch.batchSize = 1,
  darch.bootstrap = F,
  # DArch configuration
  darch.fineTuneFunction = backpropagation,
  darch.initialMomentum = .5,
  darch.finalMomentum = .9,
  darch.momentumRampLength = 1,
  darch.nesterovMomentum = T,
  # higher for sigmoid activation
  darch.learnRate = 1,
  darch.learnRateScale = 1,
  darch.errorFunction = mseError,
  darch.dropout = 0,
  darch.dropout.dropConnect = F,
  darch.dropout.momentMatching = 0,
  darch.dropout.oneMaskPerEpoch = F,
  darch.dither = F,
  darch.weightDecay = 0,
  # layer configuration.
  # activation function
  # custom activation functions
  darch.unitFunction = sigmoidUnit,
  darch.unitFunction.maxout.poolSize = 2,
  darch.weightUpdateFunction = weightDecayWeightUpdate,
  # fine-tune configuration
  darch.isClass = T,
  darch.stopErr = -Inf,
  darch.stopClassErr = -Inf,
  darch.stopValidErr = -Inf,
  darch.stopValidClassErr = -Inf,
  darch.numEpochs = 100,
  darch.retainData = T,
  darch.returnBestModel = T,
  autosave = F,
  autosave.location = "./darch",
  autosave.epochs = round(darch.numEpochs / 20),
  dataSet = NULL,
  dataSetValid = NULL,
  gputools = T,
  gputools.deviceId = 0,
  paramsList = list(),
  # change to futile.logger::DEBUG if needed
  logLevel = NULL)
{
  # Don't set logging again if darch.formula has already set it
  # TODO solve cleaner
  if (is.null(paramsList$.oldLogLevel))
  {
    paramsList$.oldLogLevel <- futile.logger::flog.threshold()
    setLogLevel(logLevel)
  }
  
  params <- mergeParams(list(...), paramsList, mget(ls()),
    blacklist = c("x", "y", "xValid", "yValid", "dataSet", "dataSetValid",
      "darch"))
  
  if (is.null(params[["matMult"]]))
  {
    params[["matMult"]] <- `%*%`
  }
  
  params[["debug"]] <- (names(futile.logger::DEBUG) ==
    futile.logger::flog.threshold())
  
  if (gputools)
  {
    if ((length(find.package("gputools", quiet=T)) == 0))
    {
      futile.logger::flog.warn("gputools package not available.")
      futile.logger::flog.info("Using CPU matrix multiplication.")
      params[["gputools"]] <- F
    }
    else
    {
      params[["matMult"]] <- gputools::gpuMatMult
      # TODO handle invalid values and errors from chooseGpu()
      params[["gputools.deviceId"]] <- gputools::chooseGpu(gputools.deviceId)
      
      futile.logger::flog.info(paste("Using GPU matrix multiplication on",
        "device", params[["gputools.deviceId"]]))
    }
  }
  else
  {
    futile.logger::flog.info("Using CPU matrix multiplication.")
  }
  
  if (missing(y))
  {
    y <- NULL
    
    if (params[["darch.returnBestModel"]])
    {
      futile.logger::flog.warn(paste("No targets were provided, automatically",
        "changing darch.returnBestModel to FALSE"))
    }
    
    params[["darch.returnBestModel"]] <- F
  }
  
  # Create data set if none was provided
  if (is.null(dataSet))
  {
    dataSet <- createDataSet(data = x, targets = y, scale = scale,
      caret.preProcessParams = caret.preProcessParams, ...)
    
    if (!is.null(xValid))
    {
      dataSetValid <- createDataSet(data = xValid, targets = yValid,
        previous.dataSet = dataSet,
        caret.preProcessParams = caret.preProcessParams, ...)
    }
  }
  
  # TODO move into dataset validation?
  if (params[["darch.isClass"]] && is.null(dataSet@targets))
  {
    futile.logger::flog.warn(
      "No targets were provided, setting darch.isClass to FALSE")
    params[["darch.isClass"]] <- F
  }
  
  if (params[["darch.isClass"]] &&
        length(unique(dataSet@targets) > 2))
  {
    futile.logger::flog.warn(
      "darch.isClass was set to TRUE while numeric targets were provided")
  }
  
  params[["layers.original"]] <- layers
  
  # Allow deparsed vector to be passed
  # TODO document
  if (is.character(layers))
  {
    layers <- eval(parse(text = layers))
  }
  
  # Create default layers vector if scalar given
  if (length(layers) == 1)
  {
    futile.logger::flog.warn(paste("No vector given for \"layers\" parameter,",
      "constructing DBN with one hidden layer of %s neurons."), layers)
    layers = c(ncol(dataSet@data), layers, ncol(dataSet@targets))
  }
  
  numLayers = length(layers)
  
  # Adjust neurons in input layer
  if (layers[1] != ncol(dataSet@data))
  {
    futile.logger::flog.info(paste("Changing number of neurons in the input",
      "layer from %s to %s based on dataset."), layers[1], ncol(dataSet@data))
    layers[1] <- ncol(dataSet@data)
  }
  
  # Adjust neurons in output layer if classification
  if (params[["darch.isClass"]] && layers[numLayers] != ncol(dataSet@targets))
  {
    futile.logger::flog.info(paste("Changing number of neurons in the output",
      "layer from %s to %s based on dataset."), layers[numLayers],
      ncol(dataSet@targets))
    layers[numLayers] <- ncol(dataSet@targets)
  }
  
  # Print warning if using GPU matrix multiplication with small network
  if (params[["gputools"]])
  {
    matrixSizes <- vector(mode = "numeric", length = numLayers - 1)
    
    for (i in 1:(numLayers - 1))
    {
      matrixSizes[i] <-
        params[["darch.batchSize"]] * layers[i] + layers[i] * layers[i+1]
    }
    
    # TODO matrix multiplication for validation is not considered
    # TODO what can be considered small? for now, average of less than 100x100
    if (mean(matrixSizes) < 100*100*2)
    {
      futile.logger::flog.warn(paste("Due to small network and / or batch",
        "size, GPU matrix multiplication may be slower than CPU matrix",
        "multiplication."))
    }
  }
  
  if (is.null(darch))
  {
    futile.logger::flog.info("Creating new DArch instance")
    
    darch <- newDArch(
      layers=layers,
      # batch size for RBMs is set upon creation
      batchSize=params[["rbm.batchSize"]],
      genWeightFunc=params[[".generateWeightsFunction"]],
      logLevel=params[["logLevel"]],
      .params=params)
  }
  else
  {
    futile.logger::flog.info("Using existing DArch instance")
    darch@params <- params
  }
  
  futile.logger::flog.info("Configuring DArch instance")
  
  # Adjust RBM parameters
  rbmList <- darch@rbmList
  for(i in 1:length(rbmList))
  {
    rbmList[[i]]@initialLearnRate <- params[["rbm.learnRate"]]
    rbmList[[i]]@learnRate <- params[["rbm.learnRate"]]
    rbmList[[i]]@learnRateScale <- params[["rbm.learnRateScale"]]
    rbmList[[i]]@weightDecay <- params[["rbm.weightDecay"]]
    rbmList[[i]]@initialMomentum <- params[["rbm.initialMomentum"]]
    rbmList[[i]]@finalMomentum <- params[["rbm.finalMomentum"]]
    rbmList[[i]]@momentumRampLength <- params[["rbm.momentumRampLength"]]
    rbmList[[i]]@unitFunction <- params[[".rbm.unitFunction"]]
    rbmList[[i]]@updateFunction <- params[[".rbm.updateFunction"]]
    rbmList[[i]]@errorFunction <- params[[".rbm.errorFunction"]]
    rbmList[[i]]@normalizeWeights <- params[["normalizeWeights"]]
    rbmList[[i]]@normalizeWeightsBound <- params[["normalizeWeightsBound"]]
    rbmList[[i]]@epochsScheduled <- params[["rbm.numEpochs"]]
    rbmList[[i]] <- resetRBM(rbmList[[i]], darch=darch)
  }
  darch@rbmList <- rbmList
  
  # DArch configuration
  darch@fineTuneFunction <- params[[".darch.fineTuneFunction"]]
  darch@initialMomentum <- params[["darch.initialMomentum"]]
  darch@finalMomentum <- params[["darch.finalMomentum"]]
  darch@momentumRampLength <- params[["darch.momentumRampLength"]]
  darch@initialLearnRate <- params[["darch.learnRate"]]
  darch@learnRate <- params[["darch.learnRate"]]
  darch@learnRateScale <- params[["darch.learnRateScale"]]
  darch@errorFunction <- params[[".darch.errorFunction"]]
  darch@dropout <- params[["darch.dropout"]]
  darch@dropoutOneMaskPerEpoch <- params[["darch.dropout.oneMaskPerEpoch"]]
  darch@dropConnect <- params[["darch.dropout.dropConnect"]]
  darch@dither <- params[["darch.dither"]]
  darch@normalizeWeights <- params[["normalizeWeights"]]
  darch@normalizeWeightsBound <- params[["normalizeWeightsBound"]]
  
  unitFunctions <- (if (length(params[[".darch.unitFunction"]]) == 1)
    replicate(numLayers - 1, params[[".darch.unitFunction"]]) else
    params[[".darch.unitFunction"]])
  weightUpdateFunctions <-
    (if (length(params[[".darch.weightUpdateFunction"]]) == 1)
    replicate(numLayers - 1, params[[".darch.weightUpdateFunction"]]) else
      params[[".darch.weightUpdateFunction"]])
  
  # Check vector lengths
  if (any(c(length(unitFunctions), length(weightUpdateFunctions))
    != (numLayers - 1)))
  {
    futile.logger::flog.error(
      "Invalid number of unit or weight update functions")
    stop("Invalid configuration")
  }
  
  unitFunctionsNames <- vector(mode="character", length=numLayers - 1)
  weightUpdateFunctionsNames <- vector(mode="character", length=numLayers - 1)
  # per-layer configuration
  for (i in 1:(numLayers-1))
  {
    # Layer functions
    darch@layers[[i]][["unitFunction"]] <- unitFunctions[[i]]
    
    # Weight update functions
    darch@layers[[i]][["weightUpdateFunction"]] <- weightUpdateFunctions[[i]]
  }
  
  futile.logger::flog.info(paste("DArch instance ready for training, here is",
    "a summary of its configuration:"))
  
  print(darch)
  
  if (rbm.numEpochs > 0)
  {
    darch <- preTrainDArch(darch, dataSet, dataSetValid = dataSetValid,
      numEpochs = params[["rbm.numEpochs"]], numCD = params[["rbm.numCD"]],
      lastLayer = params[["rbm.lastLayer"]],
      isClass = params[["darch.isClass"]],
      consecutive = params[["rbm.consecutive"]], ...)
  }
  
  if (darch.numEpochs > 0)
  {
    darch@batchSize <- params[["darch.batchSize"]]
    darch <- fineTuneDArch(darch, dataSet, dataSetValid=dataSetValid,
      numEpochs=params[["darch.numEpochs"]],
      bootstrap=params[["darch.bootstrap"]], isClass=params[["darch.isClass"]],
      stopErr=params[["darch.stopErr"]],
      stopClassErr=params[["darch.stopClassErr"]],
      stopValidErr=params[["darch.stopValidErr"]],
      stopValidClassErr=params[["darch.stopValidClassErr"]], ...)
  }
  
  if (!darch.retainData)
  {
    darch@dataSet@data <- darch@dataSet@data[1,, drop = F]
    darch@dataSet@targets <- darch@dataSet@targets[1,, drop = F]
  }
  
  # Restore old log level
  # TODO what if the training crashed?
  futile.logger::flog.threshold(darch@params[[".oldLogLevel"]])
  
  darch
}

# TODO name, documentation

#' Benchmarking wrapper for \code{darch}
#' 
#' Simple benchmarking function which wraps around the \code{\link{darch}}
#' function for users who can't or don't want to use the caret package for
#' benchmarking. This function requires the foreach package to work, and will
#' perform parallel benchmarks if an appropriate backend was registered
#' beforehand.
#' 
#' @param bench.times How many benchmark runs to perform
#' @param bench.save Whether to save benchmarking results to a directory
#' @param bench.path Path (relative or absolute) including directory where
#'   benchmark results are saved if \code{bench.save} is true
#' @param bench.continue Whether the benchmark is to be continued from an
#'   earlier run (will look for results of an earlier run in the specified
#'   directory)
#' @param bench.delete Whether to delete the contents of the given directory if
#'   \code{bench.continue} is \code{FALSE}. Caution: This will attempt to delete
#'   ALL files in the given directory, use at your own risk!
#' @param bench.plots Whether to create plots for error rates and run times,
#'   will be ignored if \code{bench.save} is \code{FALSE}
#' @param output.capture Whether to capture R output in \code{.Rout} files in
#'   the given directory. This is the only way of gaining access to the R
#'   output since the foreach loop will not print anything to the console. Will
#'   be ignored if \code{bench.save} is \code{FALSE}
#' @param plot.classificationErrorRange Allows specification of the error range
#'   for the classification error to make the plot more meaningful. A value of
#'   \code{0.5}, for example, would limit the values on the y-axis to 50% of the
#'   complete error range.
#' @export
darchBench <- function(...,
  bench.times = 1,
  bench.save = F,
  bench.path = "./darch.benchmark",
  bench.continue = T,
  bench.delete = F,
  #bench.registerParallelBackend = T, TODO
  bench.plots = bench.save,
  output.capture = bench.save,
  plot.classificationErrorRange = 1.
  )
{
  # TODO deal with the log level
  indexStart <- prepareBenchmarkDirectory(bench.path, bench.save,
                                          bench.continue, bench.delete)
  
  darchList <- performBenchmark(bench.path, bench.times, indexStart,
    bench.save = bench.save, output.capture = output.capture, ...)
  
  if (bench.continue)
  {
    darchList <- c(loadAllDArch(bench.path), darchList)
  }
  
  stats <- aggregateStatistics(darchList)
  
  if (bench.save)
  {
    writeStatistics(bench.path, stats)
    
    if (bench.plots) createAllPlots(bench.path, stats$mean,
      plot.classificationErrorRange = plot.classificationErrorRange, ...)
  }
  
  c(darchList, list("stats" = stats))
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
#' @param outputLayer Layer number (if \code{> 0}) or offset (if \code{<= 0})
#'   relative to the last layer. The output of the given layer is returned.
#' @param type Output type, one of: \code{raw}, \code{bin}, \code{class}, or
#'   \code{character}. \code{raw} returns the layer output, \code{bin} returns
#'   \code{1} for every layer output \code{>0.5}, \code{0} otherwise, and
#'   \code{class} returns \code{1} for the output unit with the highest
#'   activation, otherwise \code{0}. Additionally, when using \code{class},
#'   class labels are returned when available. \code{character} is the same as
#'   \code{class}, except using character vectors instead of factors.
#' @return Vector or matrix of networks outputs, output type depending on the 
#'   \code{type} parameter
#' @export
#' @aliases predict.darch
#' @family darch interface functions
predict.DArch <- function (object, ..., newdata = NULL, type = "raw",
                           outputLayer = 0)
{
  oldLogLevel <- futile.logger::flog.threshold()
  setLogLevel(futile.logger::WARN)
  
  darch <- object
  
  if (is.null(newdata))
  {
    dataSet <- darch@dataSet
  }
  else
  {
    dataSet <- createDataSet(data = newdata,
      targets = if (is.null(darch@dataSet@formula)) NULL else F,
      dataSet = darch@dataSet)
  }
  
  execOut <- darch@executeFunction(darch, dataSet@data, outputLayer)[,, drop=T]
  
  ret <- switch(type, raw = execOut, bin = (execOut > .5)*1,
    class=,
    character =
    {
      if (outputLayer != 0 && outputLayer != length(darch@layers))
      {
        stop(paste("Only \"raw\" or \"bin\" output types supported when not",
          "using last layer as output layer."))
      }
      
      if (is.null(dataSet@parameters$preProcess) ||
        is.null(dataSet@parameters$dummyVarsTargets$lvls))
      {
        if (!is.null(dim(execOut)))
        {
          ret <- diag(ncol(execOut))[max.col(execOut, ties.method="first"),]
        }
        else
        {
          ret <- (execOut > .5) * 1
        }
      }
      else
      {
        if (!is.null(dim(execOut)))
        {
          if (length(dataSet@parameters$dummyVarsTargets$vars) > 1)
          {
            stop(paste("Prediction using multiple output variables not yet",
                       "supported, use \"raw\" or \"bin\" output type."))
          }
          
          ret <- dataSet@parameters$dummyVarsTargets$lvls[[1]][max.col(execOut,
            ties.method="first")]
          
          # convert to factor
          if (type == "class")
          {
            ret <- factor(ret, levels=dataSet@parameters$dummyVarsTargets$lvls[[1]])
          }
        }
        else
        {
          ret <- dataSet@parameters$ylevels[1 + (execOut > .5)]
        }
        
        ret
      }
    }, stop(paste0("Invalid type argument \"", type, "\"")))
  
  # Reset log level
  futile.logger::flog.threshold(oldLogLevel)
  
  ret
}

#' Test classification network.
#' 
#' Forward-propagate given data through the deep neural network and return
#' classification accuracy using the given labels.
#' 
#' @param darch \code{\linkS4class{DArch}} instance.
#' @param data New data to use, \code{NULL} to use training data.
#' @param targets Labels for the \code{data}, \code{NULL} to use training
#'  labels (only possible when \code{data} is \code{NULL} as well).
#' @return Vector containing error function output and percentage of incorrect
#'  classifications.
#' @export
#' @family darch interface functions
testDarch <- function(darch, data=NULL, targets=T)
{
  if (is.null(data))
  {
    dataSet <- darch@dataSet
  }
  else
  {
    if (is.null(darch@dataSet@formula) && targets == T)
    {
      futile.logger::flog.error(
        "No target data provided for classification test")
      stop()
    }
    
    dataSet <- createDataSet(data=data,
                             targets = targets,
                             dataSet = darch@dataSet)
  }
  
  e <- testDArch(darch, dataSet@data, dataSet@targets, "All Data",
                 getDarchParam("darch.isClass", T, darch))
  list("error" = e[1], "percentIncorrect" = e[2], "numIncorrect" = e[3])
}