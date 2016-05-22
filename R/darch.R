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

# TODO remove @importFrom Rcpp when no longer needed

#' Fit a deep neural network
#'
#' Fit a deep neural network with optional pre-training and one of various
#' fine-tuning algorithms.
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
#' @import methods stats ggplot2 caret
#' @importFrom Rcpp sourceCpp
#' @useDynLib darch
#'
#' @author Martin Drees \email{mdrees@@stud.fh-dortmund.de} and contributors.
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
#' @name darch
#' @export
darch <- function(x, ...)
{
  UseMethod("darch")
}

#' @param x Input data matrix or \code{\link{data.frame}}
#'   (\code{darch.default}) or \code{\link{formula}} (\code{darch.formula}) or
#'   \code{\linkS4class{DataSet}} (\code{darch.DataSet}).
#' @param y Target data matrix or \code{\link{data.frame}}, if \code{x} is an
#'   input data matrix or \code{\link{data.frame}}.
#' @param layers Vector containing one integer for the number of neurons of
#'   each layer. Defaults to c(\code{a}, 10, \code{b}), where \code{a} is the
#'   number of columns in the training data and \code{b} the number of columns
#'   in the targets. If this has length 1, it is used as the number of neurons
#'   in the hidden layer, not as the number of layers!
#' @param ... Additional parameters.
#' @param xValid Validation input data matrix or \code{\link{data.frame}}.
#' @param yValid Validation target data matrix or \code{\link{data.frame}}, if
#'   \code{x} is a data matrix or \code{\link{data.frame}}.
#' @param preProc.factorToNumeric Whether all factors should be converted to
#'   numeric.
#' @param preProc.factorToNumeric.targets Whether all factors should be
#'   converted to numeric in the target data.
#' @param preProc.orderedToFactor.targets Whether ordered factors in the target
#'   data should be converted to unordered factors. \strong{Note:} Ordered
#'   factors are converted to numeric by \code{\link[caret]{dummyVars}} and no
#'   longer usable for classification tasks.
#' @param preProc.fullRank Whether to use full rank encoding. See
#'   \link[caret]{preProcess} for details.
#' @param preProc.fullRank.targets Whether to use full rank encoding for target
#' data. See \link[caret]{preProcess} for details.
#' @param preProc.params List of parameters to pass to the
#'   \code{\link[caret]{preProcess}} function for the input data or
#'   \code{FALSE} to disable input data pre-processing.
#' @param preProc.targets Whether target data is to be centered and
#'   scaled. Unlike \code{preProc.params}, this is just a logical
#'   turning pre-processing for target data on or off, since this
#'   pre-processing has to be reverted when predicting new data. Most useful
#'   for regression tasks. \strong{Note:} This will skew the raw network error.
#' @param normalizeWeights Logical indicating whether to normalize weights (L2
#'   norm = \code{normalizeWeightsBound}).
#' @param normalizeWeightsBound Upper bound on the L2 norm of incoming weight
#'   vectors. Used only if \code{normalizeWeights} is \code{TRUE}.
#' @param shuffleTrainData Logical indicating whether to shuffle training data
#'   before each epoch.
#' @param generateWeightsFunction Weight generation function or vector of layer
#'   generation functions of length \code{number of layers} - 1. Possible
#'   weight generation functions include \code{\link{generateWeightsUniform}}
#'   (default), \code{\link{generateWeightsNormal}},
#'   \code{\link{generateWeightsGlorotNormal}},
#'   \code{\link{generateWeightsGlorotUniform}},
#'   \code{\link{generateWeightsHeNormal}}, and
#'   \code{\link{generateWeightsHeUniform}}.
#' @param rbm.batchSize Pre-training batch size.
#' @param rbm.lastLayer \code{Numeric} indicating at which layer to stop the
#'   pre-training. Possible values include \code{0}, meaning that all layers
#'   are trained; positive integers, meaning to stop training after the RBM
#'   where \code{rbm.lastLayer} forms the visible layer; negative integers,
#'   meaning to stop the training at \code{rbm.lastLayer} RBMs from the top
#'   RBM.
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
#' @param rbm.unitFunction Unit function during pre-training. Possible
#'   functions include \code{\link{sigmoidUnitRbm}} (default),
#'   \code{\link{tanhUnitRbm}}, and \code{\link{linearUnitRbm}}.
#' @param rbm.updateFunction Update function during pre-training. Currently,
#'   \code{darch} only provides \code{\link{rbmUpdate}}.
#' @param rbm.errorFunction Error function during pre-training. This is only
#'   used to estimate the RBM error and does not affect the training itself.
#'   Possible error functions include \code{\link{mseError}} and
#'   \code{\link{rmseError}}.
#' @param rbm.numCD Number of full steps for which contrastive divergence is
#'   performed. Increasing this will slow training down considerably.
#' @param rbm.consecutive Logical indicating whether to train the RBMs one at
#'   a time for \code{rbm.numEpochs} epochs (\code{TRUE}, default) or
#'   alternatingly training each RBM for one epoch at a time (\code{FALSE}).
#' @param rbm.numEpochs Number of pre-training epochs. \strong{Note:} When
#'   passing a value other than \code{0} here and also passing an existing
#'   \code{\linkS4class{DArch}} instance via the \code{darch} parameter, the
#'   weights of the network will be completely reset!
#'   Pre-training is essentially a
#'   form of advanced weight initialization and it makes no sense to perform
#'   pre-training on a previously trained network.
#' @param rbm.allData Logical indicating whether to use training and validation
#'   data for pre-training. \strong{Note:} This also applies when using
#'   bootstrapping.
#' @param darch Existing \code{\linkS4class{DArch}} instance for which training
#'   is to be resumed. \strong{Note:} When enabling pre-training, previous
#'   training results we be lost, see explanation for parameter
#'   \code{rbm.numEpochs}.
#' @param darch.batchSize Batch size, i.e. the number of training samples that
#'   are presented to the network before weight updates are performed, for
#'   fine-tuning.
#' @param bootstrap Logical indicating whether to use bootstrapping to
#'   create a training and validation data set from the given training data.
#' @param bootstrap.unique Logical indicating whether to take only unique
#'   samples for the training (\code{TRUE}, default) or take all drawn
#'   samples (\code{FALSE}), which will results in a bigger training set with
#'   duplicates. \strong{Note:} This is ignored if \code{bootstrap.num} is
#'   greater than 0.
#' @param bootstrap.num If this is greater than 0, bootstrapping will draw this
#'   number of training samples without replacement.
#' @param darch.fineTuneFunction Fine-tuning function. Possible values include
#'   \code{\link{backpropagation}} (default), \code{\link{rpropagation}},
#'   \code{\link{minimizeClassifier}} and \code{\link{minimizeAutoencoder}}
#'   (unsupervised).
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
#' @param darch.nesterovMomentum Whether to use
#'   \href{https://cs231n.github.io/neural-networks-3/#sgd}{Nesterov Accelerated Momentum}.
#'   (NAG) for gradient descent based fine-tuning algorithms.
#' @param darch.errorFunction Error function during fine-tuning. Possible error
#'   functions include \code{\link{mseError}},\code{\link{rmseError}}, and
#'   \code{\link{crossEntropyError}}.
#' @param darch.dropout Dropout rates. If this is a vector it will be treated
#'   as the dropout rates for each individual layer. If one element is missing,
#'   the input dropout will be set to 0. When enabling
#'   \code{darch.dropout.dropConnect}, this vector needs an additional element
#'   (one element per weight matrix between two layers as opposed to one
#'   element per layer excluding the last layer).
#' @param darch.dropout.dropConnect Whether to use DropConnect instead of
#'   dropout for the hidden layers. Will use \code{darch.dropout} as the
#'   dropout rates.
#' @param darch.dropout.momentMatching How many iterations to perform during
#'   moment matching for dropout inference, 0 to disable moment matching.
#' @param darch.dropout.oneMaskPerEpoch Whether to generate a new mask for each
#'   batch (\code{FALSE}, default) or for each epoch (\code{TRUE}).
#' @param darch.dither Whether to apply
#'   \href{http://arxiv.org/abs/1508.04826}{dither} to numeric columns in the
#'   training input data.
#' @param darch.weightDecay Weight decay factor, defaults to \code{0}. All
#'   weights will be multiplied by (1 - \code{darch.weightDecay}) prior to each
#'   weight update.
#' @param darch.unitFunction Layer function or vector of layer functions of
#'   length \code{number of layers} - 1. Note that the first entry signifies
#'   the layer function between layers 1 and 2, i.e. the output of layer 2.
#'   Layer 1 does not have a layer function, since the input values are used
#'   directly. Possible unit functions include \code{\link{linearUnit}},
#'   \code{\link{sigmoidUnit}}, \code{\link{tanhUnit}},
#'   \code{\link{rectifiedLinearUnit}}, \code{\link{softplusUnit}},
#'   \code{\link{softmaxUnit}}, and \code{\link{maxoutUnit}}.
#' @param darch.weightUpdateFunction Weight update function or vector of weight
#'   update functions, very similar to \code{darch.unitFunction}. Possible
#'   weight update functions include \code{\link{weightDecayWeightUpdate}} and
#'   \code{\link{maxoutWeightUpdate}} Note that \code{\link{maxoutWeightUpdate}}
#'   must be used on the layer \strong{after} the maxout activation function!
#' @param darch.maxout.poolSize Pool size for maxout units, when
#'   using the maxout acitvation function. See \code{\link{maxoutUnit}}.
#' @param darch.maxout.unitFunction Inner unit function used by maxout. See
#'   \code{darch.unitFunction} for possible unit functions.
#' @param darch.isClass Whether output should be treated as class labels
#'   during fine-tuning and classification rates should be printed.
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
#' @param retainData Logical indicating whether to store the training
#'   data in the \code{\linkS4class{DArch}} instance after training or when
#'   saving it to disk.
#' @param darch.returnBestModel Logical indicating whether to return the best
#'   model at the end of training, instead of the last.
#' @param darch.returnBestModel.validationErrorFactor When evaluating models
#'   with validation data, how high should the validation error be valued,
#'   compared to the training error? This is a value between 0 and 1. By
#'   default, this value is \code{1 - exp(-1)}. The training error factor
#'   and the validation error factor will always add to 1, so if you pass 1
#'   here, the training error will be ignored, and if you pass 0 here, the
#'   validation error will be ignored.
#' @param darch.trainLayers Either TRUE to train all layers or a mask containing
#'   TRUE for all layers which should be trained and FALSE for all layers that
#'   should not be trained (no entry for the input layer).
#' @param darch.elu.alpha Alpha parameter for the exponential linear unit
#'   function. See \code{\link{exponentialLinearUnit}}.
#' @inheritParams backpropagation
#' @inheritParams rpropagation
#' @inheritParams minimizeClassifier
#' @inheritParams generateWeightsNormal
#' @inheritParams generateWeightsUniform
#' @param autosave Logical indicating whether to activate automatically saving
#'   the \code{\linkS4class{DArch}} instance to a file during fine-tuning.
#' @param autosave.dir Directory for the autosave files, the file names will be
#'   e.g. autosave_010.net for the DArch instance after 10 epochs
#' @param autosave.epochs After how many epochs should auto-saving happen, by
#'   default after every 5% of overall progress. If this number is smaller than
#'   1, the network will only be saved once when thee fine-tuning is done.
#' @param autosave.trim Whether to trim the network before saving it. This will
#'   remove the dataset and the layer weights, resulting in a network that is
#'   no longer usable for predictions or training. Useful when only statistics
#'   and settings need to be stored.
#' @param dataSet \code{\linkS4class{DataSet}} instance, passed from
#'   darch.DataSet(), may be specified manually.
#' @param dataSetValid \code{\linkS4class{DataSet}} instance containing
#'   validation data.
#' @param gputools Logical indicating whether to use gputools for matrix
#'   multiplication, if available.
#' @param gputools.deviceId Integer specifying the device to use for GPU
#'   matrix multiplication. See \code{\link[gputools]{chooseGpu}}.
#' @param paramsList List of parameters, can include and does overwrite
#'   specified parameters listed above. Primary for convenience or for use in
#'   scripts.
#' @param logLevel \code{\link{futile.logger}} log level. Uses the currently
#'   set log level by default, which is \code{futile.logger::flog.info} if it
#'   was not changed. Other available levels include, from least to most
#'   verbose: \code{FATAL}, \code{ERROR}, \code{WARN}, \code{DEBUG}, and
#'   \code{TRACE}.
#' @param seed Allows the specification of a seed which will be set via
#'   \code{\link{set.seed}}. Used in the context of \code{\link{darchBench}}.
#' @return Fitted \code{\linkS4class{DArch}} instance
#' @family darch interface functions
#' @rdname darch
#' @export
darch.default <- function(
  x,
  y,
  layers = 10,
  ...,
  autosave = F,
  autosave.epochs = round(darch.numEpochs / 20),
  autosave.dir = "./darch.autosave",
  autosave.trim = F,
  bp.learnRate = 1,
  bp.learnRateScale = 1,
  bootstrap = F,
  bootstrap.unique = T,
  bootstrap.num = 0,
  cg.length = 2,
  cg.switchLayers = 1,
  darch = NULL,
  darch.batchSize = 1,
  darch.dither = F,
  darch.dropout = 0,
  darch.dropout.dropConnect = F,
  darch.dropout.momentMatching = 0,
  darch.dropout.oneMaskPerEpoch = F,
  darch.elu.alpha = 1,
  darch.errorFunction = if (darch.isClass) crossEntropyError else mseError,
  darch.finalMomentum = .9,
  darch.fineTuneFunction = backpropagation,
  darch.initialMomentum = .5,
  darch.isClass = T,
  darch.maxout.poolSize = 2,
  darch.maxout.unitFunction = linearUnit,
  darch.momentumRampLength = 1,
  darch.nesterovMomentum = T,
  darch.numEpochs = 100,
  darch.returnBestModel = T,
  darch.returnBestModel.validationErrorFactor = 1 - exp(-1),
  darch.stopClassErr = -Inf,
  darch.stopErr = -Inf,
  darch.stopValidClassErr = -Inf,
  darch.stopValidErr = -Inf,
  darch.trainLayers = T,
  darch.unitFunction = sigmoidUnit,
  darch.weightDecay = 0,
  darch.weightUpdateFunction = weightDecayWeightUpdate,
  dataSet = NULL,
  dataSetValid = NULL,
  generateWeightsFunction = generateWeightsGlorotUniform,
  gputools = F,
  gputools.deviceId = 0,
  logLevel = NULL,
  normalizeWeights = F,
  normalizeWeightsBound = 15,
  paramsList = list(),
  preProc.factorToNumeric = F,
  preProc.factorToNumeric.targets = F,
  preProc.fullRank = T,
  preProc.fullRank.targets = F,
  preProc.orderedToFactor.targets = T,
  preProc.params = F,
  preProc.targets = F,
  rbm.allData = F,
  rbm.batchSize = 1,
  rbm.consecutive = T,
  rbm.errorFunction = mseError,
  rbm.finalMomentum = .9,
  rbm.initialMomentum = .5,
  rbm.lastLayer = 0,
  rbm.learnRate = 1,
  rbm.learnRateScale = 1,
  rbm.momentumRampLength = 1,
  rbm.numCD = 1,
  rbm.numEpochs = 0,
  rbm.unitFunction = sigmoidUnitRbm,
  rbm.updateFunction = rbmUpdate,
  rbm.weightDecay = 2e-4,
  retainData = F,
  rprop.decFact = .5,
  rprop.incFact = 1.2,
  rprop.initDelta = 1/80,
  rprop.maxDelta = 50,
  rprop.method = "iRprop+",
  rprop.minDelta = 1e-6,
  seed = NULL,
  shuffleTrainData = T,
  weights.max = .1,
  weights.mean = 0,
  weights.min = -.1,
  weights.sd = .01,
  xValid = NULL,
  yValid = NULL)
{
  oldLogLevel <- futile.logger::flog.threshold()
  on.exit(futile.logger::flog.threshold(oldLogLevel))
  setLogLevel(logLevel)
  
  if (!is.null(seed))
  {
    futile.logger::flog.info("Setting seed to %s", seed)
    set.seed(seed)
  }

  additionalParameters <- list(...)

  additionalParameters <- processAdditionalParams(additionalParameters)

  params <- mergeParams(additionalParameters, paramsList, mget(ls()),
    blacklist = c("x", "y", "xValid", "yValid", "dataSet", "dataSetValid",
    "dataValid", "darch"))

  if (missing(y))
  {
    y <- NULL
  }

  # Create data set if none was provided
  if (is.null(dataSet))
  {
    # TODO solve cleaner (DataSet must be missing for this to work)
    if (is.null(darch))
    {
      dataSet <- createDataSet(data = x, targets = y,
        preProc.params = preProc.params, ...)
    }
    else
    {
      dataSet <- createDataSet(data = x, targets = y,
        dataSet = darch@dataSet,
        preProc.params = preProc.params, ...)
    }

    if (!is.null(xValid))
    {
      dataSetValid <- createDataSet(data = xValid, targets = yValid,
        previous.dataSet = dataSet,
        preProc.params = preProc.params, ...)
    }
  }

  params <- processParams(params)

  if (params[[".bootstrap"]])
  {
    if (is.null(dataSetValid))
    {
      bootstrap <- bootstrapDataSet(dataSet, params[[".bootstrap.unique"]],
        params[[".bootstrap.num"]])
      dataSet <- bootstrap[[1]]
      dataSetValid <- bootstrap[[2]]
      rm(bootstrap)
    }
    else
    {
      futile.logger::flog.warn(
        "Since validation data were provided, bootstrapping will be disabled.")
      params[[".bootstrap"]] <- F
      params[["bootstrap"]] <- F
    }
  }


  if (is.null(darch))
  {
    futile.logger::flog.info("Creating and configuring new DArch instance")

    darch <- newDArch(params)
  }
  else
  {
    futile.logger::flog.info("Configuring existing DArch instance")
    params[["darch.epochsTrained"]] <- darch@epochs
    darch@parameters <- params
    darch <- configureDArch(darch)
  }

  darch@dataSet <- postProcessDataSet()

  futile.logger::flog.info(paste("DArch instance ready for training, here is",
    "a summary of its configuration:"))

  print(darch)

  if (rbm.numEpochs > 0 && darch@epochs == 0)
  {
    darch <- preTrainDArch(darch, dataSet, dataSetValid = dataSetValid,
      numEpochs = params[["rbm.numEpochs"]], numCD = params[["rbm.numCD"]],
      lastLayer = params[["rbm.lastLayer"]],
      isClass = params[["darch.isClass"]],
      consecutive = params[["rbm.consecutive"]], ...)
  }
  else if (rbm.numEpochs > 0 && darch@epochs != 0)
  {
    futile.logger::flog.warn(paste("Skipping pre-training on trained DArch",
      "instance, please create a new instance to enable pre-training."))
  }

  if (darch.numEpochs > 0)
  {
    darch <- fineTuneDArch(darch, dataSet, dataSetValid = dataSetValid,
      numEpochs = params[[".darch.numEpochs"]],
      isClass = params[[".darch.isClass"]],
      stopErr = params[[".darch.stopErr"]],
      stopClassErr = params[[".darch.stopClassErr"]],
      stopValidErr = params[[".darch.stopValidErr"]],
      stopValidClassErr = params[[".darch.stopValidClassErr"]], ...)
  }

  # Restore old log level
  # TODO what if the training crashed?
  futile.logger::flog.threshold(darch@parameters[[".oldLogLevel"]])

  darch
}

#' @param data \code{\link{data.frame}} containing the dataset, if \code{x} is
#'   a \code{\link{formula}}.
#' @rdname darch
#' @export
darch.formula <- function(x, data, layers, ..., xValid=NULL,
                          dataSet = NULL, dataSetValid = NULL, logLevel = NULL, paramsList = list(),
                          darch = NULL)
{
  oldLogLevel <- futile.logger::flog.threshold()
  on.exit(futile.logger::flog.threshold(oldLogLevel))
  setLogLevel(logLevel)
  
  if (is.null(dataSet))
  {
    # Use existing DataSet if available
    # TODO check if formula fit?
    if (!is.null(darch))
    {
      dataSet <- createDataSet(data = data,
                               targets = NULL, dataSet = darch@dataSet)
    }
    else
    {
      dataSet <- createDataSet(data = data, formula = x, ...)
    }
  }
  
  if (is.null(dataSetValid) && !is.null(xValid))
  {
    dataSetValid <- createDataSet(xValid, T, previous.dataSet = dataSet,
                                  ...)
  }
  
  darch <- darch(dataSet, dataSetValid = dataSetValid, ...,
                 layers = if (missing(layers)) 10 else layers, paramsList = paramsList,
                 darch = darch)
  
  darch
}

#' @rdname darch
#' @export
darch.DataSet <- function(x, ...)
{
  darch.default(x = NULL, y = NULL, ..., dataSet = x)
}