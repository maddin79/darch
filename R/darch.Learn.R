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

#' Pre-trains a \code{\linkS4class{DArch}} network
#' 
#' This function pre-trains a \code{\linkS4class{DArch}} network with the 
#' contrastive divergence method
#' 
#' @details The function runs for every \code{\linkS4class{RBM}} in the
#'   attribute \code{rbmList} the training function \code{\link{trainRBM}}
#'   copies after the training the weights and biases into the corresponding
#'   layer of the \code{\linkS4class{DArch}} network.
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param dataSet \code{\linkS4class{DataSet}} to be used for training.
#' @param dataSetValid \code{\linkS4class{DataSet}} to be used for validation.
#' @param numEpochs The number of epochs
#' @param numCD The number of CD iterations
#' @param lastLayer Numeric indicating after which layer to stop training.
#' @param isClass Whether to test pre-trained networks against target data
#' @param consecutive Whether to train RBMs consecutively instead of each one
#'   epoch at a time.
#' @param ... Additional parameters for the function \code{\link{trainRBM}}
#' @return Trained \code{\linkS4class{DArch}} instance
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{RBM}}, 
#'   \code{\link{trainRBM}}
#' @include darch.Class.R
#' @include dataset.R
#' @keywords internal
setGeneric(
  name = "preTrainDArch",
  def = function(darch, dataSet, dataSetValid = NULL, numEpochs = 1, numCD = 1,
               lastLayer = 0, isClass = F, consecutive = T, ...)
      {standardGeneric("preTrainDArch")}
)

#' Pre-trains a \code{\linkS4class{DArch}} network
#' 
#' @inheritParams preTrainDArch
#' @seealso \link{preTrainDArch}
#' @keywords internal
setMethod(
  f = "preTrainDArch",
  signature = "DArch",
  definition = function(darch, dataSet, dataSetValid = NULL, numEpochs = 1,
                      numCD = 1, lastLayer = 0, isClass = F, consecutive = T,
                      ...)
  {
    if (!validateDataSet(dataSet, darch))
    {
      stop(futile.logger::flog.error("Invalid dataset provided."))
    }
    
    futile.logger::flog.info("Starting pre-training for %s epochs", numEpochs)

    # Use validation data as well
    if (!is.null(dataSetValid) && getParameter(".rbm.allData"))
    {
      dataSet@data <- rbind(dataSet@data, dataSetValid@data)
      
      if (!is.null(dataSet@targets))
      {
        dataSet@targets <- rbind(dataSet@targets, dataSetValid@targets)
      }
    }
    
    futile.logger::flog.info("Training set consists of %d samples",
                             nrow(dataSet@data))
    
    timeStart <- Sys.time()
    validData <- if (!is.null(dataSetValid)) dataSetValid@data else NULL
    validTargets <- if (!is.null(dataSetValid)) dataSetValid@targets else NULL
    rbmList <- darch@rbmList
    numRbms <- length(rbmList)
    layers <- getParameter(".layers")

    # Autoencoder detecton
    if (numRbms %% 2 == 0 && all(rev(layers) == layers))
    {
      futile.logger::flog.info("Pre-training network as autoencoder")
      
      darch@parameters[[".rbm.autoencoder"]] <- T
      numRbms <- numRbms/2
    }
    
    iterationsRbms <- (if (lastLayer <= 0) max(numRbms + lastLayer, 1)
      else min(lastLayer, numRbms))
    outerIterations <- if (consecutive) 1 else numEpochs
    numEpochs <- if (consecutive) numEpochs else 1
    
    futile.logger::flog.info("The first %s RBMs are going to be trained",
      iterationsRbms)
    
    for (j in 1:outerIterations)
    {
      trainData <- dataSet@data
      
      for (i in 1:iterationsRbms)
      {
        rbmList[i] <-
          trainRBM(rbmList[[i]], trainData, numEpochs, numCD, ...,
          net = darch)
        trainData <- rbmList[[i]]@output
        darch@layers[[i]][["weights"]] <-
          rbind(rbmList[[i]]@weights, rbmList[[i]]@hiddenBiases)
        
        if (!is.null(darch@parameters[[".rbm.autoencoder"]]))
        {
          darch@layers[[(numRbms * 2 + 1) - i]][["weights"]] <-
            rbind(t(rbmList[[i]]@weights), rbmList[[i]]@visibleBiases)
        }
        
        # Print current error
        testDArch(darch, dataSet@data, dataSet@targets, "Train set", isClass)
        
        if (!is.null(validData))
        {
          testDArch(darch, validData, validTargets,
                    "Validation set", isClass)
        }
      }
    }
    
    darch@rbmList <- rbmList
    
    # TODO record this individually for each RBM?
    darch@parameters[[".rbm.numEpochsTrained"]] <- rbmList[[1]]@epochs
    stats <- darch@stats
    
    timeEnd <- Sys.time()
    preTrainTime <- as.double(difftime(timeEnd, timeStart, units = "secs"))
    stats[["preTrainTime"]] <-
      stats[["preTrainTime"]] + preTrainTime
    
    darch@stats <- stats
    
    futile.logger::flog.info("Pre-training finished after %s",
              format(difftime(timeEnd, timeStart)))
    
    darch
  }
)

#' Fine tuning function for the deep architecture
#' 
#' The fine tuning function for deep architectures. This function use the 
#' function saved in the attribute \code{fineTuneFunction} to train the deep 
#' architecture.
#' 
#' @details The function trains the given network \code{darch} with the function
#'   saved in the attribute \code{fineTuneFunction} of the 
#'   \code{\linkS4class{DArch}}-Object. The data and classes for validation and
#'   testing are optional. If they are provided the network will be executed
#'   with this datasets and statistics will be calculated. This statistics are
#'   saved in the \code{stats} attribute (see \code{\linkS4class{Net}}). Also it
#'   is possible to set stop criteria for the training on the error 
#'   (\code{stopErr}, \code{stopValidErr}) or the correct classifications 
#'   (\code{stopClassErr}, \code{stopValidClassErr}) of the training or 
#'   validation dataset.
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param dataSet \code{\linkS4class{DataSet}} containing training and 
#'   optionally validation and test data.
#' @param dataSetValid \code{\linkS4class{DataSet}} to be used for validation.
#' @param ... Additional parameters for the training function
#' @param numEpochs The number of training iterations
#' @param bootstrap Whether to use bootstrapping to create validation data.
#' @param isClass Indicates whether the training is for a classification net. 
#'   When \code{TRUE} then statistics for classification will be determind. 
#'   Default is \code{TRUE}
#' @param stopErr Stop criteria for the error on the train data. Default is 
#'   \code{-Inf}
#' @param stopClassErr Stop criteria for the classification error on the train 
#'   data. Default is \code{101}
#' @param stopValidErr Stop criteria for the error on the validation data. 
#'   Default is \code{-Inf}.
#' @param stopValidClassErr Stop criteria for the classification error on the 
#'   validation data. Default is \code{101}.
#' @param shuffleTrainData Whether to shuffle train data before each epoch.
#' @param debugMode Whether to enable debug mode, internal parameter.
#' @return Trained \code{\linkS4class{DArch}} instance.
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{Net}}, 
#'   \code{\link{backpropagation}}, \code{\link{rpropagation}}, 
#'   \code{\link{minimizeAutoencoder}}, \code{\link{minimizeClassifier}}
#' @keywords internal
setGeneric(
  name = "fineTuneDArch",
  def = function(darch, dataSet, dataSetValid = NULL, numEpochs = 1,
               isClass = TRUE, stopErr = -Inf,
               stopClassErr = 101, stopValidErr = -Inf,
               stopValidClassErr = 101, shuffleTrainData = T, debugMode = F,
               ...)
  {standardGeneric("fineTuneDArch")}
)

# TODO make datasets more dynamic (process list)

#' Fine tuning function for the deep architecture
#' 
#' @inheritParams fineTuneDArch
#' @seealso \link{fineTuneDArch}
#' @keywords internal
setMethod(
  f = "fineTuneDArch",
  signature = "DArch",
  definition = function(darch, dataSet, dataSetValid = NULL, numEpochs = 1,
    isClass = TRUE, stopErr = -Inf, stopClassErr = 101,
    stopValidErr = -Inf, stopValidClassErr = 101,
    shuffleTrainData = getParameter(".shuffleTrainData", T),
    debugMode = getParameter(".debug", F), ...)
  {
    # delete rbmList, not needed from this point onwards
    darch@rbmList <- list()
    timeStart <- Sys.time()
    darch@epochs <- max(darch@epochs, length(darch@stats$times))
    
    if (!validateDataSet(dataSet, darch) ||
          (!is.null(dataSetValid) && !validateDataSet(dataSetValid, darch)))
    {
      stop(futile.logger::flog.error("Invalid dataset provided."))
    }
    
    stats <- darch@stats

    returnBestModel <- getParameter(".darch.returnBestModel")
    dot632Const <-
      getParameter(".darch.returnBestModel.validationErrorFactor")
    autosave <- getParameter(".autosave")
    autosave.dir <- getParameter(".autosave.dir")
    autosave.epochs <- getParameter(".autosave.epochs")
    autosave.trim <- getParameter(".autosave.trim")
    
    if (autosave)
    {
      prepareBenchmarkDirectory(autosave.dir, save = T, continue = T,
        delete = F)
    }
    
    trainData <- dataSet@data
    trainTargets <- dataSet@targets
    
    numRows <- nrow(dataSet@data)
    stats[["numPatterns"]][["train"]] <- numRows
    
    futile.logger::flog.info("Training set consists of %d samples.", numRows)
    
    validData <- NULL
    validTargets <- NULL
    
    if (!is.null(dataSetValid))
    {
      validData <- dataSetValid@data
      validTargets <- dataSetValid@targets
      
      futile.logger::flog.info("Validation set consists of %d samples",
        nrow(validData))
      
      stats[["numPatterns"]][["valid"]] <- nrow(validData)
    }
    
    # Dropout
    dropout <- getParameter(".darch.dropout")
    dropoutOneMaskPerEpoch <- getParameter(".darch.dropout.oneMaskPerEpoch")
    requiredDropoutLength <- length(darch@layers) +
      getParameter(".darch.dropout.dropConnect")
    # If no vector is given, take this to be hidden layer dropout only
    if (length(dropout) == 1)
    {
      dropout <- rep(dropout, requiredDropoutLength - 1)
    }
    
    # Fix dropout vector length or abort with an error
    if (length(dropout) != requiredDropoutLength)
    {
      # Prepend 0 for input dropout if missing
      if (length(dropout) == (requiredDropoutLength - 1))
      {
        dropout <- c(0, dropout)
      }
      else
      {
        stop(futile.logger::flog.error(paste("Invalid length of \"dropout\"",
          "parameter, needs to be one of 1, %s or %s, is %s"),
          requiredDropoutLength - 1, requiredDropoutLength,
          length(dropout)))
      }
    }
    
    darch@parameters[[".darch.dropout"]] <- dropout
    
    batchSize <- getParameter(".darch.batchSize")
    dither <- getParameter(".darch.dither")
    # Dither column mask: don't apply dither to columns with two levels
    ditherMask <- (if (dither)
      apply(trainData, 2, function(c) { (length(unique(c)) > 2)*1 }) else NULL)
    
    futile.logger::flog.info(
      "Start deep architecture fine-tuning for %s epochs", numEpochs)
    
    ret <- makeStartEndPoints(batchSize, numRows)
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    futile.logger::flog.info("Number of Batches: %s (batch size %s)",
      numBatches, batchSize)
    startEpoch <- darch@epochs
    endEpoch <- startEpoch + numEpochs
    numDigitsEpochs <- floor(log(endEpoch, 10)) + 1
    errorBest <- list("raw" = Inf, "class" = Inf)
    modelBest <- darch
    fineTuneFunction <- getParameter(".darch.fineTuneFunction")
    
    for (i in c((startEpoch + 1):endEpoch))
    {
      timeEpochStart <- Sys.time()
      futile.logger::flog.info(paste0("Epoch: %", numDigitsEpochs, "s of %s"),
        i - startEpoch, numEpochs)
      
      # shuffle data for each epoch if enabled
      randomSamples <-
        if (shuffleTrainData) sample(1:numRows, size = numRows) else 1:numRows
      randomData <- trainData[randomSamples,, drop = F]
      randomTargets <- trainTargets[randomSamples,, drop = F]
      
      # apply dither in-place
      if (dither)
      {
        ditherCpp(randomData, ditherMask)
      }
      
      # generate dropout masks for this epoch
      if (any(dropout > 0))
      {
        darch <- generateDropoutMasksForDarch(darch)
      }
      
      darch@epochs <- darch@epochs + 1
      
      for (j in 1:numBatches)
      {
        start <- batchValues[[j]] + 1
        end <- batchValues[[j + 1]]
        
        # generate new dropout masks for batch if necessary
        if (any(dropout > 0) && !dropoutOneMaskPerEpoch)
        {
          darch <- generateDropoutMasksForDarch(darch)
        }
        
        darch <-
          fineTuneFunction(darch, randomData[start:end,, drop = F],
                                 randomTargets[start:end,, drop = F], ...)
      }
      
      error <- list("raw" = 0, "class" = 0)
      
      if (!is.null(trainTargets))
      {
        # Network error
        out <- testDArch(darch, trainData, trainTargets, "Train set", isClass)
        stats[[1]][[1]] <- c(stats[[1]][[1]],out[1])
        stats[[1]][[2]] <- c(stats[[1]][[2]],out[2])
        error[["raw"]] <- error[["raw"]] + out[1] * (1 - dot632Const)
        error[["class"]] <-
          (if (!is.na(out[2])) error[["class"]] + out[2] * (1 - dot632Const)
          else Inf)
        
        # TODO solve cleaner
        if (is.na(out[1]))
        {
          stop(futile.logger::flog.error(paste("The network error is not",
            "numeric. This may be caused by numeric overflows/underflows,",
            "or by faulty activation functions. Try to reduce the learn rates",
            "and/or activate weight normalization and see if the problem",
            "persists.")))
        }
        
        if (out[1] <= stopErr)
        {
          darch@cancel <- TRUE
          darch@cancelMessage <-
            paste0("The new error (", out[1],") on the train data is smaller ",
            "than or equal to the minimum error (", stopErr, ").")
        }
        
        if (isClass && out[2] <= stopClassErr )
        {
          darch@cancel <- TRUE
          darch@cancelMessage <-
            paste0("The new classification error (", out[2],") on the ",
            "training data is smaller than or equal to the minimum ",
            "classification error (", stopClassErr, ").")
        }
        
        # Validation error
        if (!is.null(validData))
        {
          validTimeStart <- Sys.time()
          out <- testDArch(darch, validData, validTargets, "Validation set",
                           isClass)
          stats[[2]][[1]] <- c(stats[[2]][[1]],out[1])
          stats[[2]][[2]] <- c(stats[[2]][[2]],out[2])
          error[["raw"]] <- error[["raw"]] + out[1] * dot632Const
          error[["class"]] <-
            (if (!is.na(out[2])) error[["class"]] + out[2] * dot632Const
            else Inf)
          
          stats[["validTime"]] <- stats[["validTime"]] +
            as.double(difftime(Sys.time(), validTimeStart, units = "secs"))
          
          if (out[1] <= stopValidErr )
          {
            darch@cancel <- TRUE
            darch@cancelMessage <-
              paste0("The new error (", out[[2]][1],
              ") on the validation data is smaller than or equal to the ",
              "minimum error (", stopValidErr, ").")
          }
          
          if (isClass && out[2] <= stopValidClassErr )
          {
            darch@cancel <- TRUE
            darch@cancelMessage <-
              paste0("The new classification error (", out[2],
              ") on the validation data is smaller than or equal to the ",
              "minimum classification error (", stopValidClassErr, ").")
          }
        }
        
        # Set .632+ errors
        stats[[3]][[1]] <- c(stats[[3]][[1]], error[["raw"]])
        stats[[3]][[2]] <- c(stats[[3]][[2]], error[["class"]])
      }
      
      # TODO add parameter darch.cancelFile and document this feature
      if (file.exists("DARCH_CANCEL"))
      {
        darch@cancel <- TRUE
        darch@cancelMessage <-
          paste0("File DARCH_CANCEL found in the working directory.")
      }
      
      stats[["times"]][i] <-
        as.double(difftime(Sys.time(), timeEpochStart, units = "secs"))
      
      darch@stats <- stats
      if (returnBestModel)
      {
        if (error[["class"]] < errorBest[["class"]] ||
            (error[["raw"]] <= errorBest[["raw"]]
            && error[["class"]] == errorBest[["class"]]))
        {
          errorBest <- error
          modelBest <- darch
        }
        else
        {
          modelBest@stats <- stats
        }
      }
      
      if (autosave && autosave.epochs > 0 &&
            ((i - startEpoch) %% autosave.epochs) == 0)
      {
        futile.logger::flog.info("Autosaving current model to '%s'",
          autosave.dir)
        
        autosave(darch, autosave.dir, autosave.trim, numDigitsEpochs)
      }
      
      # debug output
      if (debugMode)
      {
        for (j in 1:(length(darch@layers)))
        {
          futile.logger::flog.debug("Weights standard deviation layer %s: %s",
                                    j, sd(darch@layers[[j]][["weights"]]))
          futile.logger::flog.debug("Weights at zero in layer %s: %s",
            j, sum(abs(darch@layers[[j]][["weights"]]) < 1e-10))
        }
      }
      
      futile.logger::flog.info(
        paste0("Finished epoch %", numDigitsEpochs,
               "s of %s after %s (%.0f patterns/sec)"), i - startEpoch,
        numEpochs, format(difftime(Sys.time(), timeEpochStart), digits = 3),
        numRows / stats[["times"]][i])
      
      if (darch@cancel)
      {
        futile.logger::flog.info("The training is canceled:")
        futile.logger::flog.info(darch@cancelMessage)
        darch@cancelMessage <- "No reason specified."
        darch@cancel <- FALSE
        break
      }
    }
    
    if (returnBestModel)
    {
      darch <- modelBest
      testDArch(darch, trainData, trainTargets, "Train set (best model)",
                isClass)
      
      if (!is.null(validData))
      {
        testDArch(darch, validData, validTargets,
                  "Validation set (best model)", isClass)
      }
      
      futile.logger::flog.info("Best model was found after epoch %s",
                               darch@epochs - startEpoch)
    }
    
    if (!is.null(validData))
    {
      rawErrorFunctionName <-
        getErrorFunctionName(getParameter(".darch.errorFunction"))
      futile.logger::flog.info("Final %.3f validation %s: %.3f", dot632Const,
        rawErrorFunctionName,
        (1 - dot632Const) * darch@stats$trainErrors$raw[darch@epochs] +
        dot632Const * darch@stats$validErrors$raw[darch@epochs])
      
      if (isClass)
      {
        futile.logger::flog.info(
          "Final %.3f validation classification error: %.2f%%", dot632Const,
          (1 - dot632Const) * darch@stats$trainErrors$class[darch@epochs] +
          dot632Const * darch@stats$validErrors$class[darch@epochs])
      }
    }
    
    timeEnd <- Sys.time()
    fineTuneTime <- as.double(difftime(timeEnd, timeStart, units = "secs"))
    darch@stats[["fineTuneTime"]] <-
      darch@stats[["fineTuneTime"]] + fineTuneTime
    
    futile.logger::flog.info(paste("Fine-tuning finished after",
                    format(difftime(timeEnd, timeStart), digits = 4)))
    
    if (autosave)
    {
      autosave(darch, autosave.dir, autosave.trim, numDigitsEpochs)
    }
    
    darch
  }
)
