# Copyright (C) 2013-2016 Martin Drees
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
#' @param numEpochs The number of epochs
#' @param numCD The number of CD iterations
#' @param ... Additional parameters for the function \code{\link{trainRBM}}
#' @param lastLayer Numeric indicating after which layer to stop training.
#' @return Trained \code{\linkS4class{DArch}} instance
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{RBM}}, 
#'   \code{\link{trainRBM}}
#' @include darch.R
#' @include dataset.R
#' @export
setGeneric(
  name="preTrainDArch",
  def=function(darch, dataSet, numEpochs = 1, numCD = 1, ..., lastLayer = 0)
      {standardGeneric("preTrainDArch")}
)

#' Pre-trains a \code{\linkS4class{DArch}} network
#' 
#' @inheritParams preTrainDArch
#' @seealso \link{preTrainDArch}
#' @export
setMethod(
  f="preTrainDArch",
  signature="DArch",
  definition=function(darch, dataSet, numEpochs = 1,
                      numCD = 1, ..., lastLayer = 0)
  {
    if (!validateDataSet(dataSet, darch))
    {
      stop("Invalid dataset provided.")
    }

    timeStart <- Sys.time()
    trainData <- dataSet@data
    darch@dataSet <- dataSet
    darch@preTrainParameters[["numCD"]] <- numCD
    rbmList <- darch@rbmList
    numRbms <- length(rbmList)
    
    length <- (if (lastLayer <= 0) max(numRbms + lastLayer, numRbms)
               else min(lastLayer, numRbms))
    
    flog.info("Start DArch pre-training")
    for(i in 1:length)
    {
      rbmList[i] <- trainRBM(rbmList[[i]], trainData, numEpochs, numCD, ...,
                             darch=darch)
      trainData <- rbmList[[i]]@output
      darch@layers[[i]][["weights"]] <- rbind(rbmList[[i]]@weights,
                                         rbmList[[i]]@hiddenBiases)
    }
    
    # TODO delete all but one element of the rbmList (for the print function)?
    #darch@rbmList <- rbmList
    darch@rbmList <- list()
    darch@preTrainParameters[["numEpochs"]] <- rbmList[[1]]@epochs
    stats <- darch@stats
    
    stats[["preTrainTime"]] <-
      stats[["preTrainTime"]] + as.double(Sys.time() - timeStart, "secs")
    stats[["batchSize"]] <- darch@batchSize
    
    darch@stats <- stats
    
    flog.info("Pre-training finished")
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
#'   validation data. Default is \code{101} .
#' @return Trained \code{\linkS4class{DArch}} instance.
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{Net}}, 
#'   \code{\link{backpropagation}}, \code{\link{rpropagation}}, 
#'   \code{\link{minimizeAutoencoder}}, \code{\link{minimizeClassifier}}
#' @include darch.R
#' @include dataset.R
#' @export
setGeneric(
  name="fineTuneDArch",
  def=function(darch, dataSet, dataSetValid = NULL, numEpochs = 1,
               bootstrap = T, isClass = TRUE, stopErr = -Inf,
               stopClassErr = 101, stopValidErr = -Inf, stopValidClassErr = 101,
               ...)
  {standardGeneric("fineTuneDArch")}
)

# TODO make datasets more dynamic (process list)

#' Fine tuning function for the deep architecture
#' 
#' @inheritParams fineTuneDArch
#' @seealso \link{fineTuneDArch}
#' @export
setMethod(
  f="fineTuneDArch",
  signature="DArch",
  definition=function(darch, dataSet, dataSetValid = NULL, numEpochs = 1,
                      bootstrap = T, isClass = TRUE,
                      stopErr = -Inf, stopClassErr = 101, stopValidErr = -Inf,
                      stopValidClassErr = 101, ...)
  {
    timeStart <- Sys.time()
    darch@dataSet <- dataSet
    
    if (!validateDataSet(dataSet, darch) ||
          (!is.null(dataSetValid) && !validateDataSet(dataSetValid, darch)))
    {
      stop("Invalid dataset provided.")
    }
    
    bootstrap <- bootstrap && is.null(dataSetValid)
    returnBestModel <- getDarchParam("darch.returnBestModel", F, darch)
    autosave <- getDarchParam("autosave", F, darch)
    autosave.location <- getDarchParam("autosave.location", "./darch", darch)
    autosave.epochs <- getDarchParam("autosave.epochs", round(numEpochs / 20),
                                     darch)
    
    # Record parameters
    darch@fineTuningParameters <-
      list(isClass = isClass,
           stopErr = stopErr, stopClassErr = stopClassErr,
           stopValidErr = stopValidErr, stopValidClassErr = stopValidClassErr,
           numEpochs = darch@epochs,
           bootstrap = bootstrap,
           returnBestModel = returnBestModel)
    
    trainData <- dataSet@data
    trainTargets <- dataSet@targets
    validData <- if (!is.null(dataSetValid)) dataSetValid@data else NULL
    validTargets <- if (!is.null(dataSetValid)) dataSetValid@targets else NULL
    
    numRows <- nrow(dataSet@data)
    
    # bootstrapping
    if (bootstrap)
    {
      bootstrapTrainingSamples <- sample(1:numRows, numRows, replace=T)
      bootstrapValidationSamples <-
        which(!(1:numRows %in% bootstrapTrainingSamples))
      # TODO validate sizes?
      trainData <- dataSet@data[bootstrapTrainingSamples,, drop = F]
      trainTargets <- dataSet@targets[bootstrapTrainingSamples,, drop = F]
      validData <- dataSet@data[bootstrapValidationSamples,, drop = F]
      validTargets <- dataSet@targets[bootstrapValidationSamples,, drop = F]
    }
    
    flog.info("Start deep architecture fine-tuning")
    
    ret <- makeStartEndPoints(darch@batchSize, nrow(trainData[]))    
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    stats <- darch@stats
    
    if (is.null(darch@stats) || length(darch@stats) < 1){
      stats <-
        list("dataErrors"=list("raw"=c(),"class"=c()),
             "validErrors"=list("raw"=c(),"class"=c()),
             "times"=c())
    }
    
    flog.info(paste("Number of Batches: ", numBatches))
    startEpoch <- darch@epochs
    errorBest <- Inf
    modelBest <- darch
    for(i in c((startEpoch + 1):(startEpoch + numEpochs))){
      timeEpochStart <- Sys.time()
      flog.info(paste("Epoch:", i - startEpoch, "of", numEpochs))
      
      # shuffle data for each epoch
      # TODO make shuffling configurable for debugging?
      randomSamples <- sample(1:numRows, size=numRows)
      trainData <- trainData[randomSamples,, drop = F]
      trainTargets <- trainTargets[randomSamples,, drop = F]
      
      # apply dither in-place
      if (darch@dither)
      {
        ditherCpp(trainData)
      }
      
      # generate dropout masks for this epoch
      if (darch@dropoutHidden > 0 || darch@dropoutInput > 0)
      {
        darch <- generateDropoutMasksForDarch(darch)
      }
      
      darch@epochs <- darch@epochs + 1
      for(j in 1:numBatches){
        #flog.debug(paste("Epoch", i,"Batch",j))
        start <- batchValues[[j]]+1
        end <- batchValues[[j+1]]
        
        # generate new dropout masks for batch if necessary
        if ((darch@dropoutHidden > 0 || darch@dropoutInput > 0) &&
              !darch@dropoutOneMaskPerEpoch)
        {
          darch <- generateDropoutMasksForDarch(darch)
        }
        
        darch <-
          darch@fineTuneFunction(darch, trainData[start:end,, drop = F],
                                 trainTargets[start:end,, drop = F], ...)
      }
      
      error <- 0
      
      if (!is.null(trainTargets))
      {
        # Network error 
        out <- testDArch(darch, trainData, trainTargets, "Train set", isClass)
        stats[[1]][[1]] <- c(stats[[1]][[1]],out[1])
        stats[[1]][[2]] <- c(stats[[1]][[2]],out[2])
        errorIndex <- if(isClass) 2 else 1
        error <- error + out[errorIndex] * .37
        
        if (out[1] <= stopErr )
        {
          darch@cancel <- TRUE
          darch@cancelMessage <-
            paste0("The new error (", out[1],") on the train data is smaller ",
                   "than or equal to the minimum error (", stopErr, ").")
        }
        
        if (out[2] <= stopClassErr )
        {
          darch@cancel <- TRUE
          darch@cancelMessage <-
            paste0("The new classification error (", out[2],") on the training ",
                   "data is smaller than or equal to the minimum classification ",
                   "error (", stopClassErr, ").")
        }
        
        # Validation error
        if (!is.null(validData))
        {
          out <- testDArch(darch, validData, validTargets, "Validation set",
                           isClass)
          stats[[2]][[1]] <- c(stats[[2]][[1]],out[1])
          stats[[2]][[2]] <- c(stats[[2]][[2]],out[2])
          error <- error + out[errorIndex] * .63
          
          if (out[1] <= stopValidErr )
          {
            darch@cancel <- TRUE
            darch@cancelMessage <-
              paste0("The new error (", out[[2]][1],
                     ") on the validation data is smaller than or equal to the ",
                     "minimum error (", stopValidErr, ").")
          }
          
          if (out[2] <= stopValidClassErr )
          {
            darch@cancel <- TRUE
            darch@cancelMessage <-
              paste0("The new classification error (", out[2],
                    ") on the validation data is smaller than or equal to the ",
                    "minimum classification error (", stopValidClassErr, ").")
          }
        }
        
        #for (k in 1:length(darch@layers))
        #{
        #  flog.debug("Weight norms in layer %d: %s", k, paste(range(sqrt(colSums(getLayerWeights(darch, k)^2))), collapse=" "))
        #}
      }

      #flog.debug("Momentum epoch %d: %f, learn rate: %f", i,
      #          getMomentum(darch), darch@learnRate*(1 - getMomentum(darch)))
      
      # update learn rate
      darch@learnRate <- darch@learnRate * darch@learnRateScale
      
      if (file.exists("DARCH_CANCEL"))
      {
        darch@cancel <- TRUE
        darch@cancelMessage <-
          paste0("File DARCH_CANCEL found in the working directory.")
      }
      
      stats[["times"]][i] <- as.double(Sys.time() - timeEpochStart, "secs")
      
      if (returnBestModel)
      {
        if (error < errorBest)
        {
          errorBest <- error
          modelBest <- darch
        }
        else
        {
          modelBest@stats <- stats
        }
      }
      else
      {
        darch@stats <- stats
      }
      
      if (autosave && autosave.epochs > 0 &&
            ((i - startEpoch) %% autosave.epochs) == 0)
      {
        saveDArch(if (returnBestModel) modelBest else darch, autosave.location)
      }
      
      if (darch@cancel)
      {
        flog.info("The training is canceled:")
        flog.info(darch@cancelMessage)
        darch@cancelMessage <- "No reason specified."
        darch@cancel <- FALSE
        break
      }
    }
    
    if (returnBestModel)
    {
      darch <- modelBest
      testDArch(darch, trainData, trainTargets, "Train set using best model",
                isClass)
      
      if (!is.null(validData))
      {
        testDArch(darch, validData, validTargets,
                  "Validation set using best model", isClass)
      }
    }
    
    darch@stats[["fineTuneTime"]] <-
      darch@stats[["fineTuneTime"]] + as.double(Sys.time() - timeStart, "secs")
    
    darch@fineTuningParameters[["numEpochs"]] <- darch@epochs
    darch@fineTuningParameters[["batchSize"]] <- darch@batchSize
    darch@fineTuningParameters[["stats"]] <- stats
    flog.info("Fine-tuning finished")
    
    if (autosave)
    {
      saveDArch(darch, autosave.location)
    }
    
    darch
  }
)
