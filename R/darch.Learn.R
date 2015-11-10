# Copyright (C) 2013-2015 Martin Drees
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
#' @param trainOutputLayer Logical indicating whether to train the output layer
#'  RBM.
#' @return Trained \code{\linkS4class{DArch}} instance
#' @seealso \code{\linkS4class{DArch}}, \code{\linkS4class{RBM}}, 
#'   \code{\link{trainRBM}}
#' @include darch.R
#' @include dataset.R
#' @export
setGeneric(
  name="preTrainDArch",
  def=function(darch, dataSet, numEpochs = 1, numCD = 1, ..., trainOutputLayer = F)
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
                      numCD = 1, ..., trainOutputLayer = F)
  {
    if (!validateDataSet(dataSet, darch))
    {
      stop("Invalid dataset provided.")
    }

    timeStart <- Sys.time()
    trainData <- dataSet@data
    darch@dataSet <- dataSet
    darch@preTrainParameters[["numCD"]] <- numCD
    rbmList <- getRBMList(darch)
    
    flog.info("Start DArch pre-training")
    for(i in 1:(length(rbmList)-(!trainOutputLayer))){
      rbmList[i] <- trainRBM(rbmList[[i]], trainData, numEpochs, numCD, ...)
      trainData <- getOutput(rbmList[[i]])
      setLayerWeights(darch,i) <- rbind(getWeights(rbmList[[i]]),getHiddenBiases(rbmList[[i]]))
    }
    
    setRBMList(darch) <- rbmList
    darch@preTrainParameters[["numEpochs"]] <- getEpochs(rbmList[[1]])
    stats <- getStats(darch)
    
    stats[["preTrainTime"]] <-
      stats[["preTrainTime"]] + as.double(Sys.time() - timeStart, "secs")
    stats[["batchSize"]] <- getBatchSize(darch)
    
    setStats(darch) <- stats
    
    flog.info("Pre-training finished")
    return(darch)
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
#'   \code{\linkS4class{DArch}}-Object. The data (\code{trainData}, 
#'   \code{validData}, \code{testData}) and belonging classes of the data 
#'   (\code{targetData}, \code{validTargets}, \code{testTargets})  can be hand 
#'   over either as matrix or as ff-matrix (see package \link{ff} for details). 
#'   The data and classes for validation and testing are optional. If they are 
#'   provided the network will be executed with this datasets and statistics 
#'   will be calculated. This statistics are saved in the \code{stats} attribute
#'   (see \code{\linkS4class{Net}}). The attribute \code{isBin} indicates
#'   whether the output data must be interpreted as binary value. If true every
#'   value over 0.5 is interpreted as 1 otherwise as 0. Also it is possible to
#'   set stop criteria for the training on the error (\code{stopErr}, 
#'   \code{stopValidErr}) or the correct classifications (\code{stopClassErr}, 
#'   \code{stopValidClassErr}) of the training or validation dataset.
#'   
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param dataSet \code{\linkS4class{DataSet}} containing training and 
#'   optionally validation and test data.
#' @param dataSetValid \code{\linkS4class{DataSet}} to be used for validation.
#' @param ... Additional parameters for the training function
#' @param numEpochs The number of training iterations
#' @param bootstrap Whether to use bootstrapping to create validation data.
#' @param isBin Indicates whether the output data must be interpreted as boolean
#'   value. Default is \code{FALSE}. If it is true, every value over 0.5 is 
#'   interpreted as 1 and under as 0.
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
               bootstrap = T, isBin = FALSE, isClass = TRUE, stopErr = -Inf,
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
                      bootstrap = T, isBin = FALSE, isClass = TRUE,
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
    
    # Record parameters
    darch@fineTuningParameters <-
      list(isBin = isBin, isClass = isClass,
           stopErr = stopErr, stopClassErr = stopClassErr,
           stopValidErr = stopValidErr, stopValidClassErr = stopValidClassErr,
           numEpochs = getEpochs(darch),
           bootstrap = bootstrap && is.null(dataSetValid))
    
    trainData <- dataSet@data
    trainTargets <- dataSet@targets
    validData <- if (!is.null(dataSetValid)) dataSetValid@data else NULL
    validTargets <- if (!is.null(dataSetValid)) dataSetValid@targets else NULL
    
    # bootstrapping
    if (bootstrap && is.null(validData))
    {
      numRows <- nrow(dataSet@data)
      bootstrapTrainingSamples <- sample(1:numRows, numRows, replace=T)
      bootstrapValidationSamples <-
        which(!(1:numRows %in% bootstrapTrainingSamples))
      # TODO validate sizes?
      trainData <- dataSet@data[bootstrapTrainingSamples,, drop = F]
      trainTargets <- dataSet@targets[bootstrapTrainingSamples,, drop = F]
      validData <- dataSet@data[bootstrapValidationSamples,, drop = F]
      validTargets <- dataSet@targets[bootstrapValidationSamples,, drop = F]
    }
    
    # Function for testing the network against the given data.#################
    testFunc <- function(darch,data,targets,dataType){
      darch <- getExecuteFunction(darch)(darch,data[])
      execOut <- getExecOutput(darch)
      if (isBin){
        boolOut <- (execOut>0.5)*1
      }else{
        boolOut <- execOut
      }
      
      class <- 0
      if (isClass){
        rows <- nrow(targets)
        cols <- ncol(targets)
        boolOutTargets <- cbind(boolOut, targets)
        class <- sum(apply(boolOutTargets, 1, function(y)
            { any(y[1:cols] != y[(cols+1):(2*cols)])}))
        class <- (class/rows)*100
      }
      tError <- getErrorFunction(darch)(targets[], execOut)
      flog.info(paste(dataType,tError[[1]],tError[[2]]))
      if (isClass){
        flog.info(paste0("Classification error on ",dataType," ",round(class, 2),"%"))  
      }
      return(c(tError[[2]],class))
    }
    ###########################################################################
    
    flog.info("Start deep architecture fine-tuning")
    
    ret <- makeStartEndPoints(getBatchSize(darch),nrow(trainData[]))    
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    stats <- getStats(darch)
    
    if (is.null(getStats(darch)) || length(getStats(darch)) < 1){
      stats <-
        list("dataErrors"=list("raw"=c(),"class"=c()),
             "validErrors"=list("raw"=c(),"class"=c()),
             "times"=c())

      setStats(darch) <- stats
    }
    
    flog.info(paste("Number of Batches: ",numBatches))
    startEpoch <- getEpochs(darch)
    for(i in c((startEpoch+1):(startEpoch+numEpochs))){
      timeEpochStart <- Sys.time()
      flog.info(paste("Epoch:", i - startEpoch, "of", numEpochs))
      
      # generate dropout masks for this epoch
      darch <- generateDropoutMasksForDarch(darch)
      
      darch <- incrementEpochs(darch)
      for(j in 1:numBatches){
        #flog.debug(paste("Epoch", i,"Batch",j))
        start <- batchValues[[j]]+1
        end <- batchValues[[j+1]]
        
        # generate new dropout masks for batch if necessary
        if (!getDropoutOneMaskPerEpoch(darch))
        {
          darch <- generateDropoutMasksForDarch(darch)
        }
        
        darch <-
          darch@fineTuneFunction(darch,
                                 trainData[start:end,, drop = F],
                                 trainTargets[start:end,, drop = F], ...)
      }
      
      stats <- getStats(darch)
      
      if (!is.null(trainTargets))
      {
        # Network error 
        out <- testFunc(darch,trainData[],trainTargets[],"Train set")
        stats[[1]][[1]] <- c(stats[[1]][[1]],out[1])
        stats[[1]][[2]] <- c(stats[[1]][[2]],out[2])
        
        if (out[1] <= stopErr ){
          setCancel(darch) <- TRUE
          setCancelMessage(darch) <-
            paste0("The new error (", out[1],") on the train data is smaller ",
                   "than or equal to the minimum error (", stopErr, ").")
        }
        
        if (out[2] <= stopClassErr ){
          setCancel(darch) <- TRUE
          setCancelMessage(darch) <-
            paste0("The new classification error (", out[2],") on the training ",
                   "data is smaller than or equal to the minimum classification ",
                   "error (", stopClassErr, ").")
        }
        
        # Validation error
        if (!is.null(validData)){
          out <- testFunc(darch,validData[],validTargets[],"Validation set")
          stats[[2]][[1]] <- c(stats[[2]][[1]],out[1])
          stats[[2]][[2]] <- c(stats[[2]][[2]],out[2])
          
          if (out[1] <= stopValidErr ){
            setCancel(darch) <- TRUE
            setCancelMessage(darch) <-
              paste0("The new error (", out[[2]][1],
                     ") on the validation data is smaller than or equal to the ",
                     "minimum error (", stopValidErr, ").")
          }
          
          if (out[2] <= stopValidClassErr ){
            setCancel(darch) <- TRUE
            setCancelMessage(darch) <-
              paste0("The new classification error (", out[2],
                    ") on the validation data is smaller than or equal to the ",
                    "minimum classification error (", stopValidClassErr, ").")
          }
        }
      }
      
      if (file.exists("DARCH_CANCEL"))
      {
        setCancel(darch) <- TRUE
        setCancelMessage(darch) <-
          paste0("File DARCH_CANCEL found in the working directory.")
      }
      
      stats[["times"]][i] <- as.double(Sys.time() - timeEpochStart, "secs") 
      setStats(darch) <- stats
      
      if (getCancel(darch)){
        flog.info("The training is canceled:")
        flog.info( getCancelMessage(darch))
        setCancelMessage(darch) <- "No reason specified."
        setCancel(darch) <- FALSE
        break
      }
    }
    
    stats <- getStats(darch)
    stats[["fineTuneTime"]] <-
      stats[["fineTuneTime"]] + as.double(Sys.time() - timeStart, "secs")
    setStats(darch) <- stats
    
    darch@fineTuningParameters[["numEpochs"]] <- getEpochs(darch)
    darch@fineTuningParameters[["batchSize"]] <- getBatchSize(darch)
    darch@fineTuningParameters[["stats"]] <- stats
    flog.info("Fine-tuning finished")
    return(darch)
  }  
)
