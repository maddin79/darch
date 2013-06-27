#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname preTrainDArch-methods
setGeneric(
  name="preTrainDArch",
  def=function(darch,trainData,maxEpoch=1,numCD=1,...){standardGeneric("preTrainDArch")}
)

#' @rdname preTrainDArch-methods
#' @aliases preTrainDArch,DArch-method
setMethod(
  f="preTrainDArch",
  signature="DArch",
  definition=function(darch,trainData,maxEpoch=1,numCD=1,...){
    rbmList <- getRBMList(darch)
    log4r::info(getLogger(darch),"Start DArch pre-training")
    for(i in 1:length(rbmList)){					
      rbmList[i] <- trainRBM(rbmList[[i]],trainData,maxEpoch,numCD,...)
      trainData <- getOutput(rbmList[[i]])
      setLayerWeights(darch,i) <- rbind(getWeights(rbmList[[i]]),getHiddenBiases(rbmList[[i]]))
    }
    setRBMList(darch) <- rbmList
    log4r::info(getLogger(darch),"Pre-training finished")
    return(darch)
  }
)

#' Fine tuning function for the deep architecture.
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param trainData The training data
#' @param targetData The expected output for the training data
#' @param ... Additional parameters for the training function
#' @param maxEpoch The number of training iterations
#' @param isBin Indicates whether the output data must be interpreted as
#'                boolean value. Default is \code{FALSE}. If it is true, every
#'                value over 0.5 is interpreted as 1 and under as 0.
#' @param isClass Indicates whether the training is for a classification net.
#'                When \code{TRUE} then statistics for classification will be 
#'                determind. Default is \code{TRUE}
#' @param validData Data for validating the network. Default is \code{NULL}
#' @param validTargets The expected output for the training data Default is \code{NULL}
#' @param testData Data for testing the network. Default is \code{NULL}
#' @param testTargets The expected output for the training data Default is \code{NULL}
#' @param stopErr Stop criteria for the error on the train data. Default is \code{-Inf}
#' @param stopClassErr Stop criteria for the classification error on the train data. Default is \code{101}
#' @param stopValidErr Stop criteria for the error on the validation data. Default is \code{-Inf}.
#' @param stopValidClassErr Stop criteria for the classification error on the validation data. Default is \code{101} .
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname fineTuneDArch-methods
setGeneric(
  name="fineTuneDArch",
  def=function(darch,trainData,targetData,...,maxEpoch=1,isBin=FALSE,isClass=TRUE,
               validData=NULL,validTargets=NULL,testData=NULL,testTargets=NULL,
               stopErr=-Inf,stopClassErr=101,stopValidErr=-Inf,
               stopValidClassErr=101){standardGeneric("fineTuneDArch")}
)

#' @rdname fineTuneDArch-methods
#' @aliases fineTuneDArch,DArch-method
setMethod(
  f="fineTuneDArch",
  signature="DArch",
  definition=function(darch,trainData,targetData,...,maxEpoch=1,isBin=FALSE,isClass=TRUE,
                      validData=NULL,validTargets=NULL,testData=NULL,
                      testTargets=NULL,stopErr=-Inf,stopClassErr=101,
                      stopValidErr=-Inf,stopValidClassErr=101){
    # Standardabweichung
    stdabw <- function(x) {n=length(x) ; sqrt(var(x) * (n-1) / n)}
    
    # Function for testing the network against the given data.#################
    testFunc <- function(darch,data,targets,dataType){
      darch <- getExecuteFunction(darch)(darch,data[])
      execOut <- getExecOutput(darch)
      if(isBin){
        boolOut <- (execOut>0.5)*1
      }else{
        boolOut <- execOut
      }
      class <- 0
      if(isClass){
        rows <- nrow(targets)
        for(h in 1:rows){
          rowsums <- sum(boolOut[h,]==targets[h,])
          if(rowsums == ncol(boolOut)){
            class <- class+1
          }
        }
        class <- (class*100)/rows
      }
      tError <- getErrorFunction(darch)(targets[], execOut)
      log4r::info(logger,paste(dataType,tError[[1]],tError[[2]]))
      if(isClass){
        log4r::info(logger,paste("Correct classifications on ",dataType,class,"%"))  
      }
      return(c(tError[[2]],class))
    }
    ###########################################################################
    
    logger <- getLogger(darch)
    log4r::info(logger,"Start deep architecture fine-tuning")
    
    ret <- makeStartEndPoints(getBatchSize(darch),nrow(trainData[]))    
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    if(is.null(getStats(darch)) || length(getStats(darch)) < 1){
      stats <- list("DataStats"=list("Errors"=c(),"CorrectClassifications"=c()),
                    "ValidStats"=list("Errors"=c(),"CorrectClassifications"=c()),
                    "TestStats"=list("Errors"=c(),"CorrectClassifications"=c()))
#       stats <- list("DataStats"=list("Errors"=c(),"CorrectClassifications"=c()),
#                     "ValidStats"=list("Errors"=c(),"CorrectClassifications"=c()),
#                     "TestStats"=list("Errors"=c(),"CorrectClassifications"=c()),
#                     "WeightChanges"=list())
      setStats(darch) <- stats
    }
    
    log4r::debug(logger,paste("Number of Batches: ",numBatches))
    for(i in c(1:maxEpoch)){
      log4r::info(logger,paste("Epoch", i,"----------"))
      
#       # Save weights for calculate the weight change per Epoch
#       numLayers <- length(getLayers(darch))
#       oldWeights <- list()
#       for(n in 1:numLayers){
#         oldWeights[[n]] <- getLayerWeights(darch,n)
#       }
      ########################################################
      for(j in 1:numBatches){
        log4r::debug(logger,paste("Epoch", i,"Batch",j))
        start <- batchValues[[j]]+1
        end <- batchValues[[j+1]]
        darch <- darch@fineTuneFunction(darch,trainData[start:end,],targetData[start:end,],i,...)
        
        if(getCancel(darch)){
          log4r::info(logger,"The training is canceled:")
          log4r::info(logger, getCancelMessage(darch))
          setCancelMessage(darch) <- "No reason specified."
          setCancel(darch) <- FALSE
          return(darch)
        }
      }
      
      stats <- getStats(darch)
      # Network error 
      out <- testFunc(darch,trainData[],targetData[],"Train set")
      stats[[1]][[1]] <- c(stats[[1]][[1]],out[1])
      stats[[1]][[2]] <- c(stats[[1]][[2]],out[2])
      
      if(out[1] <= stopErr ){
        setCancel(darch) <- TRUE
        setCancelMessage(darch) <- paste("The new error (", out[[2]][1],") on the train data is smaller than the max error (",stopErr,").",sep="")
      }
      
      if(out[2] >= stopClassErr ){
        setCancel(darch) <- TRUE
        setCancelMessage(darch) <- paste("The new classification error (", out[[2]][2],") is bigger than the max classification error (",stopClassErr,").",sep="")
      }
      
      # Validation error
      if(!is.null(validData)){
        out <- testFunc(darch,validData[],validTargets[],"Validation set")
        stats[[2]][[1]] <- c(stats[[2]][[1]],out[1])
        stats[[2]][[2]] <- c(stats[[2]][[2]],out[2])
        
        if(out[1] <= stopValidErr ){
          setCancel(darch) <- TRUE
          setCancelMessage(darch) <- paste("The new error (", out[[2]][1],
                                           ") on the validation data is smaller than the max error (",
                                           stopErr,").",sep="")
        }
        
        if(out[2] >= stopValidClassErr ){
          setCancel(darch) <- TRUE
          setCancelMessage(darch) <- paste("The new classification error (", out[[2]][2],
                                           ") is on the validation data bigger than the max classification error (",
                                           stopClassErr,").",sep="")
        }
      }
      
#       # Calculate the weights change per Epoch
#       for(n in 1:numLayers){
#         if(i <= 1){
#           stats[["WeightChanges"]][[n]] <- matrix(0,maxEpoch,2)
#         }
#         weights <- getLayerWeights(darch,n)
#         change <- c(abs(oldWeights[[n]]-weights))
#         std <- stdabw(change)
#         stats[["WeightChanges"]][[n]][i,] <- c(mean(change),std)
#       }
      
      # Test error
      if(!is.null(testData)){
        out <- testFunc(darch,testData[],testTargets[],"Test set")
        stats[[3]][[1]] <- c(stats[[3]][[1]],out[1])
        stats[[3]][[2]] <- c(stats[[3]][[2]],out[2])
      }
      
      setStats(darch) <- stats
      
      if(getCancel(darch)){
        log4r::info(logger,"The training is canceled:")
        log4r::info(logger, getCancelMessage(darch))
        setCancelMessage(darch) <- "No reason specified."
        setCancel(darch) <- FALSE
        return(darch)
      }
    }
    
    log4r::info(logger,"Fine-tuning finished")
    return(darch)
  }  
)