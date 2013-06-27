#' Train an RBM with Contrastiv Divergence
#' 
#' BEARBEITEN
#'
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param trainData The data for the training
#' @param maxEpoch The number of training iterations
#' @param numCD Number of contrastive divergence iterations
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname trainRBM-methods
setGeneric(
  name="trainRBM",
  def=function(rbm,trainData,maxEpoch=1,numCD=1,...){standardGeneric("trainRBM")}
)

#' @rdname trainRBM-methods
#' @aliases trainRBM,RBM-method
setMethod(
  f="trainRBM",
  signature=c("RBM"),
  definition=function(rbm,trainData,maxEpoch=1,numCD=1,...){
    # make start and end points for the batches
    logger <- getLogger(rbm)
    log4r::info(logger,paste("Starting the training of the rbm with ", getNumVisible(rbm)," visible and ", getNumHidden(rbm)," hidden units.",sep=""))
   
    ret <- makeStartEndPoints(getBatchSize(rbm),nrow(trainData))
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    stats <- getStats(rbm)
    if(is.null(stats) || length(stats) < 1){
      stats <- list("Errors"=c())
    }
    
    # Contains maxEpoch, actual epoch, numBatches, actual batch, numCD,
    # actual cd and if the cd loop is finished. Is given to the unit 
    # functions.
    runParams <- c("maxEpoch"=maxEpoch,"actualEpoch"=0,"numBatches"=numBatches,"actualBatch"=0,"numCD"=numCD,"actualCD"=0, "finishCD"=0) 
    output <- matrix(0,dim(trainData)[1],getNumHidden(rbm))
    
    for(i in c(1:maxEpoch)){
      runParams["actualEpoch"] <- i
      epochError = 0
      log4r::debug(logger,paste("Epoche: ",i,sep=""))
      
      
      for(j in 1:numBatches){
        runParams["finishCD"] <- 0
        runParams["actualBatch"] <- j
        
        weights <- getWeights(rbm)
        visibleBiases <- getVisibleBiases(rbm)
        hiddenBiases <- getHiddenBiases(rbm)
        
        # Get the batch
        start <- batchValues[[j]]+1
        end <- batchValues[[j+1]]
        data <- trainData[start:end,]
        if(is.null(dim(data))){
          data <- t(as.matrix(data))
        }
        
        setVisibleUnitStates(rbm) <- list(data)
        
        # Generate positive phase data list for the batch
        posPhaseData <- getPosPhaseData(rbm)
        posPhaseData <- list(data)
        
        # Run the contrastive divergence chain for numCD-times
        for(k in 1:numCD){
          runParams["actualCD"] <- k
          ret <- rbm@hiddenUnitFunction(rbm,getVisibleUnitStates(rbm),hiddenBiases, weights, runParams,...)
          setHiddenUnitStates(rbm) <- ret
          output[start:end,] <- ret[[1]]
          
          if(k == 1){
            # saving the positive phase data
            posPhaseData[[2]] <- ret
            setPosPhaseData(rbm) <- posPhaseData
          }
          setVisibleUnitStates(rbm) <- rbm@visibleUnitFunction(rbm,getHiddenUnitStates(rbm),visibleBiases, t(weights), runParams,...) 
        }
        
        runParams["finishCD"] <- 1
        # calculate the negative phase data
        setHiddenUnitStates(rbm) <- rbm@hiddenUnitFunction(rbm,getVisibleUnitStates(rbm),hiddenBiases,weights, runParams,...)
        
        error <- rbm@errorFunction(getPosPhaseData(rbm)[[1]], getVisibleUnitStates(rbm)[[1]])
        log4r::debug(logger,paste("Batch ",j," ",error[[1]]/nrow(data),"=", (error[[2]]),sep=""))
        epochError <- error[[2]]/nrow(data) + epochError;
        
        if(i>rbm@momentumSwitch){
          rbm@momentum<-rbm@finalMomentum;
        }					
        
        rbm <- rbm@updateFunction(rbm)
      }
      epochError <- epochError/numBatches
      stats[[1]] <- c(stats[[1]],epochError)
      log4r::info(logger,paste("Epoch ",i," error: ",epochError,sep=""))
    }
    
    setStats(rbm) <- stats
    setOutput(rbm) <- output
    return(rbm)
  }
)