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

#' Trains a \code{\link{RBM}} with contrastive divergence
#' 
#' The function trains a restricted Boltzmann machine (\code{\link{RBM}}) with 
#' the contrastive divergence method.
#' 
#' @details This function is build on the basis of the code from G. Hinton et. 
#'   al. (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html - last 
#'   visit 06.06.2013) for the pre training of deep belief nets. The original 
#'   code is located in the files 'rbm.m' and 'rbmhidlinear.m'. It iterates in 
#'   every epoche over the batches and calculates the updates for the weights. 
#'   If it is the first CD iteration or the CD iterations are finished, the 
#'   hidden units are calculated with the real value activations of the visible 
#'   units, otherwise with the binary activations. To tell the unit functions 
#'   the actual state of the training, the function generates a array with the 
#'   following running parameters and passes them to the units: Number of 
#'   epochs: "numEpochs", current epochs: "currentEpoch", Number of batches: 
#'   "numBatches", current batch: "currentBatch", Maximal CD iterations: 
#'   "numCD", current CD iteration: "currentCD", CD is finished: "finishCD". 
#'   (see source code from \code{\link{sigmUnitFuncSwitch}} for an example).
#'   
#'   
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param trainData The data matrix for the training
#' @param numEpochs The number of training iterations
#' @param numCD Number of contrastive divergence iterations
#' @param ... Additional parameters for the unit functions
#' @seealso \code{\linkS4class{RBM}}
#'   
#' @include rbm.R
#'   
#' @export
setGeneric(
  name="trainRBM",
  def=function(rbm,trainData,numEpochs=1,numCD=1,...){standardGeneric("trainRBM")}
)

#' Trains a \code{\link{RBM}} with contrastive divergence
#' 
#' @inheritParams trainRBM
#' @seealso \link{trainRBM}
#' @export
setMethod(
  f="trainRBM",
  signature=c("RBM"),
  definition=function(rbm, trainData, numEpochs=1, numCD=1, ...){
    # make start and end points for the batches
    flog.info(paste0("Starting the training of the rbm with ", getNumVisible(rbm)," visible and ", getNumHidden(rbm)," hidden units."))
    
    ret <- makeStartEndPoints(getBatchSize(rbm),nrow(trainData))
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    stats <- getStats(rbm)
    if (is.null(stats) || length(stats) < 1){
      stats <- list("errors"=c(),
                    "times"=c())
    }
    
    # Contains numEpochs, current epoch, numBatches, current batch, numCD,
    # current cd and if the cd loop is finished. Is given to the unit 
    # functions.
    runParams <- c("numEpochs"=numEpochs,"currentEpoch"=getEpochs(rbm),"numBatches"=numBatches,
                   "currentBatch"=0,"numCD"=numCD,"currentCD"=0, "finishCD"=0) 
    output <- matrix(0,dim(trainData)[1],getNumHidden(rbm))
    
    for(i in c((getEpochs(rbm) + 1) : (getEpochs(rbm) + numEpochs)))
    {
      timeEpochStart <- Sys.time()
      runParams["currentEpoch"] <- i
      epochError = 0
      flog.debug(paste("Epoch:", i))
      
      for(j in 1:numBatches){
        runParams["finishCD"] <- 0
        runParams["currentBatch"] <- j
        
        weights <- getWeights(rbm)
        visibleBiases <- getVisibleBiases(rbm)
        hiddenBiases <- getHiddenBiases(rbm)
        
        # Get the batch
        start <- batchValues[[j]]+1
        end <- batchValues[[j+1]]
        data <- trainData[start:end,, drop = F]
        
        setVisibleUnitStates(rbm) <- list(data)
        
        # Generate positive phase data list for the batch
        # TODO what?
        #posPhaseData <- getPosPhaseData(rbm)
        posPhaseData <- list(data)
        
        # Run the contrastive divergence chain for numCD-times
        for(k in 1:numCD){
          runParams["currentCD"] <- k
          ret <- rbm@hiddenUnitFunction(rbm, getVisibleUnitStates(rbm),
                                        hiddenBiases, weights, runParams, ...)
          setHiddenUnitStates(rbm) <- ret
          output[start:end,] <- ret[[1]]
          
          if (k == 1){
            # saving the positive phase data
            posPhaseData[[2]] <- ret
            setPosPhaseData(rbm) <- posPhaseData
          }
          setVisibleUnitStates(rbm) <-
            rbm@visibleUnitFunction(rbm, getHiddenUnitStates(rbm),
                                    visibleBiases, t(weights), runParams, ...)
        }
        
        runParams["finishCD"] <- 1
        # calculate the negative phase data
        setHiddenUnitStates(rbm) <- rbm@hiddenUnitFunction(rbm,getVisibleUnitStates(rbm),hiddenBiases,weights, runParams,...)
        
        error <- rbm@errorFunction(getPosPhaseData(rbm)[[1]], getVisibleUnitStates(rbm)[[1]])
        #flog.info(paste("Batch ",j," ",error[[2]]/nrow(data),"=", (error[[2]]),sep=""))
        epochError <- error[[2]]/nrow(data) + epochError;
        
        rbm <- rbm@updateFunction(rbm)
      }
      epochError <- epochError/numBatches
      stats[["errors"]] <- c(stats[["errors"]],epochError)
      stats[["times"]][i] <- as.double(Sys.time() - timeEpochStart, "secs")
      flog.info(paste("Epoch ",i," error: ",epochError,sep=""))
      rbm <- incrementEpochs(rbm)
    }
    
    setStats(rbm) <- stats
    setOutput(rbm) <- output
    return(rbm)
  }
)
