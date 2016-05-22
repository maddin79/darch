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

#' Trains an \code{\linkS4class{RBM}} with contrastive divergence
#' 
#' The function trains a restricted Boltzmann machine (\code{\linkS4class{RBM}}) with 
#' the contrastive divergence method.
#' 
#' @details This function is build on the basis of the code from G. Hinton et. 
#'   al. (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html - last 
#'   visit 2016-04-30) for the pre training of deep belief nets. The original 
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
#'   
#'   
#' @param rbm A instance of the class \code{\linkS4class{RBM}}.
#' @param trainData The data matrix for the training
#' @param numEpochs The number of training iterations
#' @param numCD Number of contrastive divergence iterations
#' @param shuffleTrainData Whether to shuffle the training data prior to each
#'   epoch.
#' @param ... Additional parameters for the unit functions
#' @seealso \code{\linkS4class{RBM}}
#' @keywords internal
#' @include rbm.Class.R
setGeneric(
  name = "trainRBM",
  def = function(rbm, trainData, numEpochs=1, numCD=1, shuffleTrainData = T, ...)
  {standardGeneric("trainRBM")}
)

setMethod(
  f = "trainRBM",
  signature = c("RBM"),
  definition = function(rbm, trainData, numEpochs=1, numCD=1,
    shuffleTrainData = getParameter(".shuffleTrainData", net = rbm), ...)
  {
    numVisible <- getParameter(".numVisible", net = rbm)
    numHidden <- getParameter(".numHidden", net = rbm)
    unitFunction <- getParameter(".rbm.unitFunction", net = rbm)
    errorFunction <- getParameter(".rbm.errorFunction", net = rbm)
    errorFunctionName <- getErrorFunctionName(errorFunction)
    updateFunction <- getParameter(".rbm.updateFunction", net = rbm)
    
    # make start and end points for the batches
    if (rbm@epochs == 0)
    {
      futile.logger::flog.info(paste("Starting the training of the rbm with",
        "%s visible and %s hidden units."), numVisible, numHidden)
    }
    
    batchSize <- getParameter(".rbm.batchSize", net = rbm)
    numRows <- nrow(trainData)
    ret <- makeStartEndPoints(batchSize, numRows)
    batchValues <- ret[[1]]
    numBatches <- ret[[2]]
    
    stats <- rbm@stats
    if (is.null(stats) || length(stats) < 1)
    {
      stats <- list("errors" = c(), "times" = c())
    }
    
    # Contains numEpochs, current epoch, numBatches, current batch, numCD,
    # current cd and if the cd loop is finished. Is given to the unit 
    # functions.
    runParams <- c("numEpochs" = numEpochs, "currentEpoch" = rbm@epochs,
                   "numBatches" = numBatches, "currentBatch" = 0,
                   "numCD" = numCD, "currentCD" = 0, "finishCD" = 0) 
    output <- matrix(0, dim(trainData)[1], numHidden)
    
    for (i in c((rbm@epochs + 1):(rbm@epochs + numEpochs)))
    {
      timeStart <- Sys.time()
      runParams["currentEpoch"] <- i
      epochError = 0
      
      # shuffle data for each epoch
      if (shuffleTrainData)
      {
        randomSamples <- sample(1:numRows, size = numRows)
        trainData <- trainData[randomSamples,, drop = F]
      }
      
      for (j in 1:numBatches) {
        runParams["finishCD"] <- 0
        runParams["currentBatch"] <- j
        
        weights <- rbm@weights
        visibleBiases <- rbm@visibleBiases
        hiddenBiases <- rbm@hiddenBiases
        
        # Get the batch
        start <- batchValues[[j]] + 1
        end <- batchValues[[j + 1]]
        data <- trainData[start:end,, drop = F]
        
        rbm@visibleUnitStates <- list(data)
        
        # Generate positive phase data list for the batch
        posPhaseData <- list(data)
        
        # Run the contrastive divergence chain for numCD-times
        for (k in 1:numCD) {
          runParams["currentCD"] <- k
          
          if (k == 1)
          {
            ret <- unitFunction(rbm, rbm@visibleUnitStates[[1]],
                    hiddenBiases, weights, runParams, ...)
            
            # saving the positive phase data
            posPhaseData[[2]] <- ret
            rbm@posPhaseData <- posPhaseData
          }
          else
          {
            ret <- unitFunction(rbm, rbm@visibleUnitStates[[2]],
                    hiddenBiases, weights, runParams, ...)
          }
          
          rbm@hiddenUnitStates <- ret
          output[start:end,] <- ret[[1]]
          
          rbm@visibleUnitStates <-
            unitFunction(rbm, rbm@hiddenUnitStates[[2]],
              visibleBiases, t(weights), runParams, ...)
        }
        
        runParams["finishCD"] <- 1
        # calculate the negative phase data
        rbm@hiddenUnitStates <-
          unitFunction(rbm,rbm@visibleUnitStates[[1]],
            hiddenBiases,weights, runParams,...)
        
        error <-
          errorFunction(rbm@posPhaseData[[1]], rbm@visibleUnitStates[[1]])
        epochError <- epochError + (error[[2]] / nrow(data));
        
        rbm <- updateFunction(rbm)
      }
      
      epochError <- epochError / numBatches
      stats[["errors"]] <- c(stats[["errors"]], epochError)
      timeEnd <- Sys.time()
      stats[["times"]][i] <-
        as.double(difftime(Sys.time(), timeStart, units = "secs"))
      
      rbmId <- paste0("[RBM ", numVisible, "x", numHidden, "]")
      futile.logger::flog.info("%s Epoch %d %s error: %s", rbmId, i,
        errorFunctionName, epochError)
      futile.logger::flog.info("Finished epoch %d after %s", i,
                      format(difftime(timeEnd, timeStart)))
      rbm@epochs <- rbm@epochs + 1
    }
    
    rbm@stats <- stats
    rbm@output <- output
    rbm
  }
)
