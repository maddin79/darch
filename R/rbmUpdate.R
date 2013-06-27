#' BEARBEITEN
#' 
#' BEARBEITEN 
#' 
#' @param rbm BEARBEITEN
#' @return BEARBEITEN
#' 
#' @seealso \code{\link{RBM}}
#' 
#' @docType methods
#' @rdname rbmUpdate
#' @export
rbmUpdate <- function(rbm){
  # get parameters
  momentum <- getMomentum(rbm)
  weightInc <- getWeightInc(rbm)
  weights <- getWeights(rbm)
  visibleBiasInc <- getVisibleBiasesInc(rbm)
  visibleBiases <- getVisibleBiases(rbm)
  hiddenBiasInc <- getHiddenBiasesInc(rbm)
  hiddenBiases <- getHiddenBiases(rbm)
  data <- getPosPhaseData(rbm)[[1]]
  # If the batch size is 1, the data must be converted to a matrix
  if(is.null(dim(data))){
    data <- t(as.matrix(data))
  }
  posHiddenProbs <-  getPosPhaseData(rbm)[[2]][[1]]
  negativData <- getVisibleUnitStates(rbm)[[1]]
  negHiddenProbs <- getHiddenUnitStates(rbm)[[1]]
  learnRateWeights <- getLearnRateWeights(rbm)
  weightCost <- getWeightCost(rbm)
  learnRateBiasVisible <- getLearnRateBiasVisible(rbm)
  learnRateBiasHidden <- getLearnRateBiasHidden(rbm)
  batchSize <- getBatchSize(rbm)
  
  # positive phase
  posProducts <- t(data) %*% posHiddenProbs
  posHiddienAct <- apply(posHiddenProbs,2,sum)
  posVisibleAct <- apply(data,2,sum)
  
  # negative phase
  negProducts  <- t(negativData) %*% negHiddenProbs;
  negHiddienAct <- apply(negHiddenProbs,2,sum);
  negVisibleAct <- apply(negativData,2,sum);
  
  # update
  weightInc <- momentum*weightInc + learnRateWeights * ((posProducts - negProducts)/batchSize - weightCost*weights)
  visibleBiasInc <- momentum*visibleBiasInc + (learnRateBiasVisible/batchSize) * (posVisibleAct-negVisibleAct)
  hiddenBiasInc <- momentum*hiddenBiasInc + (learnRateBiasHidden/batchSize) * (posHiddienAct-negHiddienAct)
  
  setWeights(rbm) <- weights + weightInc
  setVisibleBiases(rbm) <- visibleBiases + visibleBiasInc
  setHiddenBiases(rbm) <- hiddenBiases + hiddenBiasInc
  setWeightInc(rbm) <- weightInc
  setVisibleBiasesInc(rbm) <- visibleBiasInc
  setHiddenBiasesInc(rbm) <- hiddenBiasInc
  
  return(rbm)
}