#' Resilient-Backpropgation training for deep architectures.
#' 
#' The function traines a deep architecture with the resilient backpropagation
#' algorithm. It is able to use four different types of training (see details).
#' For details of the resilient backpropagation algorith see the references.
#' 
#' @details
#' The code for the calculation of the weight change is a translation from the
#' matlab code from the Rprop Optimization Toolbox implemented by R. Calandra 
#' (see References).
#' 
#' Copyright (c) 2011, Roberto Calandra. All rights reserved. Redistribution and
#'  use in source and binary forms, with or without modification, are permitted 
#'  provided that the following conditions are met:
#'  1. Redistributions of source code must retain the above copyright notice, 
#'  this list of conditions and the following disclaimer.
#'  2. Redistributions in binary form must reproduce the above copyright notice, 
#'  this list of conditions and the following disclaimer in the documentation 
#'  and/or other materials provided with the distribution.
#'  3. The names of its contributors may be used to endorse or promote products 
#'  derived from this software without specific prior written permission. 
#'  4. If used in any scientific publications, the publication has to refer 
#'  specifically to the work published on this webpage. 
#'  
#'  This software is provided by us "as is" and any express or implied 
#'  warranties, including, but not limited to, the implied warranties of 
#'  merchantability and fitness for particular purpose are disclaimed. In no 
#'  event shall the copyright holders or any contributor be liable for any 
#'  direct, indirect, incidental, special, exemplary, or consequential damages 
#'  however caused and on any theory of liability whether in contract, strict 
#'  liability or tort arising in any way out of the use of this software, even 
#'  if advised of the possibility of such damage. 
#' 
#' @param darch The deep architecture to train
#' @param trainData The training data
#' @param targetData The expected output for the training data
#' @param epoch The number of training iterations
#' @param method The method for the training. Default is "iRprop+"
#' @param decFact Decreasing factor for the training. Default is \code{0.5}.
#' @param incFact Increasing factor for the training Default is \code{1.2}.
#' @param weightDecay Weight decay for the training. Default is \code{0}
#' @param initDelta Initialisation value for the update. Default is \code{0.0125}.
#' @param minDelta Lower bound for step size. Default is \code{0.000001}
#' @param maxDelta Upper bound for step size. Default is \code{50}
#' 
#' @return \code{darch} - The trained deep architecture
#' 
#' @details
#' The possible training methods (parameter \code{method}) are the following 
#' (see References for details):
#' \tabular{ll}{
#' Rprop+: \tab Rprop with Weight-Backtracking\cr
#' Rprop-: \tab Rprop without Weight-Backtracking\cr
#' iRprop+: \tab Improved Rprop with Weight-Backtracking\cr
#' iRprop-: \tab Improved Rprop with Weight-Backtracking\cr
#' } 
#'
#'
#' @usage rpropagation(darch,trainData,targetData,epoch,method="iRprop+", 
#' decFact=0.5,incFact=1.2,weightDecay=0,
#' initDelta=0.0125,minDelta=0.000001,maxDelta=50)
#' 
#' @references
#' M. Riedmiller, H. Braun. A direct adaptive method for faster backpropagation
#' learning: The RPROP algorithm. In Proceedings of the IEEE International
#' Conference on Neural Networks, pp 586-591. IEEE Press, 1993.
#' 
#' C. Igel , M. Huesken.  Improving the Rprop Learning Algorithm, Proceedings of 
#' the Second International Symposium on Neural Computation, NC 2000, ICSC 
#' Academic Press, Canada/Switzerland, pp. 115-121., 2000.
#' 
#' Kohavi, R., A Study of Cross-Validation and Bootstrap for Accuracy Estimation
#'  and Model Selection, Proceedings of the 14th Int. Joint Conference on 
#' Artificial Intelligence 2, S. 1137-1143, Morgan Kaufmann, Morgan Kaufmann 
#' Publishers Inc., San Francisco, CA, USA, 1995.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname rpropagation
#' @include darch.R
#' @export
rpropagation <- function(darch,trainData,targetData,epoch,method="iRprop+",
                         decFact=0.5,incFact=1.2,weightDecay=0,initDelta=0.0125,
                         minDelta=0.000001,maxDelta=50){
  numLayers <- length(getLayers(darch))
  delta <- list()
  gradients <- list()
  outputs <- list()
  derivatives <- list()
  stats <- getStats(darch)
  
  # If the batch size is 1, the data must be converted to a matrix
  if(is.null(dim(trainData))){
    trainData <- t(as.matrix(trainData))
  }
  
  # 1. Forwardpropagate
  data <- trainData
  numRows <- dim(data)[1]
  for(i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    func <- getLayerFunction(darch,i)
    ret <- func(data,getLayerWeights(darch,i))
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
  }
  rm(data,numRows,func,ret)
  
  # Old Forwardpropagate
  #darch <- getExecuteFunction(darch)(darch,trainData)
  #outputs <- getExecOutputs(darch)
  
  # 2. Calculate the Error on the network output
  output <- cbind(outputs[[numLayers-1]][],rep(1,dim(outputs[[numLayers-1]])[1]))
  error <- (targetData - outputs[[numLayers]][])
  delta[[numLayers]] <- error * derivatives[[numLayers]]
  gradients[[numLayers]] <- t(-t(delta[[numLayers]])%*%output)
  
  errOut <- getErrorFunction(darch)(targetData,outputs[[numLayers]][])
  flog.debug(paste("Pre-Batch",errOut[[1]],errOut[[2]]))
  newE <- errOut[[2]]
  
  # Use only entrys bigger than index 3 in the stats-list
  if(length(stats) < 5){
    stats[[5]] <- c(newE)
    oldE <- Inf
  }else{
    stats[[5]] <- c(stats[[5]],newE)
    oldE <- stats[[5]][length(stats[[5]])-1]
  }
  
  # 4. Backpropagate the error
  for(i in (numLayers-1):1){
    
    weights <- getLayerWeights(darch,i+1)
    weights <- weights[1:(nrow(weights)-1),]
    
    if(i > 1){
      output <- cbind(outputs[[i-1]][],rep(1,dim(outputs[[i-1]])[1]))
    }else{
      output <- cbind(trainData,rep(1,dim(trainData)[1]))
    }
    
    error <-  delta[[i+1]] %*% t(weights)
    delta[[i]] <- error * derivatives[[i]]
    gradients[[i]] <- -t(t(delta[[i]])%*%output)
  }
  rm(delta,error,output)
  
  
  # 5.  Update the weights
  for(i in 1:numLayers){
    weights <- getLayerWeights(darch,i)
    
    gradients[[i]] <- gradients[[i]] + weightDecay*weights
    
    if(length(getLayer(darch,i)) < 3){
      setLayerField(darch,i,3) <- matrix(0,nrow(gradients[[i]]),ncol(gradients[[i]])) # old gradients
      setLayerField(darch,i,4) <- matrix(initDelta,nrow(weights),ncol(weights)) # old deltas
      setLayerField(darch,i,5) <- matrix(0,nrow(weights),ncol(weights)) # old deltaWs
    }
    
    oldGradient <- getLayerField(darch,i,3)
    oldDelta <-  getLayerField(darch,i,4)
    oldDeltaW <- getLayerField(darch,i,5)
    
    gg <- gradients[[i]]*oldGradient
    maxD <- matrix(maxDelta,nrow(oldDelta),ncol(oldDelta))
    minD <- matrix(minDelta,nrow(oldDelta),ncol(oldDelta))
    delta <- pmin(oldDelta*incFact,maxD)*(gg>0) + 
      pmax(oldDelta*decFact,minD)*(gg<0) + 
      oldDelta*(gg==0)
    
    if(method == "Rprop+"){
      deltaW <- -sign(gradients[[i]])*delta*(gg>=0) - oldDeltaW*(gg<0)
      gradients[[i]] <- gradients[[i]]*(gg>=0)
    }
    
    if(method == "Rprop-"){
      deltaW <- -sign(gradients[[i]])*delta
    }
    
    if(method == "iRprop+"){
      deltaW <- -sign(gradients[[i]])*delta*(gg>=0) - oldDeltaW*(gg<0)*(newE>oldE)
      gradients[[i]] <- gradients[[i]]*(gg>=0)
    }
    
    if(method == "iRprop-"){
      gradients[[i]] <- gradients[[i]]*(gg>=0)
      deltaW <- -sign(gradients[[i]])*delta
    }
    
    if(epoch > getMomentumSwitch(darch)){
      setMomentum(darch) <- getFinalMomentum(darch) 
    }
    
    biases <- t(as.matrix(weights[nrow(weights),]))
    weights <- as.matrix(weights[1:(nrow(weights)-1),])
    
    weights <- weights + deltaW[1:(nrow(deltaW)-1),] + (getMomentum(darch) * oldDeltaW[1:(nrow(deltaW)-1),])
    biases <- biases + deltaW[nrow(deltaW),]
    setLayerWeights(darch,i) <- rbind(weights,biases)
    
    setLayerField(darch,i,3) <- gradients[[i]]
    setLayerField(darch,i,4) <- delta
    setLayerField(darch,i,5) <- deltaW
  }
  
  setStats(darch) <- stats
  return(darch)
}