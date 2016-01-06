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

#' Resilient backpropagation training for deep architectures.
#' 
#' The function trains a deep architecture with the resilient backpropagation
#' algorithm. It is able to use four different types of training (see details).
#' For details of the resilient backpropagation algorithm see the references.
#' 
#' @details
#' The code for the calculation of the weight change is a translation from the
#' MATLAB code from the Rprop Optimization Toolbox implemented by R. Calandra 
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
#. If advised of the possibility of such damage. 
#' 
#' @param darch The deep architecture to train
#' @param trainData The training data
#' @param targetData The expected output for the training data
#' @param method The method for the training. Default is "iRprop+"
#' @param rprop.decFact Decreasing factor for the training. Default is \code{0.5}.
#' @param rprop.incFact Increasing factor for the training Default is \code{1.2}.
#' @param rprop.initDelta Initialisation value for the update. Default is \code{0.0125}.
#' @param rprop.minDelta Lower bound for step size. Default is \code{0.000001}
#' @param rprop.maxDelta Upper bound for step size. Default is \code{50}
#' @param ... Further parameters.
#' 
#' @return \linkS4class{DArch} - The trained deep architecture
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
#' @seealso \code{\link{darch}}
#' @family fine-tuning functions
#' @export
rpropagation <- function(darch, trainData, targetData, method="iRprop+",
  rprop.decFact=0.5, rprop.incFact=1.2, rprop.initDelta=0.0125,
  rprop.minDelta=0.000001, rprop.maxDelta=50,
  matMult = getDarchParam("matMult", `%*%`, darch), ...)
{
  layers <- getLayers(darch)
  numLayers <- length(layers)
  delta <- list()
  gradients <- list()
  outputs <- list()
  derivatives <- list()
  stats <- getStats(darch)
  
  dropoutInput <- getDropoutInputLayer(darch)
  dropoutHidden <- getDropoutHiddenLayers(darch)
  
  # 1. Forwardpropagate
  if (dropoutInput > 0)
  {
    trainData <- applyDropoutMask(trainData, getDropoutMask(darch, 0))
  }
  
  data <- trainData
  numRows <- dim(data)[1]
  for(i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    weights <- getLayerWeights(darch,i)
    func <- getLayerFunction(darch,i)
    
    # apply dropout masks to weights and outputs
    if (dropoutHidden > 0 && (i < numLayers || darch@dropConnect))
    {
      # this is done to allow activation functions to avoid considering values
      # that are later going to be dropped
      weights <- applyDropoutMask(weights, getDropoutMask(darch, i))
      
      ret <- func(matMult(data, weights), darch=darch)
      
      if (!darch@dropConnect && i < numLayers)
      {
        ret[[1]] <- applyDropoutMask(ret[[1]], getDropoutMask(darch, i))
        ret[[2]] <- applyDropoutMask(ret[[2]], getDropoutMask(darch, i))
      }
    }
    else
    {
      ret <- func(matMult(data, weights), darch=darch)
    }
    
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
  }
  rm(data,numRows,func,ret)
  
  # 2. Calculate the Error on the network output
  output <- cbind(outputs[[numLayers-1]],rep(1,dim(outputs[[numLayers-1]])[1]))
  error <- (targetData - outputs[[numLayers]])
  delta[[numLayers]] <- error * derivatives[[numLayers]]
  gradients[[numLayers]] <- t(matMult(-t(delta[[numLayers]]), output))
  
  errOut <- getErrorFunction(darch)(targetData, outputs[[numLayers]])
  #flog.debug(paste("Pre-Batch",errOut[[1]],errOut[[2]]))
  newE <- errOut[[2]]
  oldE <- if (is.null(stats[["oldE"]])) Inf else stats[["oldE"]]
  stats[["oldE"]] <- newE
  
  # 4. Backpropagate the error
  for(i in (numLayers-1):1){
    
    weights <- getLayerWeights(darch,i+1)
    weights <- weights[1:(nrow(weights) - 1),, drop = F]
    
    if (i > 1){
      output <- cbind(outputs[[i-1]][],rep(1,dim(outputs[[i-1]])[1]))
    }else{
      output <- cbind(trainData,rep(1,dim(trainData)[1]))
    }
    
    error <-  matMult(delta[[i+1]], t(weights))
    delta[[i]] <- error * derivatives[[i]]
    gradients[[i]] <- -t(matMult(t(delta[[i]]), output))
  }
  rm(delta,error,output)
  
  
  # 5.  Update the weights
  for(i in 1:numLayers){
    weights <- getLayerWeights(darch,i)
    
    if (is.null(layers[[i]][["rprop.init"]]))
    {
      setLayerField(darch, i, "rprop.init") <- T
      setLayerField(darch, i, "rprop.gradients") <-
        matrix(0,nrow(gradients[[i]]),ncol(gradients[[i]])) # old gradients
      setLayerField(darch, i, "rprop.delta") <-
        matrix(rprop.initDelta,nrow(weights),ncol(weights)) # old deltas
      # momentum terms
      setLayerField(darch, i, "rprop.inc") <-
        matrix(0, nrow(weights), ncol(weights))
    }
    
    oldGradient <- getLayerField(darch, i, "rprop.gradients")
    oldDelta <-  getLayerField(darch, i, "rprop.delta")
    oldDeltaW <- getLayerField(darch, i, "rprop.inc")
    
    gg <- gradients[[i]] * oldGradient
    maxD <- matrix(rprop.maxDelta, nrow(oldDelta), ncol(oldDelta))
    minD <- matrix(rprop.minDelta, nrow(oldDelta), ncol(oldDelta))
    delta <- (pmin(oldDelta * rprop.incFact, maxD) * (gg > 0) +
      pmax(oldDelta * rprop.decFact, minD) * (gg < 0) +
      oldDelta * (gg == 0))
    
    if (method == "Rprop+"){
      deltaW <- -sign(gradients[[i]]) * delta * (gg >= 0) - oldDeltaW * (gg<0)
      gradients[[i]] <- gradients[[i]] * (gg >= 0)
    }
    
    if (method == "Rprop-"){
      deltaW <- -sign(gradients[[i]]) * delta
    }
    
    if (method == "iRprop+"){
      deltaW <- (-sign(gradients[[i]]) * delta * (gg>=0) - oldDeltaW * (gg<0) *
        (newE > oldE))
      gradients[[i]] <- gradients[[i]] * (gg >= 0)
    }
    
    if (method == "iRprop-"){
      gradients[[i]] <- gradients[[i]] * (gg >= 0)
      deltaW <- -sign(gradients[[i]]) * delta
    }
    
    inc <- deltaW + (getMomentum(darch) * oldDeltaW)
    
    darch <- getWeightUpdateFunction(darch, i)(darch, i, inc[1:(nrow(inc)-1),],
      inc[nrow(inc),])
    
    setLayerField(darch, i, "rprop.gradients") <- gradients[[i]]
    setLayerField(darch, i, "rprop.delta") <- delta
    setLayerField(darch, i, "rprop.inc") <- inc
  }
  
  setStats(darch) <- stats
  return(darch)
}
