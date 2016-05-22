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

#' Resilient backpropagation training for deep architectures.
#' 
#' The function trains a deep architecture with the resilient backpropagation
#' algorithm. It is able to use four different types of training (see details).
#' For details of the resilient backpropagation algorithm see the references.
#' 
#' @details
#' 
#' RPROP supports dropout and uses the weight update function as
#' defined via the \code{darch.weightUpdateFunction} parameter of
#' \code{\link{darch}}.
#' 
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
#' @param rprop.method The method for the training. Default is "iRprop+"
#' @param rprop.decFact Decreasing factor for the training. Default is \code{0.6}.
#' @param rprop.incFact Increasing factor for the training Default is \code{1.2}.
#' @param rprop.initDelta Initialisation value for the update. Default is \code{0.0125}.
#' @param rprop.minDelta Lower bound for step size. Default is \code{0.000001}
#' @param rprop.maxDelta Upper bound for step size. Default is \code{50}
#' @param nesterovMomentum See \code{darch.nesterovMomentum} parameter of
#'   \code{\link{darch}}.
#' @param dropout See \code{darch.dropout} parameter of
#'   \code{\link{darch}}.
#' @param dropConnect See \code{darch.dropout.dropConnect} parameter of
#'   \code{\link{darch}}.
#' @param errorFunction See \code{darch.errorFunction} parameter of
#'   \code{\link{darch}}.
#' @param matMult Matrix multiplication function, internal parameter.
#' @param debugMode Whether debug mode is enabled, internal parameter.
#' @param ... Further parameters.
#' 
#' @return \linkS4class{DArch} - The trained deep architecture
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, darch.fineTuneFunction = "rpropagation",
#'  preProc.params = list(method = c("center", "scale")),
#'  darch.unitFunction = c("softplusUnit", "softmaxUnit"),
#'  rprop.method = "iRprop+", rprop.decFact = .5, rprop.incFact = 1.2,
#'  rprop.initDelta = 1/100, rprop.minDelta = 1/1000000, rprop.maxDelta = 50)
#' }
#' @details
#' The possible training methods (parameter \code{rprop.method}) are the
#' following  (see References for details):
#' \tabular{ll}{
#' Rprop+: \tab Rprop with Weight-Backtracking\cr
#' Rprop-: \tab Rprop without Weight-Backtracking\cr
#' iRprop+: \tab Improved Rprop with Weight-Backtracking\cr
#' iRprop-: \tab Improved Rprop without Weight-Backtracking\cr
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
rpropagation <- function(darch, trainData, targetData,
  rprop.method=getParameter(".rprop.method"),
  rprop.decFact=getParameter(".rprop.decFact"),
  rprop.incFact=getParameter(".rprop.incFact"),
  rprop.initDelta=getParameter(".rprop.initDelta"),
  rprop.minDelta=getParameter(".rprop.minDelta"),
  rprop.maxDelta=getParameter(".rprop.maxDelta"),
  nesterovMomentum = getParameter(".darch.nesterovMomentum"),
  dropout = getParameter(".darch.dropout"),
  dropConnect = getParameter(".darch.dropout.dropConnect"),
  errorFunction = getParameter(".darch.errorFunction"),
  matMult = getParameter(".matMult"),
  debugMode = getParameter(".debug", F, darch), ...)
{
  # Print fine-tuning configuration on first run
  # TODO more details on the configuration
  if (!getParameter(".rprop.init", F, darch))
  {
    darch@parameters[[".rprop.init"]] <- T
  }
  
  layers <- darch@layers
  numLayers <- length(layers)
  delta <- list()
  gradients <- list()
  outputs <- list()
  derivatives <- list()
  momentum <- getMomentum(darch)
  
  dropoutInput <- dropout[1]
  
  # 1. Forwardpropagate
  if (dropoutInput > 0)
  {
    trainData <- applyDropoutMaskCpp(trainData, getDropoutMask(darch, 0))
  }
  
  data <- trainData
  numRows <- nrow(data)
  numRowsWeights <- vector(mode = "numeric", length = numLayers)
  weights <- vector(mode = "list", length = numLayers)
  for(i in 1:numLayers){
    data <- cbind(data,rep(1,numRows))
    numRowsWeights[i] <- nrow(layers[[i]][["weights"]])
    numColsWeights <- ncol(layers[[i]][["weights"]])
    
    # Initialize rprop layer variables
    if (is.null(layers[[i]][["rprop.init"]]))
    {
      layers[[i]][["rprop.init"]] <- T
      layers[[i]][["rprop.gradients"]] <-
        matrix(0, numRowsWeights[i], numColsWeights) # old gradients
      layers[[i]][["rprop.delta"]] <-
        matrix(rprop.initDelta, numRowsWeights[i], numColsWeights) # old deltas
      layers[[i]][["rprop.inc"]] <-
        matrix(0, numRowsWeights[i], numColsWeights) # momentum terms
    }
    
    if (nesterovMomentum)
    {
      weights[[i]] <-
        layers[[i]][["weights"]] + layers[[i]][["rprop.inc"]] * momentum
    }
    else
    {
      weights[[i]] <- layers[[i]][["weights"]]
    }
    
    func <- layers[[i]][["unitFunction"]]
    
    # apply dropout masks to weights and / or outputs
    # TODO extract method
    if ((i < numLayers || dropConnect) && dropout[i + 1] > 0)
    {
      dropoutMask <- getDropoutMask(darch, i)
      
      if (dropConnect)
      {
        weights[[i]] <- applyDropoutMaskCpp(weights[[i]], dropoutMask)
        
        ret <- func(matMult(data, weights[[i]]), net = darch,
                    dropoutMask = dropoutMask)
      }
      else
      {
        ret <- func(matMult(data, weights[[i]]), net = darch,
                    dropoutMask = dropoutMask)
        
        ret[[1]] <- applyDropoutMaskCpp(ret[[1]], dropoutMask)
        ret[[2]] <- applyDropoutMaskCpp(ret[[2]], dropoutMask)
      }
    }
    else
    {
      ret <- func(matMult(data, weights[[i]]), net = darch)
    }
    
    outputs[[i]] <- ret[[1]]
    data <- ret[[1]]
    derivatives[[i]] <- ret[[2]]
    
    if (debugMode)
    {
      futile.logger::flog.debug("Layer %s: Activation standard deviation: %s",
                                i, sd(data))
      futile.logger::flog.debug("Layer %s: Derivatives standard deviation: %s",
                                i, sd(derivatives[[i]]))
    }
  }
  #rm(data,numRows,func,ret)
  
  # 2. Calculate the Error on the network output
  output <- cbind(outputs[[numLayers-1]],rep(1,dim(outputs[[numLayers-1]])[1]))
  error <- (targetData - outputs[[numLayers]])
  delta[[numLayers]] <- error * derivatives[[numLayers]]
  gradients[[numLayers]] <- t(matMult(-t(delta[[numLayers]]), output))
  
  errOut <- errorFunction(targetData, outputs[[numLayers]])
  #flog.debug(paste("Pre-Batch",errOut[[1]],errOut[[2]]))
  newE <- errOut[[2]]
  oldE <- if (is.null(layers[[1]][["rprop.oldE"]])) .Machine$double.xmax
    else layers[[1]][["rprop.oldE"]]
  layers[[1]][["rprop.oldE"]] <- newE
  
  # 4. Backpropagate the error
  for (i in (numLayers - 1):1)
  {
    weights[[i+1]] <- weights[[i+1]][1:(nrow(weights[[i+1]]) - 1),, drop = F]
    
    
    output <- (if (i > 1) cbind(outputs[[i-1]],rep(1,dim(outputs[[i-1]])[1]))
               else cbind(trainData,rep(1,dim(trainData)[1])))
    
    error <-  matMult(delta[[i + 1]], t(weights[[i + 1]]))
    delta[[i]] <- error * derivatives[[i]]
    gradients[[i]] <- -t(matMult(t(delta[[i]]), output))
  }
  #rm(delta,error,output)
  
  
  # 5.  Update the weights
  for (i in 1:numLayers)
  {    
    oldGradient <- layers[[i]][["rprop.gradients"]]
    delta <-  layers[[i]][["rprop.delta"]]
    deltaW <- layers[[i]][["rprop.inc"]]
    inc <- momentum * deltaW
    
    gg <- gradients[[i]] * oldGradient
    #maxD <- matrix(rprop.maxDelta, nrow(oldDelta), ncol(oldDelta))
    #minD <- matrix(rprop.minDelta, nrow(oldDelta), ncol(oldDelta))
    #delta <- (pmin(oldDelta * rprop.incFact, maxD) * (gg > 0) +
    #  pmax(oldDelta * rprop.decFact, minD) * (gg < 0) +
    #  oldDelta * (gg == 0))
    #  
    rpropDeltaCpp(gg, delta, rprop.incFact, rprop.decFact, rprop.minDelta,
      rprop.maxDelta)
    
    if (rprop.method == "Rprop+")
    {
      deltaW <- -sign(gradients[[i]]) * delta * (gg >= 0) - deltaW * (gg < 0)
      
      rpropGradientsCpp(gg, gradients[[i]])
    }
    
    else if (rprop.method == "Rprop-")
    {
      deltaW <- -sign(gradients[[i]]) * delta
    }
    
    else if (rprop.method == "iRprop+")
    {
      rpropDeltaWiRpropPlus(gg, deltaW, gradients[[i]], delta, newE, oldE)
      
      rpropGradientsCpp(gg, gradients[[i]])
    }
    
    else if (rprop.method == "iRprop-")
    {
      deltaW <- -sign(gradients[[i]]) * delta
      rpropGradientsCpp(gg, gradients[[i]]) # TODO order?
    }
    
    else
    {
      futile.logger::flog.error("Unknown RPROP method '%s'", rprop.method)
    }
    
    inc <- inc + deltaW
    
    if (debugMode)
    {
      futile.logger::flog.debug("Layer %s: Weight change ratio: %s",
        i, norm(inc) / norm(layers[[i]][["weights"]]))
    }
    
    layers[[i]][["weights"]] <-
      (darch@layers[[i]][["weightUpdateFunction"]](darch, i,
      inc[1:(nrow(inc) - 1),, drop = F], inc[nrow(inc),]))
    
    layers[[i]][["rprop.gradients"]] <- gradients[[i]]
    #layers[[i]][["rprop.delta"]] <- delta
    layers[[i]][["rprop.inc"]] <- inc
  }
  
  darch@layers <- layers
  darch
}

printDarchParams.rpropagation <- function(darch, lf = futile.logger::flog.info)
{
  lf("[RPROP] Using rpropagation for fine-tuning")
  printParams(c("rprop.method", "rprop.decFact",
              "rprop.incFact", "rprop.initDelta", "rprop.minDelta",
              "rprop.maxDelta"), "RPROP")
  lf("[RPROP] See ?rpropagation for documentation")
}