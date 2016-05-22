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

#' Conjugate gradient for a autoencoder network
#' 
#' This function trains a \code{\linkS4class{DArch}} autoencoder network with the 
#' conjugate gradient method. 
#' 
#' @details
#' This function is built on the basis of the code from G. Hinton et. al.
#' (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html - last visit 
#' 2016-04-30) for the fine tuning of deep belief nets. The original code is 
#' located in the files 'backpropclassify.m', 'CG_MNIST.m' and 
#' 'CG_CLASSIFY_INIT.m'. 
#' It implements the fine tuning for a classification net with backpropagation
#' using a direct translation of the \code{\link{minimize}} function from C. 
#' Rassmussen (available at http://www.gatsby.ucl.ac.uk/~edward/code/minimize/ 
#' - last visit 2016-04-30) to R.
#' 
#' \code{minimizeAutoencoder} supports dropout but does not use the weight
#' update function as defined via the \code{darch.weightUpdateFunction}
#' parameter of \code{\link{darch}}, so that weight decay, momentum etc. are not
#' supported.
#' 
#' @inheritParams minimizeClassifier
#' @inheritParams backpropagation
#' @return The trained \code{\linkS4class{DArch}} object.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris, c(6,10,2,10,6), darch.isClass = F,
#'  preProc.params = list(method=c("center", "scale")),
#'  darch.numEpochs = 20, darch.batchSize = 6, darch.unitFunction = tanhUnit
#'  darch.fineTuneFunction = "minimizeAutoencoder")
#' }
#' @seealso \code{\link{darch}}, \code{\link{fineTuneDArch}}
#' @family fine-tuning functions
#' @include darch.Class.R
#' @export
minimizeAutoencoder <- function(darch, trainData, targetData,
  cg.length = getParameter(".cg.length"),
  dropout = getParameter(".darch.dropout"),
  dropConnect = getParameter(".darch.dropout.dropConnect"),
  matMult = getParameter(".matMult"),
  debugMode = getParameter(".debug"), ...)
{
  # Function for gradients ###############################
  fr <- function(par, dims, data, target=NULL, epochSwitch=NULL)
  {  
    startPos <- 1
    endPos <- 0
    numRows <- dim(data)[1]
    length <- length(dims)
    outputs <- list()
    gradients <- list()
    derivatives <- list()
    weights <- list()
    
    d <- data	
    # Calculating the outputs
    for (i in 1:length) {
      d <- cbind(d,rep(1,numRows))
      endPos <- endPos + dims[[i]][1]*dims[[i]][2]
      weights[[i]] <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
      startPos <- endPos + 1
      
      if (i < length && dropout[i + 1] > 0 && !dropConnect)
      {
        dropoutMask <- getDropoutMask(darch, i)
        
        ret <- darch@layers[[i]][["unitFunction"]](matMult(d, weights[[i]]),
          net = darch, dropoutMask = dropoutMask)
        
        outputs[[i]] <- applyDropoutMaskCpp(ret[[1]], dropoutMask)
        derivatives[[i]] <- applyDropoutMaskCpp(ret[[2]], dropoutMask)
      }
      else
      {
        ret <- darch@layers[[i]][["unitFunction"]](matMult(d, weights[[i]]),
                                                   net = darch)
        outputs[[i]] <- ret[[1]]
        derivatives[[i]] <- ret[[2]]
      }
      
      if (debugMode)
      {
        futile.logger::flog.debug("Layer %s: Activation standard deviation: %s",
                                  i, sd(outputs[[i]]))
        futile.logger::flog.debug(
          "Layer %s: Derivatives standard deviation: %s", i,
          sd(derivatives[[i]]))
      }
      
      d <- ret[[1]]
    }
    
    output <- outputs[[length]]
    x = data*log(output) + (1 - data) * log(1 - output)
    f = -1 / nrow(data) * sum(x)
    
    ix <- 1 / nrow(data) * (output - data)
    out <- cbind(outputs[[i - 1]],rep(1,nrow(outputs[[i - 1]])))
    gradients[[length]] <- matMult(t(out), ix)
    
    for(i in (length - 1):1){
      derivatives[[i]] <- cbind(derivatives[[i]],rep(1,nrow(derivatives[[i]])))
      ix <- (matMult(ix, t(weights[[i + 1]])))* derivatives[[i]] # outputs[[i]]*(1-outputs[[i]])
      ix <- ix[,1:(dim(ix)[2] - 1)]
      if (i > 1){
        out <- cbind(outputs[[i - 1]],rep(1,nrow(outputs[[i - 1]])))
        gradients[[i]] <- matMult(t(out), ix)
      }else{
        d <- cbind(data,rep(1,numRows))
        gradients[[i]] <- matMult(t(d), ix)
      }
    }
    
    ret <- c(f)
    for (i in 1:length(gradients)) {
      ret <- c(ret, c(gradients[[i]]))
    }
    return(ret)
  }
  # End function for gradients ###############################
  
  # Initialize CG parameters on first run
  # TODO remove?
  if (!getParameter(".cg.init", F, darch))
  {
    darch@parameters[[".cg.init"]] <- T
  }
  
  dropout <- c(dropout, 0) # TODO fix
  dropoutInput <- dropout[1]
  
  if (dropoutInput > 0)
  {
    trainData <- applyDropoutMaskCpp(trainData, getDropoutMask(darch, 0))
  }
  
  numLayers <- length(darch@layers)
  par <- c()
  dims <- list()
  for (i in 1:numLayers)
  {
    weights <- darch@layers[[i]][["weights"]]
    
    if (dropout[i + 1] > 0 && dropConnect)
    {
      weights <- applyDropoutMaskCpp(weights, getDropoutMask(darch, i))
    }
    
    dims[[i]] <- dim(weights)
    par <- c(par, c(weights))
  }
  
  # optimize
  #flog.debug("Starting the minimize() function.")
  ret <- minimizeCpp(par, fr, cg.length, 1, dims, trainData, matrix(), 0,
                     matMult)
  
  par <- ret[[1]]
  # Add the optimized weights to the darch layers
  startPos <- 1
  endPos <- 0
  for (i in 1:length(dims))
  {
    endPos <- endPos + dims[[i]][1]*dims[[i]][2]
    
    weightsNew <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
    
    if (dropout[i + 1] > 0)
    {
      if (dropConnect)
      {
        weightsNew <- applyDropoutMaskCpp(weightsNew, getDropoutMask(darch, i))
      }
      
      maskDropped <- which(weightsNew == 0)
      weightsNew[maskDropped] <- darch@layers[[i]][["weights"]][maskDropped]
    }
    
    if (getParameter(".darch.trainLayers")[i] == T)
      darch@layers[[i]][["weights"]] <- weightsNew
    
    startPos <- endPos + 1
  }
  
  darch
}

printDarchParams.minimizeAutoencoder <- function(darch,
  lf = futile.logger::flog.info)
{
  lf("[CG] Using unsupervised Conjugate Gradients for fine-tuning")
  printParams(c("cg.length"), "CG")
  lf("[CG] See ?minimizeAutoencoder for documentation")
}