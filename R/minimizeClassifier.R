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

#' Conjugate gradient for a classification network
#' 
#' This function trains a \code{\linkS4class{DArch}} classifier network with the
#' conjugate gradient method. 
#' 
#' @details
#' This function is build on the basis of the code from G. Hinton et. al.
#' (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html - last visit 
#' 2016-04-30) for the fine tuning of deep belief nets. The original code is 
#' located in the files 'backpropclassify.m', 'CG_MNIST.m' and 
#' 'CG_CLASSIFY_INIT.m'. 
#' It implements the fine tuning for a classification net with backpropagation
#' using a direct translation of the \code{\link{minimize}} function from C. 
#' Rassmussen (available at http://www.gatsby.ucl.ac.uk/~edward/code/minimize/ 
#' - last visit 2016-04-30) to R.
#' The parameter \code{cg.switchLayers} is for the switch between two training 
#' type. Like in the original code, the top two layers can be trained alone 
#' until \code{epoch} is equal to \code{epochSwitch}. Afterwards the entire 
#' network will be trained.
#' 
#' \code{minimizeClassifier} supports dropout but does not use the weight
#' update function as defined via the \code{darch.weightUpdateFunction}
#' parameter of \code{\link{darch}}, so that weight decay, momentum etc. are not
#' supported.
#' 
#' @param darch A instance of the class \code{\linkS4class{DArch}}.
#' @param trainData The training data matrix.
#' @param targetData The labels for the training data.
#' @param cg.length Numbers of line search 
#' @param cg.switchLayers Indicates when to train the full network instead of
#'   only the upper two layers
#' @inheritParams backpropagation
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris,
#'  preProc.params = list(method = c("center", "scale")),
#'  darch.unitFunction = c("sigmoidUnit", "softmaxUnit"),
#'  darch.fineTuneFunction = "minimizeClassifier",
#'  cg.length = 3, cg.switchLayers = 5)
#' }
#' @return The trained \code{\linkS4class{DArch}} object.
#' @seealso \code{\link{darch}}, \code{\link{fineTuneDArch}}
#' @family fine-tuning functions
#' @docType methods
#' @rdname minimizeClassifier
#' @include darch.Class.R
#' @export
minimizeClassifier <- function(darch, trainData, targetData,
  cg.length = getParameter(".cg.length"),
  cg.switchLayers = getParameter(".cg.length"),
  dropout = getParameter(".darch.dropout"),
  dropConnect = getParameter(".darch.dropout.dropConnect"),
  matMult = getParameter(".matMult"),
  debugMode = getParameter(".debug"), ...)
{
  # Function for gradients ###############################
  fr <- function(par,dims,data,target,epochSwitch){
    startPos <- 1
    endPos <- 0
    numRows <- dim(data)[1]
    length <- length(dims)
    outputs <- list()
    gradients <- list()
    derivatives <- list()
    weights <- list()
    
    if (epochSwitch){
      d <- data
      # Calculating the outputs
      for(i in 1:length){
        d <- cbind(d,rep(1,numRows))
        endPos <- endPos + dims[[i]][1]*dims[[i]][2]
        weights[[i]] <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
        startPos <- endPos+1
        
        
        if (i < length && dropout[i + 1] > 0 && !dropConnect)
        {
          dropoutMask <- getDropoutMask(darch, i)
          ret <- darch@layers[[i]][["unitFunction"]](matMult(d, weights[[i]]),
            darch = darch, dropoutMask = dropoutMask)
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
          futile.logger::flog.debug(
            "Layer %s: Activation standard deviation: %s", i, sd(outputs[[i]]))
          futile.logger::flog.debug(
            "Layer %s: Derivatives standard deviation: %s", i,
            sd(derivatives[[i]]))
        }
        
        d <- outputs[[i]]
      }
      
      output <- outputs[[length]]
      f = -sum(target*log(output))
      
      ix <- output-target
      out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
      gradients[[length]] <- matMult(t(out), ix)
      
      for(i in (length-1):1){
        derivatives[[i]] <- cbind(derivatives[[i]],rep(1,nrow(derivatives[[i]])))
        ix <- (matMult(ix, t(weights[[i+1]])))* derivatives[[i]] # outputs[[i]]*(1-outputs[[i]])
        ix <- ix[,1:(dim(ix)[2]-1)]
        if (i > 1){
          out <- cbind(outputs[[i-1]],rep(1,nrow(outputs[[i-1]])))
          gradients[[i]] <- matMult(t(out), ix)
        }else{
          d <- cbind(data,rep(1,numRows))
          gradients[[i]] <- matMult(t(d), ix)
        }
      }
    }else{
      d <- cbind(data,rep(1,numRows))
      endPos <- endPos + dims[[1]][1]*dims[[1]][2]
      weights <- matrix(par[startPos:endPos],dims[[1]][1],dims[[1]][2])
      
      ret <- darch@layers[[length(darch@layers)]][["unitFunction"]](matMult(d,
              weights), net = darch)

      output <- ret[[1]]
      
      f = -sum(target*log(output))
      
      ix <- output-target
      gradients[[1]] <- matMult(t(d), ix)
    }
    
    ret <- c(f)
    for(i in 1:length(gradients)){
      ret <- c(ret,c(gradients[[i]]))
    }
    ret
  }
  # End function for gradients ###############################
  
  # Initialize CG parameters on first run
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
  
  epochSwitch <- (darch@epochs >= cg.switchLayers)
  
  if (epochSwitch)
  {  
    for(i in 1:numLayers)
    {
      if (dropout[i + 1] > 0 && dropConnect)
      {
        weights <-
          applyDropoutMaskCpp(darch@layers[[i]][["weights"]],
          getDropoutMask(darch, i))
      }
      else
      {
        weights <- darch@layers[[i]][["weights"]]
      }
      
      dims[[i]] <- dim(weights)
      par <- c(par,c(weights))		
    }
  }
  else
  {  
    layers <- darch@layers
    length <- length(layers)
    numRows <- dim(trainData)[1]
    
    # Fordward-propagate until last layer
    for (i in 1:(length - 1))
    {
      if (dropout[i + 1] > 0 && dropConnect)
      {
        weights <- applyDropoutMaskCpp(darch@layers[[i]][["weights"]],
                                    getDropoutMask(darch, i))
      }
      else
      {
        weights <- darch@layers[[i]][["weights"]]
      }
      
      if (dropout[i + 1] > 0 && !dropConnect)
      {
        trainData <- applyDropoutMaskCpp(darch@layers[[i]][["unitFunction"]](
          matMult(cbind(trainData,rep(1, numRows)), weights), darch = darch,
          dropoutMask = getDropoutMask(darch, i))[[1]],
          getDropoutMask(darch, i))
      }
      else
      {
        trainData <- darch@layers[[i]][["unitFunction"]](
          matMult(cbind(trainData,rep(1, numRows)), weights), net = darch)[[1]]
      }
    }
    
    if (dropout[i + 1] > 0 && dropConnect)
    {
      weights <- applyDropoutMaskCpp(darch@layers[[length]][["weights"]],
                                  getDropoutMask(darch, length))
    }
    else
    {
      weights <- darch@layers[[length]][["weights"]]
    }
    
    dims[[1]] <- dim(weights)
    par <- c(par,c(weights))
  }
  
  # optimize
  #flog.debug("Starting the minimize() function.")
  ret <- minimizeCpp(par, fr, cg.length, 1, dims, trainData, targetData,
    epochSwitch, matMult)
  
  par <- ret[[1]]
  
  # Add the optimized weights to the darch layers
  startPos <- 1
  endPos <- 0
  
  for (i in 1:length(dims))
  {
    endPos <- endPos + dims[[i]][1]*dims[[i]][2]
    weightsNew <- matrix(par[startPos:endPos],dims[[i]][1],dims[[i]][2])
    
    layerNum <- if (epochSwitch) i else length(darch@layers)
    
    if (dropout[i + 1] > 0)
    {
      if (dropConnect)
      {
        weightsNew <- applyDropoutMaskCpp(weightsNew, getDropoutMask(darch, layerNum))
      }
      
      # TODO relevant without DropConnect?
      maskDropped <- which(weightsNew == 0)
      weightsNew[maskDropped] <- darch@layers[[layerNum]][["weights"]][maskDropped]
    }
    
    if (getParameter(".darch.trainLayers")[layerNum] == T)
      darch@layers[[layerNum]][["weights"]] <- weightsNew
    
    startPos <- endPos + 1
    
    if (layerNum != i) break
  }
  
  darch
}

printDarchParams.minimizeClassifier <- function(darch,
  lf = futile.logger::flog.info)
{
  lf("[CG] Using supervised Conjugate Gradients for fine-tuning")
  printParams(c("cg.length", "cg.switchLayers"), "CG")
  lf("[CG] See ?minimizeClassifier for documentation")
}