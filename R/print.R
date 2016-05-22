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

#' Print \code{\linkS4class{DArch}} details.
#' 
#' Simple redirection to \code{\link{print.DArch}}.
#' 
#' @param object \code{\linkS4class{DArch}} instance
#' @param ... Further parameters to \code{\link{print.DArch}}
#' @keywords internal
setMethod(f = "show", signature = "DArch", definition =
function(object)
{
  print(object)
})

#' Print \code{\linkS4class{DArch}} details.
#'
#' Print verbose information about a \linkS4class{DArch} instance.
#' 
#' Information printed include \code{\link{darch}} parameters and a summary of
#' training statistics.
#' 
#' @param x \code{\linkS4class{DArch}} instance
#' @param ... Further parameters, not used.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris)
#' print(model)
#' }
#' @family darch interface functions
#' @export
print.DArch <- function(x, ...)
{
  printDarchParams.global(x, ...)
  printDarchParams.preProc(x, ...)
  printDarchParams.preTrainDArch(x, ...)
  printDarchParams.fineTuneDArch(x, ...)
}

# Print global parameters (i.e. not specific to pre-training or fine-tuning)
printDarchParams.global <- function(darch, ..., lf = futile.logger::flog.info)
{
  lf("Global parameters:")
  
  # layers
  layersOriginal <- getParameter("layers")
  layers <- getParameter(".layers", layers)
  numLayers <- length(layers)
  lf(paste("Layers parameter was %s, resulted in network with %s layers and",
    "%s neurons"), deparseClean(layersOriginal), numLayers,
    paste(layers, collapse = ", "))
  
  lf("The weights for the layers were generated with %s",
     deparseClean(getParameter("generateWeightsFunction")))
  lf("Additionally, the following parameters were used for weight generation:")
  printParams(c("weights.max", "weights.min", "weights.mean", "weights.sd"),
    "weights")
  
  normalizeWeights <- getParameter(".normalizeWeights", F)
  normalizeWeightsBound <- getParameter(".normalizeWeightsBound")
  
  if (normalizeWeights)
  {
    lf("Weight normalization is enabled using a maxnorm bound of %s",
       normalizeWeightsBound)
  }
  else
  {
    lf("Weight normalization is disabled")
  }
  
  lf("Bootstrapping is %s", if (getParameter(".bootstrap"))
    "enabled with the following parameters:" else "disabled")
  
  if (getParameter(".bootstrap"))
  {
    printParams(c("bootstrap.unique", "bootstrap.num"), "bootstrap")
  }
  
  # TODO store information about train/validation data (number of samples)
  
  # Train data shuffling
  lf("Train data %s shuffled before each epoch",
      if (getParameter(".shuffleTrainData", F, darch)) "are" else "are not")
  
  # Autosave
  autosave <- getParameter(".autosave", F)
  if (autosave)
  {
    lf("Autosaving is enabled with the following settings:")
    
    printParams(c("autosave.dir", "autosave.epochs", "autosave.trim"),
      "autosave", list(".autosave.epochs" = "Autosaving after every %s epochs"),
      darch)
  }
  else
  {
    lf("Autosaving is disabled")
  }
  
  # gputools
  if (getParameter(".gputools", F))
  {
    lf("Using GPU (device %s) for matrix multiplication",
       getParameter(".gputools.deviceId", 0))
  }
  else
  {
    lf("Using CPU for matrix multiplication")
  }
}

printDarchParams.preProc <- function(darch, ...,
  lf = futile.logger::flog.info)
{
  lf("Pre-processing parameters:")
  
  printParams(grep("^preProc\\.*", names(darch@parameters), value = T),
    "preProc", blacklist = c("preProc.params"), ...)
  
  # Pre-processing parameters
  preProcessParams <- getParameter(".preProc.params", F)
  if (is.list(preProcessParams))
  {
    lf("Caret pre-processing is enabled with the following parameters:")
    
    logNamedList(preProcessParams)
  }
  else
  {
    lf("Caret pre-processing is disabled")
  }
}

# Print pre-training parameters
printDarchParams.preTrainDArch <- function(darch, ...,
  lf = futile.logger::flog.info)
{
  lf("Pre-training parameters:")
  printParams(grep("^rbm\\.*", names(darch@parameters), value = T),
              "preTrain", ...)
  epochsTrained <- getParameter(".rbm.numEpochsTrained", 0)
  lf("The selected RBMs have been trained for %s epochs", epochsTrained)
  
  if (epochsTrained > 0)
  {
    lf("Pre-training took %s", format(difftime(Sys.time() +
      darch@stats$preTrainTime, Sys.time(), units = "auto"), digits = 4))
  }
}

# Print fine-tuning parameters
printDarchParams.fineTuneDArch <- function(darch, ...,
  lf = futile.logger::flog.info)
{
  lf("Fine-tuning parameters:")
  printParams(grep("^darch\\.*", names(darch@parameters), value = T),
            "fineTune", ...)
  trainedEpochs <- length(darch@stats$times)
  lf("The network has been fine-tuned for %s epochs", trainedEpochs)
  
  rawErrorFunctionName <-
    getErrorFunctionName(getParameter(".darch.errorFunction"))
  
  if (trainedEpochs > 0)
  {
    lf("Error rates of the last model:")
    
    helper.printErrorRates(darch, trainedEpochs, rawErrorFunctionName, lf)
    
    if (trainedEpochs != darch@epochs)
    {
      lf("The best model was found after %s epochs", darch@epochs)
      lf("Error rates of the best model:")
      
      helper.printErrorRates(darch, darch@epochs, rawErrorFunctionName, lf)
    }
    
    lf("Fine-tuning took %s", format(difftime(Sys.time() +
      darch@stats$fineTuneTime, Sys.time(), units = "auto"), digits = 4))
  }
}

printParams <- function(params, prefix, desc = list(),
  darch = get("darch", envir = parent.frame()),
  lf = futile.logger::flog.info, blacklist = c())
{
  # Automatically blacklist parameters not recognized by darch
  blacklist <- c(blacklist, getParameter(".additionalParameters"))
  
  for (param in params)
  {
    if (param %in% blacklist)
    {
      next
    }
    
    dotParam <- paste0(".", param)
    value <- getParameter(param, "missing")
    dotValue <- getParameter(dotParam, value)
    
    desc[[param]] <-
      if (is.null(desc[[param]])) "Parameter %3$s is %2$s"  else desc[[param]]
    lf(paste("[%s]", desc[[param]]), prefix,
      if (any(sapply(c(dotValue), FUN = is.function))) deparseClean(value)
      else deparseClean(dotValue), param)
    
    # Try to print function documentation / parameters
    if (is.function(dotValue))
    {
      tryCatch({ do.call(paste0("printDarchParams.", value), list(darch)) },
               error = function(e) {})
    }
  }
}

helper.printErrorRates <- function(darch, epoch, rawErrorFunctionName, lf)
{
  isClass <- getParameter(".darch.isClass", F, darch)
  
  lf("Training %s: %.3f", rawErrorFunctionName,
    darch@stats$trainErrors$raw[epoch])
  
  if (isClass)
    lf("Training classification error: %.2f%%",
      darch@stats$trainErrors$class[epoch])
  
  if (length(darch@stats$validErrors$raw) > 0)
  {
    lf("Validation %s: %.3f", rawErrorFunctionName,
      darch@stats$validErrors$raw[epoch])
    
    lf(".632+ %s: %.3f", rawErrorFunctionName,
      darch@stats$dot632Errors$raw[epoch])
    
    if (isClass)
    {
      lf("Validation classification error: %.2f%%",
        darch@stats$validErrors$class[epoch])
      lf(".632+ classification error: %.2f%%",
        darch@stats$dot632Errors$class[epoch])
    }
  }
}