# Copyright (C) 2013-2016 Martin Drees
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
#' @param x \code{\linkS4class{DArch}} instance
#' @param ... Further parameters to \code{\link{print.DArch}}
#' @export
setMethod(f = "show", signature = "DArch", definition =
function(object)
{
  print(object)
})

#' Print \code{\linkS4class{DArch}} details.
#'
#' Print verbose information about a \linkS4class{DArch} instance.
#' 
#' @param x \code{\linkS4class{DArch}} instance
#' @param ... Further parameters, not used.
#' @aliases print.darch
#' @family darch interface functions
#' @export
print.DArch <- function(x, ...)
{
  printDarchParams.global(x, ...)
  printDarchParams.preTrainDArch(x, ...)
  printDarchParams.fineTuneDArch(x, ...)
}

# Print global parameters (i.e. not specific to pre-training or fine-tuning)
printDarchParams.global <- function(darch, ..., lf = futile.logger::flog.info)
{
  lf("Global parameters:")
  
  # layers
  layersOriginal <- getDarchParam("layers.original", darch = darch)
  layers <- getDarchParam("layers", layers, darch)
  numLayers <- length(layers)
  lf(paste("Layers parameter was %s, resulted in network with %s layers and",
    "%s neurons"), deparse(layersOriginal), numLayers,
    paste(layers, collapse = ", "))
  
  # Pre-processing parameters
  preProcessParams <- getDarchParam("caret.preProcessParams", F, darch)
  if (is.list(preProcessParams))
  {
    lf("Caret pre-processing is enabled with the following parameters:")
    
    logNamedList(preProcessParams)
  }
  else
  {
    lf("Pre-processing is disabled")
  }
  
  normalizeWeights <- getDarchParam("normalizeWeights", F, darch)
  normalizeWeightsBound <- getDarchParam("normalizeWeightsBound", darch = darch)
  
  if (normalizeWeights)
  {
    lf("Weight normalization is enabled using a maxnorm bound of %s",
       normalizeWeightsBound)
  }
  else
  {
    lf("Weight normalization is disabled")
  }
  
  # Train data shuffling
  lf ("Train data %s shuffled before each epoch",
      if (getDarchParam("shuffleTrainData", F, darch)) "are" else "are not")
  
  # Autosave
  autosave <- getDarchParam("autosave", F, darch)
  if (autosave)
  {
    lf("Autosaving is enabled with the following settings:")
    
    printParams(c("autosave.location", "autosave.epochs"), "autosave", list(
      "autosave.epochs" = "Autosaving after every %s epochs"), darch)
  }
  else
  {
    lf("Autosaving is disabled")
  }
  
  # gputools
  if (getDarchParam("gputools", F, darch))
  {
    lf("Using GPU (device %s) for matrix multiplication",
       getDarchParam("gputools.deviceId", darch = darch))
  }
  else
  {
    lf("Using CPU for matrix multiplication")
  }
}

# Print pre-training parameters
printDarchParams.preTrainDArch <- function(darch, ...,
  lf = futile.logger::flog.info)
{
  lf("Pre-training parameters:")
  printParams(names(darch@params)[grep("^rbm.*", names(darch@params))],
              "preTrain", darch = darch, ...)
  epochsTrained <- getDarchParam(".rbm.numEpochs", 0, darch)
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
  printParams(names(darch@params)[grep("^darch.*", names(darch@params))],
            "fineTune", darch = darch, ...)
  trainedEpochs <- length(darch@stats$times)
  lf("The network has been trained for %s epochs", trainedEpochs)
  
  rawErrorFunctionName <- getErrorFunctionName(darch@errorFunction)
  isClass <- getDarchParam("darch.isClass", F, darch)
  
  if (trainedEpochs > 0)
  {
    if (trainedEpochs != darch@epochs)
    {
      lf("The best model was found after %s epochs", darch@epochs)
      lf("Error rates of the best model:")
    }
    else
    {
      lf("Error rates of the final model:")
    }
    
    lf("Training %s: %.3f", rawErrorFunctionName,
       darch@stats$trainErrors$raw[darch@epochs])
    
    if (isClass)
      lf("Training classification error: %.2f%%",
         darch@stats$trainErrors$class[darch@epochs])
    
    if (length(darch@stats$validErrors$raw) > 0)
    {
      lf("Validation %s: %.3f", rawErrorFunctionName,
         darch@stats$validErrors$raw[darch@epochs])
      
      lf(".632+ %s: %.3f", rawErrorFunctionName,
         darch@stats$dot632Errors$raw[darch@epochs])
      
      if (isClass)
      {
        lf("Validation classification error: %.2f%%",
           darch@stats$validErrors$class[darch@epochs])
        lf(".632+ classification error: %.2f%%",
           darch@stats$dot632Errors$class[darch@epochs])
      }
    }
    
    lf("Fine-tuning took %s", format(difftime(Sys.time() +
      darch@stats$fineTuneTime, Sys.time(), units = "auto"), digits = 4))
  }
}

printParams <- function(params, prefix, desc = list(), darch,
  lf = futile.logger::flog.info)
{
  for (param in params)
  {
    dotParam <- paste0(".", param)
    value <- getDarchParam(param, "missing", darch)
    dotValue <- getDarchParam(dotParam, value, darch)
    
    desc[[param]] <-
      if (is.null(desc[[param]])) "Parameter %s is %s"  else desc[[param]]
    lf(paste("[%s]", desc[[param]]), prefix, param, deparse(value))
    
    # Try to print function documentation / parameters
    if (is.function(dotValue))
    {
      tryCatch({ do.call(paste0("printDarchParams.", value), list(darch)) },
               error = function(e) {})
    }
  }
}