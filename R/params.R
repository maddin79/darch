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

getDarchParam <- function(param, default=stop(futile.logger::flog.error(
  "Missing darch parameter \"%s\" and no default given.", param)),
  darch = get("darch", envir = parent.frame()), ...)
{
  if (!is.null(darch@params[[param]])) darch@params[[param]] else default
}

#' Set \code{\link{DArch}} parameters
#' 
#' Allows setting \code{\link{DArch}} parameters normally passed to the
#' \code{\link{darch}} interface function when not using said interface.
#' These parameters can also be passed to \code{\link{newDArch}}.
#'
#' @param darch \code{\link{DArch}} instance.
#' @param ... Parameters to be set, see \code{\link{darch.default}}.
#' @export
#' @keywords internal
setDarchParams <- function(darch, ...)
{
  darch@params <- c(list(...), darch@params)
}

# Merge parameter lists, later values do not overwrite earlier ones
mergeParams <- function(..., blacklist = c())
{
  paramLists <- list(...)
  mergedParams <- list()
  
  for (i in 1:length(paramLists))
  {
    pl <- paramLists[[i]]
    
    if (!is.list(pl) || is.null(names(pl)))
    {
      next
    }
    
    for (p in names(pl))
    {
      if (!is.null(mergedParams[[p]]) || (p %in% blacklist))
      {
        next
      }
      
      mergedParams[[p]] <- pl[[p]]
      
      # Convert to function if necessary
      if (substr(p, start=nchar(p) - 7, stop = nchar(p)) == "Function")
      {
        mergedParams[[p]] <- (if (length(mergedParams[[p]]) > 1)
          as.list(mergedParams[[p]]) else list(mergedParams[[p]]))
        pDot <- paste0(".", p)
        # Store for internal use, parameters starting with "." are not printed
        mergedParams[[pDot]] <-
          sapply(mergedParams[[p]], characterToFunction, USE.NAMES=F)
        # Store as string
        mergedParams[[p]] <- unlist(sapply(mergedParams[[p]],
          functionToCharacter, USE.NAMES=F, default = "non-darch function"))
        
        # Force unlisting
        if (length(mergedParams[[pDot]]) == 1)
        {
          mergedParams[[pDot]] <- mergedParams[[pDot]][[1]]
        }
        
        if (is.null(unlist(mergedParams[[pDot]])))
        {
          stop(futile.logger::flog.error(
            "Could not find function(s) \"%s\" for parameter %s",
            deparse(pl[[p]]), p))
        }
      }
    }
  }
  
  mergedParams
}

# Processes and validates a given list of darch parameters
processParams <- function(params)
{
  dataSet <- get("dataSet", envir = parent.frame())
  
  # TODO whitelist parameters like na.action and other known exceptions
  if (length(params[["additionalParameters"]]) > 0)
  {
    futile.logger::flog.warn(paste("The following parameters are not",
      "supported by darch and may be ignored: %s"),
      paste(names(params[["additionalParameters"]]), collapse=", "))
  }
  
  if (is.null(params[["matMult"]]))
  {
    params[["matMult"]] <- `%*%`
  }
  
  params[["debug"]] <- (names(futile.logger::DEBUG) ==
    futile.logger::flog.threshold())
  
  if (params[["gputools"]])
  {
    if ((length(find.package("gputools", quiet=T)) == 0))
    {
      futile.logger::flog.warn("gputools package not available.")
      futile.logger::flog.info("Using CPU matrix multiplication.")
      params[["gputools"]] <- F
    }
    else
    {
      params[["matMult"]] <- gputools::gpuMatMult
      # TODO handle invalid values and errors from chooseGpu()
      params[["gputools.deviceId"]] <- gputools::chooseGpu(gputools.deviceId)
      
      futile.logger::flog.info(paste("Using GPU matrix multiplication on",
        "device", params[["gputools.deviceId"]]))
    }
  }
  else
  {
    futile.logger::flog.info("Using CPU matrix multiplication.")
  }
  
  if (is.null(dataSet@targets) && params[["darch.returnBestModel"]])
  { 
    futile.logger::flog.warn(paste("No targets were provided, automatically",
      "changing darch.returnBestModel to FALSE"))
    
    params[["darch.returnBestModel"]] <- F
  }
  
  # TODO move into dataset validation?
  if (params[["darch.isClass"]] && is.null(dataSet@targets))
  {
    futile.logger::flog.warn(
      "No targets were provided, setting darch.isClass to FALSE")
    params[["darch.isClass"]] <- F
  }
  
  # TODO problematic for huge datasets?
  if (params[["darch.isClass"]] &&
    length(unique(c(dataSet@targets))) > 2)
  {
    futile.logger::flog.warn(
      "darch.isClass was set to TRUE while numeric targets were provided")
  }
  
  params[[".layers"]] <- params[["layers"]]
  
  # Allow deparsed vector to be passed
  # TODO document
  if (is.character(params[["layers"]]))
  {
    params[[".layers"]] <- eval(parse(text = params[["layers"]]))
  }
  
  # Create default layers vector if scalar given
  if (length(params[[".layers"]]) == 1)
  {
    futile.logger::flog.warn(paste("No vector given for \"layers\" parameter,",
      "constructing DBN with one hidden layer of %s neurons."),
      params[[".layers"]])
    params[[".layers"]] = c(ncol(dataSet@data), params[[".layers"]],
                            ncol(dataSet@targets))
  }
  
  layers <- params[[".layers"]]
  
  numLayers = length(layers)
  
  # Adjust neurons in input layer
  if (layers[1] != ncol(dataSet@data))
  {
    futile.logger::flog.info(paste("Changing number of neurons in the input",
      "layer from %s to %s based on dataset."), layers[1], ncol(dataSet@data))
    layers[1] <- ncol(dataSet@data)
  }
  
  # Adjust neurons in output layer if classification
  if (params[["darch.isClass"]] && layers[numLayers] != ncol(dataSet@targets))
  {
    futile.logger::flog.info(paste("Changing number of neurons in the output",
      "layer from %s to %s based on dataset."), layers[numLayers],
      ncol(dataSet@targets))
    layers[numLayers] <- ncol(dataSet@targets)
  }
  
  params[[".layers"]] <- layers
  
  # Print warning if using GPU matrix multiplication with small network
  if (params[["gputools"]])
  {
    matrixSizes <- vector(mode = "numeric", length = numLayers - 1)
    
    for (i in 1:(numLayers - 1))
    {
      matrixSizes[i] <-
        params[["darch.batchSize"]] * layers[i] + layers[i] * layers[i+1]
    }
    
    # TODO matrix multiplication for validation is not considered
    # TODO what can be considered small? for now, average of less than 100x100
    if (mean(matrixSizes) < 100*100*2)
    {
      futile.logger::flog.warn(paste("Due to small network and / or batch",
        "size, GPU matrix multiplication may be slower than CPU matrix",
        "multiplication."))
    }
  }
  
  # Validate weight generation
  params[[".generateWeightsFunction"]] <-
    (if (length(params[[".generateWeightsFunction"]]) == 1)
    replicate(numLayers - 1, params[[".generateWeightsFunction"]]) else
    params[[".generateWeightsFunction"]])
  
  if (length(params[[".generateWeightsFunction"]]) != (numLayers - 1))
  {
    stop(futile.logger::flog.error(
      "Invalid number of weight generation functions (expected %s, got %s)",
      numLayers - 1, length(params[[".generateWeightsFunction"]])))
  }
  
  params[[".darch.unitFunction"]] <-
    (if (length(params[[".darch.unitFunction"]]) == 1)
    replicate(numLayers - 1, params[[".darch.unitFunction"]]) else
    params[[".darch.unitFunction"]])
  params[[".darch.weightUpdateFunction"]] <-
    (if (length(params[[".darch.weightUpdateFunction"]]) == 1)
      replicate(numLayers - 1, params[[".darch.weightUpdateFunction"]]) else
        params[[".darch.weightUpdateFunction"]])
  
  if (length(params[[".darch.unitFunction"]]) != (numLayers - 1))
  {
    stop(futile.logger::flog.error(
      "Invalid number of unit functions (expected %s, got %s)",
      numLayers - 1, length(params[[".darch.unitFunction"]])))
  }
  
  if (length(params[[".generateWeightsFunction"]]) != (numLayers - 1))
  {
    stop(futile.logger::flog.error(
      "Invalid number of weight update functions (expected %s, got %s)",
      numLayers - 1, length(params[[".generateWeightsFunction"]])))
  }
  
  # backpropagation
  params[[".bp.learnRate"]] <- (if (length(params[["bp.learnRate"]]) == 1)
    replicate(numLayers - 1, params[["bp.learnRate"]]) else
      params[["bp.learnRate"]])
  
  if (length(params[[".bp.learnRate"]]) != (numLayers - 1))
  {
    stop(futile.logger::flog.error(
      "Invalid number of backprop learning rates (expected %s, got %s)",
      numLayers, length(params[[".bp.learnRate"]])))
  }
  
  params
}