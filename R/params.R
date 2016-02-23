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

getDarchParam <- function(param,
  default=stop(paste("Missing DArch parameter \"", param,
                     "\" and no default given.")), darch, ...)
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
          futile.logger::flog.error(
            "Could not find function(s) \"%s\" for parameter %s",
            deparse(pl[[p]]), p)
          stop("Invalid configuration")
        }
      }
    }
  }
  
  mergedParams
}

# Takes one or more functions or strings and returns a list containing both the
# 
expandFunctionParam <- function(f)
{
  
}