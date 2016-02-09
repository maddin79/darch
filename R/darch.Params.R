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
      if (is.null(mergedParams[[p]]) && !(p %in% blacklist))
      {
        mergedParams[[p]] <- pl[[p]]
      }
    }
  }
  
  mergedParams
}