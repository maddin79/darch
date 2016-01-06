getDarchParam <- function(param,
  default=stop(paste("Missing DArch parameter \"", param,
                     "\" and no default given.")), darch)
{
  if (!is.null(darch@params[[param]])) darch@params[[param]] else default
}