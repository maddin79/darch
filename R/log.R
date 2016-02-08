#' Set the log level.
#' 
#' Convenience wrapper for \code{\link{futile.logger::flog.threshold}} with
#' sanity checking and current level output.
#' 
#' The log levels are defined by the \code{\link{futile.logger}} package.
#' The following levels a available:
#' \tabular{ll}{
#' futile.logger::TRACE\cr 
#' futile.logger::DEBUG\cr
#' futile.logger::INFO\cr
#' futile.logger::WARN\cr
#' futile.logger::ERROR\cr
#' futile.logger::FATAL 
#' }
#' 
#' @param value Log level, must be one of the \code{futile.logger} constants.
#' @export
setLogLevel <- function(value)
{
  
  if (!is.null(value))
  {
    if (value %in% c(futile.logger::TRACE, futile.logger::DEBUG,
                     futile.logger::INFO, futile.logger::WARN,
                     futile.logger::ERROR, futile.logger::FATAL,
                     "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"))
    {
      futile.logger::flog.threshold(value)
    }
    else
    {
      futile.logger::flog.warn(
        "It is not possible to set the log level to %s", value)
    }
  }
  
  futile.logger::flog.info("The current log level is: %s",
                           futile.logger::flog.threshold())
}

logParams <- function(params, prefix, env=parent.frame(),
                      logFunc = futile.logger::flog.info)
{
  for (param in params)
  {
    logFunc("[%s] Parameter %s is %s", prefix, param, get(param, envir=env))
  }
}