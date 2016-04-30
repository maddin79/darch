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

# Define our own log function shortcuts for convenience and easy access
# TODO
#TRACE <- futile.logger::flog.trace
#DEBUG <- futile.logger::flog.debug
#INFO <- futile.logger::flog.info
#WARN <- futile.logger::flog.warn
#ERROR <- futile.logger::flog.error
#FATAL <- futile.logger::flog.fatal


#' Set the log level.
#' 
#' Convenience wrapper for \code{\link[futile.logger]{flog.threshold}} with
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
#' @keywords internal
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

logNamedList <- function(l, lf = futile.logger::flog.info)
{
  for (n in names(l))
  {
    # TODO functions?
    lf("%s = %s", n, deparseClean(l[[n]]))
  }
}