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

# TODO move to its own file, not Net-specific

#' Sets the log level for the \code{\link{Net}}.
#'
#' The log levels a defined by the \code{\link{futile.logger}} package.
#' The following levels a available:
#' \tabular{ll}{
#' TRACE\cr 
#' DEBUG\cr
#' INFO\cr
#' WARN\cr
#' ERROR\cr
#' FATAL 
#' }
#' 
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname setLogLevel-methods
setGeneric("setLogLevel<-",function(net,value){standardGeneric("setLogLevel<-")})

#' @rdname setLogLevel-methods
#' @aliases setLogLevel<-,Net-method
#' @name setLogLevel
setReplaceMethod(
  f="setLogLevel",
  signature="Net",
  definition=function(net,value){
    if (value == TRACE | 
       value == DEBUG |
       value == WARN | 
       value == ERROR | 
       value == FATAL |
       value == INFO){
      flog.threshold(value)
    }else{
      flog.warn(net@logger,paste("It is not possible to set the log level to",value))
      flog.info(net@logger,paste("The current log level is:",flog.logger()$threshold))
    }
    return (net)
  }
)

#' Sets the learning rate.
#' 
#' @param net A instance of the class \code{\link{Net}}.
#' @param value Object of the class \code{numeric}.
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
setGeneric("setLearnRate<-",function(net,value){standardGeneric("setLearnRate<-")})

setReplaceMethod(
  f="setLearnRate",
  signature="Net",
  definition=function(net,value){
    net@learnRate <- value
    net@initialLearnRate <- value
    return (net)
  }
)
