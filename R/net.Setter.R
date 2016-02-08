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
  definition=function(net,value)
  {
    net@learnRate <- value
    net@initialLearnRate <- value
    net
  }
)
