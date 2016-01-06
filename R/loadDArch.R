# Copyright (C) 2013-2015 Martin Drees
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

#' Loads a DArch network
#' 
#' Loads the DArch object from the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details Make sure when you load a DArch object that every file written by 
#' the \code{\link{saveDArch}} function is in the working directory.
#' 
#' @param name The name of the file without the ending ".net".
#' 
#' @return \code{darch} - The loaded deep architecture
#' 
#' @usage loadDArch(name="darch")
#' 
#' @seealso \code{\link{saveDArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname loadDArch
loadDArch <- function(name="darch")
{
  load(paste(name,".net",sep=""))
  darch
}
