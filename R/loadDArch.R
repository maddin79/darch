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

#' Loads a DArch network
#' 
#' Convenience function for loading \code{\linkS4class{DArch}} instances from
#' disk. There is no special re-initialization logic, so it is not necessary to
#' use this function, you can also just use \code{\link{load}}.
#' 
#' Loads the DArch object from the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details Make sure when you load a DArch object that every file written by 
#' the \code{\link{saveDArch}} function is in the working directory.
#' 
#' @param name The name of the file without the ending ".net".
#' @return \code{DArch} instance: the loaded deep architecture
#' @seealso \code{\link{saveDArch}}
#' @include darch.Class.R
#' @export
#' @keywords internal
#' @docType methods
#' @rdname loadDArch
loadDArch <- function(name="darch")
{
  local(get(load(paste0(name, ".net"))))
}
