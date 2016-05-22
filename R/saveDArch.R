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

#' Saves a DArch network
#' 
#' Saves the DArch object to the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details If the parameter \code{saveRBM} is \code{FALSE} the field
#'   \code{rbmList} of the DArch object is overwritten by an empty list.
#' 
#' @param darch An instance of the class \code{\linkS4class{DArch}}.
#' @param name The name for the file. Default value is "darch".
#' @param trim Whether to trim the model before saving.
#' @seealso \code{\link{loadDArch}}
#' @include darch.Class.R
#' @export
#' @keywords internal
saveDArch <- function(darch, name="darch", trim = F)
{
  if (trim)
  {
    darch@dataSet <- NULL
    darch@layers <- list()
  }
  
  save(darch, file = paste(name, ".net", sep = ""))
}
