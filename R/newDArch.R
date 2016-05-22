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

#' Constructor function for \code{\linkS4class{DArch}} objects.
#' 
#' Generate a new \code{\linkS4class{DArch}} object with the given parameters.
#' 
#' @details
#' This function is used internally only, please call \code{\link{darch}}
#' to create a new \code{\linkS4class{DArch}} instance.
#' 
#' @param params Additional parameters as a list, also stored in
#'   \code{darch@parameters}.
#' @return The new DArch object
#' @include darch.Class.R
#' @include darch.Setter.R
#' @keywords internal
newDArch <- function(params)
{
  darch <- new("DArch")
  darch@parameters <- params
  layers <- getParameter(".layers")
  futile.logger::flog.info("Constructing a network with %s layers (%s neurons).",
            length(layers), paste(layers, collapse = ', '))
  darch@stats <-
    list("trainErrors" = list("raw" = c(), "class" = c()),
         "validErrors" = list("raw" = c(), "class" = c()),
         "dot632Errors" = list("raw" = c(), "class" = c()),
         "times" = c(), "preTrainTime" = 0, "fineTuneTime" = 0, "validTime" = 0,
         "numPatterns" = list("train" = 0, "valid" = 0))
  darch <- generateRBMs(darch)
  darch <- configureDArch(darch)
  darch
}
