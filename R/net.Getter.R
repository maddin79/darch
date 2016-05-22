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

#' Returns the current momentum of the \code{Net}.
#'
#' @param net A instance of the class \code{Net}.
#'
#' @seealso \code{\linkS4class{Net}}
#' @include net.Class.R
#' @keywords internal
setGeneric("getMomentum",function(net){standardGeneric("getMomentum")})

setMethod(
  f = "getMomentum",
  signature = "Net",
  definition = function(net)
  {
    # TODO optimize for speed
    calculateMomentum(getParameter(".darch.initialMomentum", net = net),
      getParameter(".darch.finalMomentum", net = net),
      getParameter(".darch.momentumRampLength", net = net),
      getParameter(".darch.epochsScheduled", net = net), net@epochs)
  }
)
