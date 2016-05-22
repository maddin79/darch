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

setGeneric(
  name = "generateRBMs",
  def = function(darch){standardGeneric("generateRBMs")}
)

#' Generates the RBMs for the pre-training.
#' 
#' Used the layer sizes from the \code{\linkS4class{DArch}} object to create the
#' RBM objects for the pre-training. 
#' 
#' @param darch An instance of the class \code{DArch} for which RBMs are
#'   to be generated.
#' @return The DArch object with the generated RBMs
#' 
#' @seealso \code{\linkS4class{DArch}}
#'          \code{\linkS4class{RBM}}
#' 
#' @include darch.Class.R
#' @include rbm.Class.R
#' @keywords internal
setMethod(
  f = "generateRBMs",
  signature = "DArch",
  definition = function(darch)
  {
    darch@rbmList <- list()
    layers <- getParameter(".layers")
    futile.logger::flog.info("Generating RBMs.")
    for (i in 1:(length(layers) - 1))
    {
      # generate the RBMs
      visible <- layers[[i]]
      hidden <- layers[[(i + 1)]]
      futile.logger::flog.info(paste("Constructing new RBM instance with %s", 
        "visible and %s hidden units."), visible, hidden)
      rbm <- new("RBM")
      rbm@parameters <- darch@parameters
      rbm@parameters[[".numVisible"]] <- visible
      rbm@parameters[[".numHidden"]] <- hidden
      rbm@parameters[[".generateWeightsFunction"]] <-
        getParameter(".generateWeightsFunction")[[i]]
      rbm <- resetRBM(rbm)
      darch@rbmList[[i]] <- rbm
      darch <- addLayer(darch, rbm@weights, rbm@hiddenBiases,
        getParameter(".darch.unitFunction")[[i]],
        getParameter(".darch.weightUpdateFunction")[[i]])
    }
    
    darch
  }
)
