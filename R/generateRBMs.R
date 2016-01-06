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

#' Generates the RBMs for the pre-training.
#' 
#' Used the layer sizes from the DArch object to create the RBM objects for the
#' pre-training. 
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param layers An array with the sizes of the layers
#' @param genWeightFunc The function for generating the weight matrices
#' @return The DArch object with the generated RBMs
#' 
#' @seealso \code{\link{DArch}}
#'          \code{\link{RBM}}
#' 
#' @docType methods
#' @rdname generateRBMs-methods
#' @include darch.R
#' @include rbm.R
#' @export
setGeneric(
  name="generateRBMs",
  def=function(darch,layers,genWeightFunc=generateWeightsRunif){standardGeneric("generateRBMs")}
)

#' @rdname generateRBMs-methods
#' @aliases generateRBMs,DArch-method
setMethod(
  f="generateRBMs",
  signature="DArch",
  definition=function(darch,layers,genWeightFunc=generateWeightsRunif){
    darch@rbmList <- list()
    flog.info("Generating RBMs.")
    for(i in 1:(length(layers)-1)){
      # generate the RBMs
      visible <- layers[[i]]
      hidden <- layers[[(i+1)]]
      rbm <- newRBM(visible,hidden,getBatchSize(darch),flog.logger()$threshold,genWeightFunc)
      darch@rbmList[i] <- rbm
      darch <- addLayer(darch, getWeights(rbm), getHiddenBiases(rbm),
                        sigmoidUnitDerivative, weightDecayWeightUpdate)
    }
    return(darch)
  }
)
