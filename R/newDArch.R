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

#' Constructor function for \code{\link{DArch}} objects.
#' 
#' Generate a new \code{\link{DArch}} object with the given parameters.
#' 
#' @details
#' It is recommended to use this function for generating a new 
#' \code{\link{DArch}} object, because this function generates and sets all the
#' necessary parameters like the internally used \code{\link{RBM}} networks, 
#' the list of statistics (\code{stats}) etc.
#' 
#' @param layers Array of layer sizes.
#' @param batchSize Size of the batches
#' @param logLevel The logging level. See \code{\link{setLogLevel}} for details.
#' @param genWeightFunc The function for generating the weight matrices
#' 
#' @return The new DArch object
#' @include darch.R
#' @include darch.Setter.R
#' 
#' @export
newDArch <- function(layers,batchSize, 
                     logLevel=INFO, genWeightFunc=generateWeightsRunif){
  darch <- new("DArch")
  flog.threshold(logLevel)
  flog.info(paste("Constructing a darch with", length(layers), "layers."))
  setBatchSize(darch) <- batchSize  
  setGenWeightFunction(darch) <- genWeightFunc
  setStats(darch) <-
    list("dataErrors" = list("raw"=c(), "class" = c()),
         "validErrors" = list("raw"=c(), "class" = c()),
         "times" = c(), "preTrainTime" = 0, "fineTuneTime" = 0)
  darch <- generateRBMs(darch,layers,genWeightFunc)
  return(darch)
}
