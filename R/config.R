# Copyright (C) 2016 Johannes Rueckert
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

#' @include darch.Class.R
configureDArch <- function(darch)
{
  numLayers <- length(getParameter(".layers"))
  
  # per-layer configuration
  for (i in 1:(numLayers - 1))
  {
    # Layer functions
    darch@layers[[i]][["unitFunction"]] <-
      getParameter(".darch.unitFunction")[[i]]
    
    # Weight update functions
    darch@layers[[i]][["weightUpdateFunction"]] <-
      getParameter(".darch.weightUpdateFunction")[[i]]
  }
  
  dropout <- getParameter(".darch.dropout")
  
  # Execute function
  if (length(dropout) <= 1 ||
      all(dropout[2:length(dropout)] == 0))
  {
    darch@parameters[[".darch.executeFunction"]] <- runDArch
  }
  else
  {
    darch@parameters[[".darch.executeFunction"]] <- runDArchDropout
  }
  
  darch
}