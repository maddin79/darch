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

calculateMomentum <- function(initial, final, rampLength, epochsScheduled, epochsTrained)
{
  if (rampLength <= 0 || epochsScheduled <= 1 ||
      epochsTrained >= rampLength * epochsScheduled)
  {
    momentum <- final
  }
  else if (epochsTrained <= 1) momentum <- initial
  else
  {
    momentum <- (initial + (final - initial) * (epochsTrained - 1)
      / (rampLength * epochsScheduled - 1))
  }
  
  momentum
}