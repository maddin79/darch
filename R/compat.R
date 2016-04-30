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

# parameter compability list
compatibilityList <- function()
{
  list(
      "scale" = "preProc.params",
      "caret.preProcessParams" = "preProc.params",
      "preProc.logicalToNumeric" = "preProc.fullRank",
      "preProc.logicalToNumeric.targets" = "preProc.fullRank.targets",
      "preProc.orderedToNumeric" = "preProc.factorToNumeric",
      "preProc.orderedToNumeric.targets" = "preProc.factorToNumeric.targets",
      "rbm.trainOutputLayer" = "rbm.lastLayer",
      "rbm.learnRateWeights" = "rbm.learnRate",
      "rbm.learnRateBiasVisible" = "rbm.learnRate",
      "rbm.learnRateBiasHidden" = "rbm.learnRate",
      "rbm.weightCost" = "rbm.weightDecay",
      "rbm.momentumSwitch" = "rbm.momentumRampLength",
      "rbm.visibleUnitFunction" = "rbm.unitFunction",
      "rbm.hiddenUnitFunction" = "rbm.unitFunction",
      "rbm.genWeightFunction" = "generateWeightsFunction",
      "darch.bootstrap" = "bootstrap",
      "darch.genWeightFunc" = "generateWeightsFunction",
      "darch.logLevel" = "logLevel",
      "darch.momentumSwitch" = "darch.momentumRampLength",
      "darch.learnRateWeights" = "bp.learnRate",
      "darch.learnRateBiases" = "bp.learnRate",
      "darch.dropoutHidden" = "darch.dropout",
      "darch.dropoutInput" = "darch.dropout",
      "darch.dropoutOneMaskPerEpoch" = "darch.dropout.oneMaskPerEpoch",
      "darch.layerFunction" = "darch.unitFunction",
      "darch.layerFunctions" = "darch.unitFunction",
      "darch.layerFunctionDefault" = "darch.unitFunction",
      "darch.layerFunction.maxout.poolSize" = "darch.maxout.poolSize",
      "darch.unitFunction.maxout.poolSize" = "darch.maxout.poolSize",
      "darch.retainData" = "retainData"
  )
}