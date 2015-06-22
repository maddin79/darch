# Copyright (C) 2015 darch2
# based on code by Martin Drees, copyright (C) 2013 Darch
#
# This file is part of darch2.
#
# Darch2 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch2.  If not, see <http://www.gnu.org/licenses/>.

library(darch2)
library(futile.logger)

startOutputCapture <- function(name)
{
  sink(file=paste(script.dir, paste0(name,".Rout"), sep="/"),
       append=F, type=c("output"), split=T)
}

finalizeOutputCapture <- function(variableList)
{
  print(variableList)
  
  sink(file=NULL)
}