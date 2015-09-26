# Copyright (C) 2013-2015 Martin Drees
# based on code by Martin Drees, copyright (C) 2013 Darch
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
# along with darch.  If not, see <http://www.gnu.org/licenses/>.

startOutputCapture <- function(name)
{
  if (getOption("darch.examples.sinkOutput", F))
  {
    sink(file=paste0(name,".Rout"),
         append=F, type=c("output"), split=T)
  }
}

finalizeOutputCapture <- function(variableList=list())
{
  if (length(variableList) > 0)
  {
    print(variableList)
  }
  
  if (getOption("darch.examples.sinkOutput", F))
  {
    sink(file=NULL)
  }
}

source(paste0(system.file(package="darch"), "/examples/example.iris.R"))
source(paste0(system.file(package="darch"), "/examples/example.maxout.R"))
source(paste0(system.file(package="darch"), "/examples/example.mnist.R"))
source(paste0(system.file(package="darch"), "/examples/example.xor.R"))
source(paste0(system.file(package="darch"), "/examples/example.xor_nominal.R"))



cat(paste("All examples loaded. Use options(darch.examples.sinkOutput=T) if",
    "you want example output to be stored in .Rout files in the working",
    "directory. Available examples: example.iris(), example.mnist(),",
    "example.maxout(), example.xor(), example.xorNominal()."))