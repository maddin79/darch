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

autosave <- function(darch, dir, trim, digits)
{
  # Make sure the directory exists
  prepareBenchmarkDirectory(dir, save = T, continue = T)
  
  fileName <- fileName <- paste0(dir, "/autosave_",
    formatC(darch@epochs, width = digits, flag = "0"))
  
  saveDArch(darch, fileName, trim)
}