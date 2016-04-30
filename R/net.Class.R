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

#' Abstract class for neural networks.
#' 
#' This is an abstract class for neural networks. It provides some 
#' functionalities used in more than one network type.   
#' 
#' @slot epochs Number of epochs the network has been trained for.
#' @slot stats Training statistics.
#' @slot parameters List of parameters which do not change throughout training.
#' @family darch classes
#' @author Martin Drees
#' @exportClass Net
#' @rdname Net
#' @keywords internal
setClass(
  Class = "Net",
  representation = representation(
    epochs = "numeric",
    stats = "list",
    parameters = "list"
  )
)