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
#
#
#' The darch-package implements Deep Architecture Networks and restricted Boltzmann machines.
#'
#' The creation of this package is motivated by the papers from G. Hinton et. al.
#' from 2006 (see references for details) and from the MATLAB source code
#' developed in this context. 
#' This package provides the possibility to generate deep architecture networks 
#' (darch) like the deep belief networks from Hinton et. al.. The deep architectures can
#' then be trained with the contrastive divergence method.
#' After this pre-training it can be fine tuned with several learning methods like backpropagation,
#' resilient backpropagation and conjugate gradients. darch2 improves and extends darch, for example by dropout and maxout.
#'
#' \tabular{ll}{
#' Package: \tab darch\cr
#' Type: \tab Package\cr
#' Version: \tab 0.9.1\cr
#' Date: \tab 2013-04-17\cr
#' License: \tab GPL-2\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @import ff futile.logger methods
#' 
#' @author Martin Drees \email{mdrees@@stud.fh-dortmund.de}
#'
#' Maintainer: Johannes Rückert \email{j.rueckert@@gmx.net}
#' @name darch2
#' @docType package
#' @title Deep architectures in R
#' @keywords package 
#'           Neural Networks
#'           darch
#'           Deep-Belief-Networks
#'           Restricted Bolzmann Machines
#'           Contrastive Divergence
#'           Deep Architectures
#'           NN
#'           Neural Nets
#'           Resilient Backpropagation
#'           Backpropagation
#'           Conjugate Gradient
#'           Dropout
#'           Maxout
#'            
#' @references 
#' Hinton, G. E., S. Osindero, Y. W. Teh, A fast learning algorithm for deep 
#' belief nets, Neural Computation 18(7), S. 1527-1554, DOI: 
#' 10.1162/neco.2006.18.7.1527 2006.
#' 
#' Hinton, G. E., R. R. Salakhutdinov, Reducing the dimensionality of data with 
#' neural networks, Science 313(5786), S. 504-507, DOI: 10.1126/science.1127647,
#'  2006.
#' 
#' @examples source("examples/examples.R")
#' 
#' @rdname darch-package 
NULL
