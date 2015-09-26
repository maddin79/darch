# Copyright (C) 2013-2015 Martin Drees
#
# This file is part of darch.
#
# Darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch.  If not, see <http://www.gnu.org/licenses/>.

#' darch example for the iris data set.
#' 
#' @details
#' For this example, we will use a simple three-layer DBN and backpropagation
#' without maxour or dropout for fine-tuning.
#' 
#' @name example.iris
#' @family darch examples
NULL

#' darch MNIST example, using both dropout and maxout.
#' 
#' @details
#' Since maxout requires linear activations, all activations have been changed
#' accordingly. You will notice an overall drop in convergence speed compared to
#' the basic MNIST example, but also a smaller difference between the accuracies
#' on the training and validation set.
#' 
#' We use a bigger second layer (400 neurons) and a maxout pool size of 4, which
#' will effectively result in 100 outputs for this layer, the same as in the
#' basic MNIST example.
#' 
#' A higher number of fine-tuning epochs and a bigger DBN is necessary to 
#' observe the true potential of dropout and maxout.
#' 
#' @name example.maxout
#' @family darch examples
NULL

#' darch example using the MNIST database of handwritten digits.
#' 
#' @details
#' A relatively small DBN (784, 100, and 10 neurons) is used to allow training
#' within a reasonable time. Increase the second number and/or add additional
#' layers to achieve a better training performance.
#' 
#' 5 Epochs of RBM pre-training and 20 epochs of backpropagation fine-tuning
#' are used on 1000 training samples.
#' 
#' @name example.mnist
#' @family darch examples
NULL

#' DBN XOR example.
#' 
#' @details
#' An example using the simplest problem not solvable by a standard perceptron: 
#' XOR.
#' 
#' The DBN uses three layers with 2, 2, and 1 neurons. Pre-training is generally
#' not as important for such small/simple problems, and it can even be
#' counterproductive and delay fine-tuning convergence. The learning rate is
#' chosen to be relatively high to achieve faster convergence. Since a sigmoid
#' activation function is used, higher learning rates are less problematic than,
#' e.g., for linear activations. Further increasing the learning rate may lead
#' to quicker convergence, but this effect reverses at higher values and then
#' leads to delayed convergence, or no convergence at all, for more complex
#' problems (try values higher than 10, for example). The same is true for the
#' momentum, which is kept at 90\% here to further increase convergence speed
#' (change finalMomentum to .5 to see the difference), something that is not
#' recommended for more complex problems.
#' 
#' Learning is stopped as soon as 100\% of input samples are correctly 
#' classified.
#' 
#' @name example.xor
#' @family darch examples
NULL

#' darch XOR example with nominal data.
#' 
#' @details
#' This time with nominal data, to demonstrate that darch can handle such data 
#' as well.
#' 
#' See the \code{\link{example.xor}} function for more details on the problem
#' itself.
#' 
#' @name example.xorNominal
#' @family darch examples
NULL