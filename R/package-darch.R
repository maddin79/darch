#' The darch-package implements Deep-Architecture-Networks and Restricted-Bolzmann-Machines.
#'
#' The creation of this package is motivated by the papers from G. Hinton et. al.
#' from 2006 (see references for details) and from the matlab source code
#' developed in this context. 
#' This package provides the possibility to generate deep architecture networks 
#' (darch) like the deep belief networks from Hinton et. al.. The deep architectures can
#' then be trained with the contrastive divergence method.
#' After this pre-training it can be fine tuned with several lerning methods like backpropagation,
#' resilient backpropagtion and conjugate gradients.
#'
#' \tabular{ll}{
#' Package: \tab darch\cr
#' Type: \tab Package\cr
#' Version: \tab 0.9\cr
#' Date: \tab 2013-04-17\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @author Martin Drees \email{mdrees@@stud.fh-dortmund.de}
#'
#' Maintainer: Martin Drees \email{mdrees@@stud.fh-dortmund.de}
#' @name darch
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
#' @examples
#' \dontrun{
#' # This example shows how to build and train a small deep belief network
#' # for the XOR-Problem.
#' 
#' # Generating the datasets
#' inputs <- matrix(c(0,0,0,1,1,0,1,1),ncol=2,byrow=TRUE)
#' outputs <- matrix(c(0,1,1,0),nrow=4)
#' 
#' # Generating the darch
#' darch <- newDArch(c(2,4,1),batchSize=2)
#' 
#' # Pre-Train the darch
#' darch <- preTrainDArch(darch,inputs,maxEpoch=1000)
#' 
#' # Setting and running the Fine-Tune function 
#' setFineTuneFunction(darch) <- backpropagation
#' darch <- fineTuneDArch(darch,inputs,outputs,maxEpoch=1000)
#' 
#' # Running the darch
#' darch <- darch <- getExecuteFunction(darch)(darch,inputs)
#' outputs <- getExecOutputs(darch)
#' cat(outputs[[length(outputs)]])}
#' 
#' @rdname darch-package
NULL