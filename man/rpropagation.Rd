\docType{methods}
\name{rpropagation}
\alias{rpropagation}
\title{Resilient-Backpropgation training for deep architectures.}
\usage{
rpropagation(darch,trainData,targetData,epoch,method="iRprop+",
decFact=0.5,incFact=1.2,weightDecay=0,
initDelta=0.0125,minDelta=0.000001,maxDelta=50)
}
\arguments{
  \item{darch}{The deep architecture to train}

  \item{trainData}{The training data}

  \item{targetData}{The expected output for the training
  data}

  \item{epoch}{The number of training iterations}

  \item{method}{The method for the training. Default is
  "iRprop+"}

  \item{decFact}{Decreasing factor for the training.
  Default is \code{0.5}.}

  \item{incFact}{Increasing factor for the training Default
  is \code{1.2}.}

  \item{weightDecay}{Weight decay for the training. Default
  is \code{0}}

  \item{initDelta}{Initialisation value for the update.
  Default is \code{0.0125}.}

  \item{minDelta}{Lower bound for step size. Default is
  \code{0.000001}}

  \item{maxDelta}{Upper bound for step size. Default is
  \code{50}}
}
\value{
\code{darch} - The trained deep architecture
}
\description{
The function traines a deep architecture with the resilient
backpropagation algorithm. It is able to use four different
types of training (see details). For details of the
resilient backpropagation algorith see the references.
}
\details{
The code for the calculation of the weight change is a
translation from the matlab code from the Rprop
Optimization Toolbox implemented by R. Calandra (see
References).

Copyright (c) 2011, Roberto Calandra. All rights reserved.
Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met: 1. Redistributions of source
code must retain the above copyright notice, this list of
conditions and the following disclaimer.  2.
Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.  3. The names of its
contributors may be used to endorse or promote products
derived from this software without specific prior written
permission.  4. If used in any scientific publications, the
publication has to refer specifically to the work published
on this webpage.

This software is provided by us "as is" and any express or
implied warranties, including, but not limited to, the
implied warranties of merchantability and fitness for
particular purpose are disclaimed. In no event shall the
copyright holders or any contributor be liable for any
direct, indirect, incidental, special, exemplary, or
consequential damages however caused and on any theory of
liability whether in contract, strict liability or tort
arising in any way out of the use of this software, even if
advised of the possibility of such damage.
}
\references{
M. Riedmiller, H. Braun. A direct adaptive method for
faster backpropagation learning: The RPROP algorithm. In
Proceedings of the IEEE International Conference on Neural
Networks, pp 586-591. IEEE Press, 1993.

C. Igel , M. Huesken.  Improving the Rprop Learning
Algorithm, Proceedings of the Second International
Symposium on Neural Computation, NC 2000, ICSC Academic
Press, Canada/Switzerland, pp. 115-121., 2000.

Kohavi, R., A Study of Cross-Validation and Bootstrap for
Accuracy Estimation and Model Selection, Proceedings of the
14th Int. Joint Conference on Artificial Intelligence 2, S.
1137-1143, Morgan Kaufmann, Morgan Kaufmann Publishers
Inc., San Francisco, CA, USA, 1995.
}
\seealso{
\code{\link{DArch}}
}

