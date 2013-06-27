\docType{methods}
\name{minimizeClassifier}
\alias{minimizeClassifier}
\title{Fine-Tune function for a classification net}
\usage{
  minimizeClassifier(darch, trainData, targetData, epoch,
    length, switchLayers)
}
\arguments{
  \item{darch}{BEARBEITEN}

  \item{trainData}{BEARBEITEN}

  \item{targetData}{BEARBEITEN}

  \item{epoch}{BEARBEITEN}

  \item{...}{BEARBEITEN}
}
\value{
  BEARBEITEN
}
\description{
  This function is build on the basis of the code from G.
  Hinton et. al.
  (http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html
  - last visit 06.03.2013) for the fine tuning of an
  deep-belief-net. The original code is located in the
  files 'backpropclassify.m', 'CG_MNIST.m' and
  'CG_CLASSIFY_INIT.m'. It implements the fine tuning for a
  classification net with backpropagation using a direct
  translation of the minimize function from C. Rassmussen
  (available at
  http://www.gatsby.ucl.ac.uk/~edward/code/minimize/ - last
  visit 06.03.2013) to R.
}
\seealso{
  \code{\link{DArch}}
}

