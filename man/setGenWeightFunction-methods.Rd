\docType{methods}
\name{setGenWeightFunction<-}
\alias{setGenWeightFunction}
\alias{setGenWeightFunction<-}
\alias{setGenWeightFunction<-,Net-method}
\title{Sets the function for generating weight matrices.}
\arguments{
  \item{net}{A instance of the class \code{\link{Net}}.}

  \item{value}{Object of the class \code{function}.}
}
\description{
  The function have to return a matrix with number of units
  in the lower layer as number of rows and number of units
  in the upper layer as the nubmer of columns.
}
\seealso{
  \code{\link{Net}}
}

