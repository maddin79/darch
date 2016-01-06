# Copyright (C) 2013-2015 Martin Drees
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

#' Minimize a differentiable multivariate function. 
#' 
#' This function is a direct translation from the Matlab source code of the 
#' minimize function from Carl Edward Rasmussen.
#' 
#' @details
#' Minimize a differentiable multivariate function.
#' 
#' Usage: [X, fX, i] <- minimize(X, f, length, P1, P2, P3, ... )
#' 
#' where the starting point is given by "X" (D by 1), and the function named in
#' the string "f", must return a function value and a vector of partial
#' derivatives of f wrt X, the "length" gives the length of the run: if it is
#' positive, it gives the maximum number of line searches, if negative its
#' absolute gives the maximum allowed number of function evaluations. You can
#' (optionally) give "length" a second component, which will indicate the
#' reduction in function value to be expected in the first line-search (defaults
#' to 1.0). The parameters P1, P2, P3, ... are passed on to the function f.
#' 
#' The function returns when either its length is up, or if no further progress
#' can be made (ie, we are at a (local) minimum, or so close that due to
#' numerical problems, we cannot get any closer). NOTE: If the function
#' terminates within a few iterations, it could be an indication that the
#' function values and derivatives are not consistent (i.e., there may be a bug in
#' the implementation of your "f" function). The function returns the found
#' solution "X", a vector of function values "fX" indicating the progress made
#' and "i" the number of iterations (line searches or function evaluations,
#' depending on the sign of "length") used.
#
#' The Polack-Ribiere flavour of conjugate gradients is used to compute search
#' directions, and a line search using quadratic and cubic polynomial
#' approximations and the Wolfe-Powell stopping criteria is used together with
#' the slope ratio method for guessing initial step sizes. Additionally a bunch
#' of checks are made to make sure that exploration is taking place and that
#' extrapolation will not be unboundedly large.
#
#' See also: checkgrad 
#
#' Copyright (C) 2001 - 2006 by Carl Edward Rasmussen (2006-09-08).
#' 
#' @param X Starting point. An Array of the weights.
#' @param f Function for calculating the function value and the 
#' partial derivatives
#' @param length Maximum number of line searches or maximum allowed number of 
#' function evaluations if negative
#' @param ... Additional Parameters for the function f
#' @usage minimize( X, f, length, ...)
#' @return The function returns the found solution "X", a vector of function 
#' values "fX" indicating the progress made and "i" the number of iterations 
#' (line searches or function evaluations, depending on the sign of "length") 
#' used.
#' 
#' @seealso \code{\link{DArch}},
#' \code{\link{minimizeAutoencoder}},
#' \code{\link{minimizeClassifier}}
#' 
#' @docType methods
#' @rdname minimize
#' @export
minimize <-function( X, f, length, ...)
{
  # Minimize a differentiable multivariate function. 
  #
  # Usage: [X, fX, i] <- minimize(X, f, length, P1, P2, P3, ... )
  #
  # where the starting point is given by "X" (D by 1), and the function named in
  # the string "f", must return a function value and a vector of partial
  # derivatives of f wrt X, the "length" gives the length of the run: if it is
  # positive, it gives the maximum number of line searches, if negative its
  # absolute gives the maximum allowed number of function evaluations. You can
  # (optionally) give "length" a second component, which will indicate the
  # reduction in function value to be expected in the first line-search (defaults
  # to 1.0). The parameters P1, P2, P3, ... are passed on to the function f.
  #
  # The function returns when either its length is up, or if no further progress
  # can be made (ie, we are at a (local) minimum, or so close that due to
  # numerical problems, we cannot get any closer). NOTE: If the function
  # terminates within a few iterations, it could be an indication that the
  # function values and derivatives are not consistent (ie, there may be a bug in
  # the implementation of your "f" function). The function returns the found
  # solution "X", a vector of function values "fX" indicating the progress made
  # and "i" the number of iterations (line searches or function evaluations,
  # depending on the sign of "length") used.
  #
  # The Polack-Ribiere flavour of conjugate gradients is used to compute search
  # directions, and a line search using quadratic and cubic polynomial
  # approximations and the Wolfe-Powell stopping criteria is used together with
  # the slope ratio method for guessing initial step sizes. Additionally a bunch
  # of checks are made to make sure that exploration is taking place and that
  # extrapolation will not be unboundedly large.
  #
  # See also: checkgrad 
  #
  # Copyright (C) 2001 - 2006 by Carl Edward Rasmussen (2006-09-08).
  
  INT <- 0.1    # don't reevaluate within 0.1 of the limit of the current bracket
  EXT <- 3.0                  # extrapolate maximum 3 times the current step-size
  MAX <- 20                         # max 20 function evaluations per line search
  RATIO <- 10                                       # maximum allowed slope ratio
  SIG <- 0.1 
  RHO <- SIG/2 # SIG and RHO are the constants controlling the Wolfe-
  # Powell conditions. SIG is the maximum allowed absolute ratio between
  # previous and new slopes (derivatives in the search direction), thus setting
  # SIG to low (positive) values forces higher precision in the line-searches.
  # RHO is the minimum allowed fraction of the expected (from the slope at the
  # initial point in the linesearch). Constants must satisfy 0 < RHO < SIG < 1.
  # Tuning of SIG (dep   } #ing on the nature of the function to be optimized) may
  # speed up the minimization it is probably not worth playing much with RHO.
  
  # The code falls naturally into 3 parts, after the initial line search is
  # started in the direction of steepest descent. 1) we first enter a while loop
  # which uses point 1 (p1) and (p2) to compute an extrapolation (p3), until we
  # have extrapolated far enough (Wolfe-Powell conditions). 2) if necessary, we
  # enter the second loop which takes p2, p3 and p4 chooses the subinterval
  # containing a (local) minimum, and interpolates it, unil an acceptable point
  # is found (Wolfe-Powell conditions). Note, that points are always maintained
  # in order p0 <<- p1 <<- p2 < p3 < p4. 3) compute a new search direction using
  # conjugate gradients (Polack-Ribiere flavour), or revert to steepest if there
  # was a problem in the previous line-search. Return the best value so far, if
  # two consecutive line-searches fail, or whenever we run out of function
  # evaluations or line-searches. During extrapolation, the "f" function may fail
  # either with an error or returning Nan or Inf, and minimize should handle this
  # gracefully.
  
  if(length(length) == 2){
    red<-length[2] 
    length<-length[1]
  }else{ 
    red<-1
  }
  
  if(length>0){
    S<-'Linesearch'
  }else{
    S<-'Function evaluation'    
  } 
  
  i <- 0                                            # zero the run length counter
  ls.failed <- 0                             # no previous line search has failed
  ret <- f( X, ...)          # get function value and gradient
  f0 <- if(!is.nan(ret[1])) ret[1] else Inf
  df0 <- ret[2:length(ret)]
  fX <- f0
  i <- i + (length<0)                                            # count epochs?!
  s <- -df0
  d0 <- matMult(t(-s), s)     # initial search direction (steepest) and slope
  x3 <- red/(1-d0)                                  # initial step is red/(|s|+1)
  
  while(i < abs(length)){                                      # while not finished
    i <- i + (length>0)                                      # count iterations?!
    
    X0 <- X 
    F0 <- f0 
    dF0 <- df0 # make a copy of current values
    
    if (length>0){
      M <- MAX
    }else{
      M <- min(MAX, -length-i) 
    }
    
    while(1){                             # keep extrapolating as long as necessary
      x2 <- 0 
      f2 <- f0 
      d2 <- d0 
      f3 <- f0 
      df3 <- df0
      success <- FALSE
      
      while(!success && M > 0){
        M <- M - 1 
        i <- i + (length<0)                         # count epochs?!
        ret <- f(X+x3*s, ...)
        f3 <- if(!is.nan(ret[1])) ret[1] else Inf
        df3 <- ret[2:length(ret)]
        
        if (is.infinite(f3) || any(is.nan(df3)+is.infinite(df3))){
          x3 <- (x2+x3)/2                                  # bisect and try again
        }else{
          success <- 1
        }                              
      }
      
      if (f3 < F0){
        X0 <- X+x3*s 
        F0 <- f3 
        dF0 <- df3    # keep best values
      }        
      
      d3 <- matMult(t(df3), s)                     # new slope
      
      if (d3 > SIG*d0 || f3 > f0+x3*RHO*d0 || M == 0){  # are we done extrapolating?
        break
      }
      
      # move point 2 to point 1
      x1 <- x2 
      f1 <- f2 
      d1 <- d2       
      # move point 3 to point 2
      x2 <- x3 
      f2 <- f3 
      d2 <- d3                        
      
      A <- 6*(f1-f2)+3*(d2+d1)*(x2-x1)                 # make cubic extrapolation
      B <- 3*(f2-f1)-(2*d1+d2)*(x2-x1)
      # num. error possible, ok!
      suppressWarnings(x3 <- x1-d1*(x2-x1)^2/(B+sqrt(B*B-A*d1*(x2-x1))))
      if (!is.double(x3) || is.nan(x3) || is.infinite(x3) || x3 < 0){ # num prob | wrong sign?
        x3 <- x2*EXT                                 # extrapolate maximum amount
      }else if(x3 > (x2*EXT)){                  # new point beyond extrapolation limit?
        x3 <- x2*EXT                                 # extrapolate maximum amount
      }else if(x3 < (x2+INT*(x2-x1))){         # new point too close to previous point?
        x3 <- x2+INT*(x2-x1)
      } 
    }                                                        # end extrapolation
    
    while((abs(d3) > -SIG*d0 || f3 > f0+x3*RHO*d0) && M > 0){  # keep interpolating
      if (d3 > 0 || f3 > f0+x3*RHO*d0){                         # choose subinterval
        # move point 3 to point 4
        x4 <- x3 
        f4 <- f3 
        d4 <- d3                      
      }else{
        # move point 3 to point 2
        x2 <- x3 
        f2 <- f3 
        d2 <- d3                     
      }
      
      if (f4 > f0){           
        x3 <- x2-(0.5*d2*(x4-x2)^2)/(f4-f2-d2*(x4-x2))  # quadratic interpolation
      }else{
        A <- 6*(f2-f4)/(x4-x2)+3*(d4+d2)                    # cubic interpolation
        B <- 3*(f4-f2)-(2*d2+d4)*(x4-x2)
        # num. error possible, ok!
        suppressWarnings(x3 <- x2+(sqrt(B*B-A*d2*(x4-x2)^2)-B)/A)
      }
      
      if (is.nan(x3) || is.infinite(x3)){
        x3 <- (x2+x4)/2               # if we had a numerical problem then bisect
      }
      
      x3 <- max(min(x3, x4-INT*(x4-x2)),x2+INT*(x4-x2))  # don't accept too close
      ret <- f(X+x3*s, ...)
      f3 <- ret[1]
      df3 <- ret[2:length(ret)]

      if (f3 < F0){
        # keep best values
        X0 <- X+x3*s 
        F0 <- f3 
        dF0 <- df3   
      }
      
      M <- M - 1 
      i <- i + (length<0)                             # count epochs?!
      d3 <- matMult(t(df3), s)                         # new slope
    }                                                        # end interpolation

    if (abs(d3) < -SIG*d0 && f3 < f0+x3*RHO*d0){          # if line search succeeded
      X <- X+x3*s 
      f0 <- f3 
      fX <- c(fX,f0)                     # update variables
      #print(paste(S, i, "Value",f0))
      s <- (matMult(t(df3), df3)-matMult(t(df0), df3))/(matMult(t(df0), df0))*s - df3   # Polack-Ribiere CG direction
      df0 <- df3                                               # swap derivatives
      d3 <- d0 
      d0 <- matMult(t(df0), s)
      
      if (d0 > 0){                                      # new slope must be negative
        s <- -df0 
        d0 <- matMult(-t(s), s)              # otherwise use steepest direction
      }
      
      x3 <- x3 * min(RATIO, d3/(d0-.Machine[["double.xmin"]]))          # slope ratio but max RATIO
      ls.failed <- 0                              # this line search did not fail
    }else{
      # restore best point so far
      X <- X0 
      f0 <- F0 
      df0 <- dF0 
      
      if (ls.failed || i > abs(length)){         # line search failed twice in a row
        break                             # or we ran out of time, so we give up
      } 
      s <- -df0 
      d0 <- matMult(-t(s), s)                            # try steepest
      x3 <- 1/(1-d0)                     
      ls.failed <- 1                                    # this line search failed
    } #
  } #
  return(list(X,fX,i))
}
