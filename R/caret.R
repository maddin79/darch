#' Creates a custom caret model for \code{darch}.
#' 
#' TODO documentation, link to caret custom models page
#'
#' @param params \code{\link{data.frame}} of parameters or \code{NULL} to use a
#'   simple default (bp.learnRate)
#' @param grid \code{\link{data.frame}} containing a grid of parameter
#'   combinations or \code{NULL} to use a simple default
#'
#' @export
darchModelInfo <- function(params = NULL, grid = NULL)
{
  r <- list(type = c("Classification", "Regression"),
            library = "darch")
            #loop = NULL)
  
  if (is.null(params))
  {
    params <- data.frame(parameter = c("bp.learnRate"),
                         class = c("numeric"),
                         label = c("Learn rate"))
  }
  
  r$parameters <- params
  
  if (is.null(grid))
  {
    grid <- function(x, y, len = NULL, search = "grid")
    {
      if (search == "grid")
      {
        out <- data.frame(darch.learnRate = seq.int(.1,5,.1)[1:len])
      }
      else
      {
        out <- data.frame(darch.learnRate = runif(len, .01, 2))
      }
      
      out
    }
  }
  
  r$grid <- grid
  
  r$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...)
  {
    darch(x, y, paramsList=as.list(param), darch.isClass = !classProbs, ...)
  }
  
  r$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  {
    # TODO switch between class and raw when not dealing with classification?
    predict(modelFit, newdata = newdata, type = "class")
  }
  
  r$prob <- function(darch, newdata, preProc = NULL, submodels = NULL)
  {
    predict(darch, newdata = newdata, type = "raw")
  }
  
  r$levels <- function(darch)
  {
    darch@dataSet@parameters$ylevels
  }
  
  r
}

#' Wrapper for \code{\link[caret]{contr.ltfr}}
#' 
#' Simply redirects the call to \code{\link[caret]{contr.ltfr}}, this is done
#' to avoid errors when the caret namespace is not attached.
#' 
#' @param ... \code{\link[caret]{contr.ltfr}} parameters.
#' @export
#' @keywords internal
contr.ltfr <- function(...)
{
  caret::contr.ltfr(...)
}