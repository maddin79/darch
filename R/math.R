#' Helper function for matrix multiplication to keep gputools an optional
#' dependency.
#' 
#' The global option darch2.gputools controls whether gputools is used, it is
#' automatically set on package load only, and can be changed manually at any
#' time.
#' 
#' TODO add gputools to 'suggests' in DESCRIPTION file
#'
#' @param m1 First matrix
#' @param m2 Second matrix
#' 
#' @export
#' @rdname darch-math
matMul <- function(m1, m2)
{
  if (getOption("darch2.gputools", default=F))
  {
    ret <- gpuMatMult(m1, m2)
  }
  else
  {
    ret <- m1 %*% m2
  }
  
  return(ret)
}