#' Helper function for matrix multiplication to keep gputools an optional
#' dependency.
#' 
#' TODO add gputools to 'suggests' in DESCRIPTION file
#'
#' @param m1 First matrix
#' @param m2 Second matrix
#' 
#' @export
#' @rdname darch-math
gpuMatMult <- function(m1, m2)
{
  return(m1 %*% m2)
}