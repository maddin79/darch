#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname resetExecOutput-methods
setGeneric("resetExecOutput",function(darch){standardGeneric("resetExecOutput")})

#' @rdname resetExecOutput-methods
#' @aliases resetExecOutput,DArch-method
setMethod(
  f="resetExecOutput",
  signature="DArch",
  definition=function(darch){
    darch@executeOutput <- list()
    return(darch)
  }
)

#' BEARBEITEN
#' 
#' BEARBEITEN
#'
#' @param darch A instance of the class \code{\link{DArch}}.
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname resetDArch-methods
setGeneric("resetDArch",function(darch){standardGeneric("resetDArch")})

#' @rdname resetDArch-methods
#' @aliases resetDArch,DArch-method
setMethod(
  f="resetDArch",
  signature="DArch",
  definition=function(darch){
    
    return(darch)
  }
)