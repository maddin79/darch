#' Execute the darch
#' 
#' Runs the darch in a feed forward manner and saves the 
#' generated outputs for every layer in the list
#' \code{executeOutput} from the darch.
#' To get the outputs call
#' 
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param data The input data to execute the darch on. 
#' @return The DArch object with the calculated outputs
#' 
#' @seealso \code{\link{DArch}}
#' 
#' @docType methods
#' @rdname runDArch
#' @include darch.R
#' @export
runDArch <- function(darch,data){
  darch <- resetExecOutput(darch)
  layers <- getLayers(darch)
  numRows <- dim(data)[1]	
  for(i in 1:length(layers)){
    data <- cbind(data,rep(1,numRows))
    ret <- layers[[i]][[2]](data[],layers[[i]][[1]][])
    data <- ret[[1]]
    darch <- addExecOutput(darch,data)
  }
  return(darch)
}