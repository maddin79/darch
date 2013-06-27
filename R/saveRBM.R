#' Saves a RBM network
#' 
#' Saves the RBM object to the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details If the field \code{ff} of the RBM object is \code{TRUE} then
#' the weights are saved in seperate ff-files through the funktion 
#' \code{\link{saveRBMFFWeights}}.
#' 
#' @param rbm A instance of the class \code{\link{RBM}}.
#' @param name The name for the file. Default value is "rbm".
#' 
#' @usage saveRBM(rbm,name="rbm")
#' 
#' @seealso \code{\link{loadRBM}}, \code{\link{saveRBMFFWeights}} \code{\link{loadRBMFFWeights}}
#' 
#' @include rbm.R
#' 
#' @export
#' @docType methods
#' @rdname saveRBM-methods
setGeneric(
  name="saveRBM",
  def=function(rbm,name="rbm"){standardGeneric("saveRBM")}
)

#' @rdname saveRBM-methods
#' @aliases saveRBM,RBM-method
setMethod(
  f="saveRBM",
  signature="RBM",
  definition=function(rbm,name="rbm"){
    if(getFF(rbm)){
      saveRBMFFWeights(rbm,name)		
    }
    save(rbm,file=paste(name,".net",sep=""))	
  }
)