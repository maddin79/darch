#' Saves a DArch network
#' 
#' Saves the DArch object to the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details If the field \code{ff} of the DArch object is \code{TRUE} then
#' the weights are saved in seperate ff-files named by the parameter \code{name}
#' plus the string "-W" and the number of the layer.
#' In the same way the weights from the RBM's of the DArch are saved, but only
#' if the parameter \code{saveRBM} is \code{TRUE}. For more information about 
#' the how the weights and biases from the RBM's are saved see 
#' \code{\link{saveRBMFFWeights}}.
#' If the parameter \code{saveRBM} is \code{FALSE} the field \code{rbmList} of 
#' the DArch object ist overwritten by an empty list.
#' 
#' @param darch A instance of the class \code{\link{DArch}}.
#' @param name The name for the file. Default value is "darch".
#' @param saveRBM Boolean value to indicate if the RBM's are saved.
#' 
#' @usage saveDArch(darch,name="darch",saveRBM=TRUE)
#' 
#' @seealso \code{\link{loadDArch}}, \code{\link{saveRBMFFWeights}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname saveDArch-methods
setGeneric(
  name="saveDArch",
  def=function(darch,name="darch",saveRBM=TRUE){standardGeneric("saveDArch")}
)

#' @rdname saveDArch-methods
#' @aliases saveDArch,DArch-method
setMethod(
  f="saveDArch",
  signature="DArch",
  definition=function(darch,name="darch",saveRBM=TRUE){
    if(!saveRBM){
      darch@rbmList <- list()
    }
    if(darch@ff){
      if(saveRBM){
        for(i in 1:length(darch@rbmList)){
          saveRBMFFWeights(darch@rbmList[[i]],paste(name,"RBM",i,sep=""))
        }
      }
      
      for(i in 1:length(darch@layers)){
        w <- darch@layers[[i]][[1]]
        ffsave(w,file=paste(name,"-W",i,sep=""))
      }
    }
    save(darch,saveRBM,file=paste(name,".net",sep=""))
  }
)