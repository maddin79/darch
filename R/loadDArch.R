#' Loads a DArch network
#' 
#' Loads the DArch object from the filename given through the parameter \code{name}
#' plus the ending ".net".
#' 
#' @details Make sure when you load a DArch object that every file written by 
#' the \code{\link{saveDArch}}-Funktion, specially when the parameter \code{ff}
#' of the saved DArch object is \code{TRUE}, are in the working directory.
#' 
#' @param name The name of the file without the ending ".net".
#' 
#' @return \code{darch} - The loaded deep architecture
#' 
#' @usage loadDArch(name="darch")
#' 
#' @seealso \code{\link{saveDArch}}, \code{\link{loadRBMFFWeights}}
#' 
#' @include darch.R
#' 
#' @export
#' @docType methods
#' @rdname loadDArch
loadDArch <- function(name="darch"){
    load(paste(name,".net",sep=""))
    if(darch@ff){
      if(saveRBM){
        for(i in 1:length(darch@rbmList)){
          darch@rbmList[[i]] <- loadRBMFFWeights(darch@rbmList[[i]],paste(name,"RBM",i,sep=""))
        }
      }
      w <- 1
      for(i in 1:length(darch@layers)){
        ffload(paste(name,"-W",i,sep=""),overwrite=TRUE)
        darch@layers[[i]][[1]] <- w			
      }
    }
    return(darch)
  }
