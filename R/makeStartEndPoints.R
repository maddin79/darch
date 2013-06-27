#' Makes start- and end-points for the batches.
#' 
#' The start- and end-points are used for dividing the data into batches. 
#' 
#' @details If the data is not divedable by the \code{batchSize} the last batch 
#' will contain the rest of the data. 
#' The function returns a list with in which the first entry is a list with the
#' values for the start and end points for reading the data matrix. The second
#' entry is the number of batches. 
#' 
#' @param batchSize Desired batch size
#' @param numRows Number of rows of the data
#' 
#' @seealso \code{\link{Net}}
#' 
#' @export
#' @docType methods
#' @rdname makeStartEndPoints
#' @include net.R
makeStartEndPoints <- function(batchSize,numRows){
    numBatches <- ceiling(numRows/batchSize)
    batchValues <- list()
    batchValues[[1]] <- 0
    for(n in 2:(numBatches)){
      batchValues[[n]] <- (n-1)*batchSize 
    }
    
    batchValues[[length(batchValues)+1]] <- numRows
    
    return(list(batchValues,numBatches))
  }
