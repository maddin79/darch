# TODO documentation
testDArch <- function(darch, data, targets, dataType, isClass)
{
  execOut <- darch@executeFunction(darch, data)
  
  tError <- darch@errorFunction(targets, execOut)
  class <- -1
  if (isClass)
  {
    rows <- nrow(targets)
    cols <- ncol(targets)
    outRaw <- execOut
    execOut <-
      (if (cols > 1) diag(cols)[max.col(execOut, ties.method="first"),]
       else (execOut>.5)*1)
    class <- sum(rowMeans(execOut==targets)<1)/rows*100
    flog.info(paste0("Classification error on ", dataType, " ",
                     round(class, 2), "%"))
  }
  
  flog.info(paste(dataType,tError[[1]],tError[[2]]))
  
  c(tError[[2]],class)
}