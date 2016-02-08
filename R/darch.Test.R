# TODO documentation
testDArch <- function(darch, data, targets, dataType, isClass)
{
  execOut <- darch@executeFunction(darch, data)
  
  tError <- darch@errorFunction(targets, execOut)
  classError <- NA
  numIncorrect <- NA
  if (isClass)
  {
    rows <- nrow(targets)
    cols <- ncol(targets)
    outRaw <- execOut
    execOut <-
      (if (cols > 1) diag(cols)[max.col(execOut, ties.method="first"),]
       else (execOut>.5)*1)
    numIncorrect <- sum(rowMeans(execOut==targets)<1)
    classError <- numIncorrect/rows*100
    futile.logger::flog.info(paste0("Classification error on ", dataType, " ",
                     round(classError, 2), "%"))
  }
  
  futile.logger::flog.info(paste(dataType, tError[[1]], tError[[2]]))
  
  c(tError[[2]], classError, numIncorrect)
}