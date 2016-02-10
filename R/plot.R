#' Plot \code{\link{DArch}} statistics or structure.
#' 
#' This function provides different plots depending on the \code{type}
#' parameter:
#' 
#' \itemize{
#'  \item errorRaw. Prints the raw network error (e.g. MSE), this is the default
#'  \item errorClass. Prints the classification error
#'  \item time. Prints the times needed for each epoch
#'  \item net. Calls \code{\link{NeuralNetTools::plotnet}} to plot the network
#' }
#'
#' @param x \code{\link{DArch}} instance
#' @param y ignored
#' @param type Which type of plot to create, one of \code{errorRaw},
#'   \code{errorClass}, \code{time}, or \code{net}.
#' @export
plot.DArch <- function(x, y = NULL, ..., type = "errorRaw")
{
  switch(type,
    net = {
      if (!suppressMessages(requireNamespace("NeuralNetTools", quietly = T)))
      {
        stop("Package \"NeuralNetTools\" required for plotting DArch, aborting.")
      }
      
      mod_in <- c()
      struct <- c()
      
      for (i in 1:length(x@layers))
      {
        # TODO right order of weights? biases are at the end
        mod_in <- c(mod_in, x@layers[[i]][["weights"]])
        struct <- c(struct, nrow(x@layers[[i]][["weights"]])-1)
      }
      
      struct <- c(struct, ncol(x@layers[[length(x@layers)]][["weights"]]))
      
      NeuralNetTools::plotnet(mod_in, struct = struct)
    },
    errorRaw = createPlotErrorRaw(x@stats, NULL, ..., bestModelLine = x@epochs),
    # TODO error when no classification?
    errorClass =
      createPlotErrorClass(x@stats, NULL, ..., bestModelLine = x@epochs),
    time = createPlotTime(x@stats, NULL, ...),
    stop(paste0("Invalid type argument \"", type, "\"")))
}

createPlotErrorRaw <- function(stats, fileName = NULL, ...)
{
  epochs <- c(1:length(stats$times))
  
  # Plot for training and validation raw error
  writePlot(fileName, epochs,
    list(data = stats$dataErrors$raw, valid = stats$validErrors$raw),
    "Network error", "Epoch", "Error",
    legend=list(pos = "topright", labels = c("Training", "Validation")), ...)
}

createPlotErrorClass <- function(stats, fileName = NULL,
  plot.classificationErrorRange = 1, ...)
{  
  epochs <- c(1:length(stats$times))
  
  rangeY = range(stats$dataErrors$class, stats$validErrors$class)
  rangeY[2] <- rangeY[1] + (rangeY[2] - rangeY[1]) *
    plot.classificationErrorRange
  
  # Plot for training and validation classification error
  writePlot(fileName, epochs,
    list(data = stats$dataErrors$class, valid = stats$validErrors$class),
    "Classification error", "Epoch", "Error (%)",
    legend=list(pos="topright", labels=c("Training", "Validation")),
    rangeY=rangeY, ...)
}

createPlotTime <- function(stats, fileName = NULL, ...)
{
  epochs <- c(1:length(stats$times))
  
  # Plot for times
  writePlot(fileName, epochs, list(times=stats$times),
            "Runtime", "Epoch", "Time (sec)", ...)
}

writePlot <- function(fileName=NULL, x, y=list(), main, xlab, ylab, legend=NULL, rangeY=NULL, bestModelLine=0)
{
  rangeX <- range(x, finite=T)
  rangeY <- if (is.null(rangeY)) range(unlist(y), finite=T) else rangeY
  
  if (any(!is.finite(c(rangeX, rangeY))))
  {
    stop("Insufficient data, stopping.")
  }
  
  if (!is.null(fileName)) pdf(fileName)
  
  plot(rangeX, rangeY, type="n", main=main,
       xlab=xlab, ylab=ylab)
  
  lty <- 1
  for (yName in names(y))
  {
    if (length(x) == length(y[[yName]]))
    {
      lines(x, y[[yName]], type="l", lty=lty)
      lty <- lty + 1
    }
  }
  
  if (bestModelLine > 0 && bestModelLine < rangeX[2])
  {
    abline(v = bestModelLine)
  }
  
  if (!is.null(legend))
  {
    legend(legend$pos, legend=legend$label, lty=c(1:length(y)), xpd=T)
  }
  
  if (!is.null(fileName)) dev.off()
}