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
    errorRaw = createPlotErrorRaw(x@stats, NULL, ...),
    # TODO error when no classification?
    errorClass = createPlotErrorClass(x@stats, NULL, ...),
    time = createPlotTime(x@stats, NULL, ...),
    stop(paste0("Invalid type argument \"", type, "\"")))
}