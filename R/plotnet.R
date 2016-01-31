#' @export
plot.DArch <- function(x, y = NULL, ..., type = "net")
{
  switch(type, net =
    {
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
    errorClass = createPlotErrorClass(x@stats, NULL, ...),
    time = createPlotTime(x@stats, NULL, ...),
    stop(paste0("Invalid type argument \"", type, "\"")))
}

#' @export
plotnet.DArch <- function(mod_in, ...)
{
  plot(mod_in, type = "net")
}