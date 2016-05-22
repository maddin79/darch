# Copyright (C) 2015-2016 Johannes Rueckert
#
# This file is part of darch.
#
# darch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# darch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with darch. If not, see <http://www.gnu.org/licenses/>.

#' Plot \code{\linkS4class{DArch}} statistics or structure.
#' 
#' This function provides different plots depending on the \code{type}
#' parameter:
#' 
#' \itemize{
#'  \item raw. Plots the raw network error (e.g. MSE), this is the default
#'  \item class. Plots the classification error
#'  \item time. Plots the times needed for each epoch
#'  \item momentum. Plots the momentum ramp
#'  \item net. Calls \code{\link[NeuralNetTools]{plotnet}} to plot the network
#' }
#'
#' @param x \code{\linkS4class{DArch}} instance.
#' @param y See \code{type}.
#' @param ... Additional parameters, passed to plotting functions.
#' @param type Which type of plot to create, one of \code{raw},
#'   \code{class}, \code{time}, \code{momentum}, and \code{net}.
#' @return The plotted graph.
#' @examples
#' \dontrun{
#' data(iris)
#' model <- darch(Species ~ ., iris)
#' plot(model)
#' plot(model, "class")
#' plot(model, "time")
#' plot(model, "momentum")
#' plot(model, "net")
#' }
#' @family darch interface functions
#' @export
plot.DArch <- function(x, y = "raw", ..., type = y)
{
  switch(type,
    net = {
      if (!suppressMessages(requireNamespace("NeuralNetTools", quietly = T)))
      {
        stop(futile.logger::flog.error(
          "Package \"NeuralNetTools\" required for plotting DArch, aborting."))
      }
      
      mod_in <- c()
      struct <- c()
      
      for (i in 1:length(x@layers))
      {
        # TODO right order of weights? biases are at the end
        mod_in <- c(mod_in, x@layers[[i]][["weights"]])
        struct <- c(struct, nrow(x@layers[[i]][["weights"]]) - 1)
      }
      
      struct <- c(struct, ncol(x@layers[[length(x@layers)]][["weights"]]))
      
      NeuralNetTools::plotnet(mod_in, struct = struct)
    },
    raw = createPlotErrorRaw(x@stats, NULL, ..., bestModelLine = x@epochs,
      ylab = getErrorFunctionName(getParameter(".darch.errorFunction", net = x))),
    # TODO error when no classification?
    class =
      createPlotErrorClass(x@stats, NULL, ..., bestModelLine = x@epochs),
    time = createPlotTime(x@stats, NULL, ...),
    momentum = createPlotMomentum(x, ...),
    stop(futile.logger::flog.error("Invalid type argument \"%s\"", type)))
}

createPlotErrorRaw <- function(stats, fileName = NULL, ..., ylab = "Error",
  bestModelLine = 0)
{
  epochs <- c(1:length(stats$times))
  
  # Plot for training and validation raw error
  writePlot(fileName, epochs,
    list(train = stats$trainErrors$raw, valid = stats$validErrors$raw),
    "Network error", "Epoch", ylab,
    legend = list(display = T, title = "Dataset",
    labels = c("Training", "Validation")),
    bestModelLine = bestModelLine)
}

# @param plot.classificationErrorRange Allows specification of the error range
#   for the classification error to make the plot more meaningful. A value of
#   \code{0.5}, for example, would limit the values on the y-axis to 50% of 
#   the complete error range.
# TODO move error range to more general parameter applicable to all plots
createPlotErrorClass <- function(stats, fileName = NULL,
  plot.classificationErrorRange = 1, bestModelLine = 0, ...)
{  
  epochs <- c(1:length(stats$times))
  
  rangeY = range(stats$trainErrors$class, stats$validErrors$class)
  rangeY[2] <- rangeY[1] + (rangeY[2] - rangeY[1]) *
    plot.classificationErrorRange
  
  # Plot for training and validation classification error
  writePlot(fileName, epochs,
    list(train = stats$trainErrors$class, valid = stats$validErrors$class),
    "Classification error", "Epoch", "Error (%)",
    legend = list(display = T, title = "Dataset",
    labels = c("Training", "Validation")), rangeY = rangeY, bestModelLine)
}

createPlotTime <- function(stats, fileName = NULL, ...)
{
  epochs <- c(1:length(stats$times))
  
  # Plot for times
  writePlot(fileName, epochs, list(times = stats$times),
            "Training time", "Epoch", "Time (sec)")
}

createPlotMomentum <- function(darch, fileName = NULL, bestModelLine = 0, ...)
{
  epochs <- c(1:length(darch@stats$times))
  
  y <- sapply(epochs, FUN = function(x)
    {
      calculateMomentum(getParameter(".darch.initialMomentum"),
        getParameter(".darch.finalMomentum"),
        getParameter(".darch.momentumRampLength"),
        getParameter(".darch.epochsScheduled"), x)
    })
  
  writePlot(fileName, epochs, list(momentum = y), "Momentum Ramp", "Epoch",
    "Momentum", bestModelLine = bestModelLine)
}

writePlot <- function(fileName = NULL, x, y = list(), main, xlab, ylab,
  legend = NULL, rangeY = NULL, bestModelLine = 0)
{
  rangeX <- range(x, finite = T)
  rangeY <- if (is.null(rangeY)) range(unlist(y), finite = T) else rangeY
  
  if (any(!is.finite(c(rangeX, rangeY))))
  {
    # TODO more informative message
    stop(futile.logger::flog.error("Insufficient data, stopping."))
  }
  
  df <- data.frame(x)
  for (yName in names(y))
  {
    if (length(x) == length(y[[yName]]))
    {
      label <-
        if (is.null(legend)) yName else legend$label[which(names(y) == yName)]
      df[[label]] <- y[[yName]]
    }
  }
  
  df <- reshape2::melt(df, id.vars = 1)
  
  gp <- (ggplot(data = df, aes_string(x = "x", y = "value", group = "variable",
    linetype = "variable")) + geom_line() + coord_cartesian(ylim = rangeY)
    + ylab(ylab) + xlab(xlab))
  
  if (!is.null(legend))
  {
    gp <- gp + scale_linetype_discrete(name = legend$title)
  }
  else
  {
    gp <- gp + theme(legend.position = "none")
  }
  
  if (bestModelLine > 0)
  {
    if (bestModelLine < rangeX[2])
    {
      gp <- gp + geom_vline(xintercept = bestModelLine, linetype = "longdash",
                            colour = "red")
    }
    
    iterations <- sprintf("best iteration = %s", bestModelLine)
    trainError <- sprintf(", train error = %.3f", y[["train"]][bestModelLine])
    validError <- ""
    
    if (length(levels(df$variable)) > 1)
    {
      validError <- sprintf(", valid error = %.3f", y[["valid"]][bestModelLine])
    }
    
    mainExpression <- sprintf(
      "atop(\"%s\", atop(italic(\"%s%s%s\"), \"\"))", main,
      iterations, validError, trainError)
    main <- parse(file = NULL, text = mainExpression)
  }
  
  gp <- gp + ggtitle(main) + theme(text = element_text(size=18))
  
  if (!is.null(fileName))
  {
    ggsave(fileName, gp, width = 7, height = 7,  dpi = 120)
  }
  else gp
}