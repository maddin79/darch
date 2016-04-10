# Copyright (C) 2013-2016 Martin Drees
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

# TODO name, documentation

#' Benchmarking wrapper for \code{darch}
#' 
#' Simple benchmarking function which wraps around the \code{\link{darch}}
#' function for users who can't or don't want to use the caret package for
#' benchmarking. This function requires the foreach package to work, and will
#' perform parallel benchmarks if an appropriate backend was registered
#' beforehand.
#' 
#' @param ... Parameters to the  \code{\link{darch}} function.
#' @param bench.times How many benchmark runs to perform
#' @param bench.save Whether to save benchmarking results to a directory
#' @param bench.path Path (relative or absolute) including directory where
#'   benchmark results are saved if \code{bench.save} is true
#' @param bench.continue Whether the benchmark is to be continued from an
#'   earlier run (will look for results of an earlier run in the specified
#'   directory)
#' @param bench.delete Whether to delete the contents of the given directory if
#'   \code{bench.continue} is \code{FALSE}. Caution: This will attempt to delete
#'   ALL files in the given directory, use at your own risk!
#' @param bench.plots Whether to create plots for error rates and run times,
#'   will be ignored if \code{bench.save} is \code{FALSE}
#' @param output.capture Whether to capture R output in \code{.Rout} files in
#'   the given directory. This is the only way of gaining access to the R
#'   output since the foreach loop will not print anything to the console. Will
#'   be ignored if \code{bench.save} is \code{FALSE}
#' @param plot.classificationErrorRange Allows specification of the error range
#'   for the classification error to make the plot more meaningful. A value of
#'   \code{0.5}, for example, would limit the values on the y-axis to 50% of 
#'   the complete error range.
#' @inheritParams darch
#' @export
darchBench <- function(...,
  bench.times = 1,
  bench.save = F,
  bench.path = "./darch.benchmark",
  bench.continue = T,
  bench.delete = F,
  #bench.registerParallelBackend = T, TODO
  bench.plots = bench.save,
  output.capture = bench.save,
  plot.classificationErrorRange = 1.,
  logLevel = NULL
)
{
  oldLogLevel <- futile.logger::flog.threshold()
  on.exit(futile.logger::flog.threshold(oldLogLevel))
  setLogLevel(logLevel)
  
  indexStart <- prepareBenchmarkDirectory(bench.path, bench.save,
    bench.continue, bench.delete)
  
  darchList <- performBenchmark(bench.path, bench.times, indexStart,
    bench.save = bench.save, output.capture = output.capture, ...)

  darchList
}

# Make sure the benchmark directory is ready
prepareBenchmarkDirectory <- function(name, save = F, continue = F, delete = F,
                                       fileType=c(".net", ".Rout"))
{
  if (!save) return(1)
  
  # TODO windows?
  if (file.exists(paste0(name, "/")) && !continue)
  {
    if (!delete)
    {
      stop(futile.logger::flog.error(paste("Benchmark directory \"%s\" already",
      "exists. Please delete the directory manually or pass bench.delete=T",
      "to automatically delete the contents of the directory (use at your own",
      "risk!)."), name))
    }
    else
    {
      futile.logger::flog.warn(
        "Deleting contents of directory %s in 5 seconds...", name)
      
      Sys.sleep(time = 5)
      
      # delete files, but don't delete empty directory
      if (length(dir(name)) > 0 ) file.remove(paste(name, dir(name), sep = "/"))
      
      futile.logger::flog.info(
        "Successfully deleted contents of directory %s", name)
    }
  }
  
  suppressWarnings(dir.create(name, recursive = T))
  
  if (!file.exists(paste0(name, "/")))
  {
    stop(futile.logger::flog.error(
      "Benchmark directory could not be created."))
  }
  
  indexStart <- 1
  
  if (continue)
  {
    indexStart <- max(unlist(sapply(fileType, FUN = function(x)
    {
      fileName <- tail(dir(name, pattern = paste0(".*_\\d{3}", x)), n = 1)
      as.numeric(substr(tail(strsplit((if (length(fileName) > 0) fileName
        else "none_000"), "_")[[1]], n = 1), 1, 3)) + 1
    })))
  }
  
  indexStart
}

performBenchmark <- function(name, iterations = 1, indexStart = 1, ...,
                              bench.save = F, output.capture = F)
{ 
  if (!suppressMessages(requireNamespace("foreach", quietly = T)))
  {
    stop(futile.logger::flog.error(
      "\"foreach\" package required when using darchBench()."))
  }
  
  futile.logger::flog.info("Starting %d training runs...",
                           iterations)
  
  i <- indexStart
  resultList <-
    foreach::`%dopar%`(foreach::foreach(i = indexStart:(indexStart + iterations - 1)),
  {
    fileName <- paste0(name, "/", basename(name), "_", formatC(i, 2,flag = "0"))
    outputFile <- if (bench.save && output.capture) 
      paste0(fileName, ".Rout") else NULL

    capture.output(darch <- darch(..., autosave.location = fileName,
      autosave = T), file = outputFile)
    
    darch
  })
  
  futile.logger::flog.info("Finished %d training runs", iterations)
  
  resultList
}

createAllPlots <- function(name, stats, raw.ylab = "Error", bestModelLine = 0,
  ...)
{
  filePrefix <- paste0(name, "/", basename(name))
  
  createPlotErrorRaw(stats, paste0(filePrefix, "_error_raw.pdf"),
    ylab = raw.ylab, bestModelLine = bestModelLine, ...)
  createPlotErrorClass(stats, paste0(filePrefix, "_error_class.pdf"),
    bestModelLine = bestModelLine, ...)
  createPlotTime(stats, paste0(filePrefix, "_time.pdf"), ...)
}