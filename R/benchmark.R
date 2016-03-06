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
  
  if (bench.continue)
  {
    darchList <- c(loadAllDArch(bench.path), darchList)
  }
  
  stats <- aggregateStatistics(darchList)
  
  if (bench.save)
  {
    writeStatistics(bench.path, stats)
    
    if (bench.plots) createAllPlots(bench.path, stats$mean,
      plot.classificationErrorRange = plot.classificationErrorRange,
      raw.ylab = getErrorFunctionName(darchList[[1]]@errorFunction), ...)
  }
  
  c(darchList, list("stats" = stats))
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
        else "none_001"), "_")[[1]], n = 1), 1, 3)) + 1
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

    capture.output(darch <- darch(..., autosave.location = fileName),
                   file = outputFile)
    
    darch
  })
  
  futile.logger::flog.info("Finished %d training runs", iterations)
  
  resultList
}

writeStatistics <- function(name, stats)
{
  fileName <- paste0(name, "/", basename(name), ".stats")
  
  assign(basename(name), stats)
  
  save(list = basename(name), file = fileName)
}

aggregateStatistics <- function(darchList)
{
  statsMean <- NULL
  stats <- list()
  
  # calculate means
  for (i in 1:length(darchList))
  {
    stats[[i]] <- darchList[[i]]@stats
    statsMean <- recursiveApplyLists(statsMean, darchList[[i]]@stats, 0, NULL,
                                     recursive.mean, n = length(darchList))
  }
  
  statsDeviation <- NULL
  
  # standard deviations
  for (i in 1:length(darchList))
  {
    statsDeviation <- recursiveApplyLists(statsDeviation, darchList[[i]]@stats,
      0, statsMean, recursive.variance, n = length(darchList))
  }
  
  statsDeviation <- recursiveApplyLists(NULL, statsDeviation, 0, NULL,
                                        recursive.sqrt)
  
  list("all" = stats, "mean" = statsMean, "deviation" = statsDeviation)
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

loadAllDArch <- function(dirName)
{
  files <- dir(dirName, pattern = "*.net")
  darchList <- vector(mode = "list", length = length(files))
  
  if (length(files) > 0)
  {
    # load DArch objects and extract stats
    for (i in 1:length(files))
    {
      darch <- loadDArch(paste0(dirName, "/",
                                substr(files[i], 1, nchar(files[i]) - 4)))
      
      darchList[[i]] <- darch
    }
  }
  
  darchList
}

recursiveApplyLists <- function(l1 = NULL, l2, default = 0,
  additionalList = NULL, func, ...)
{
  if (is.null(l1))
  {
    l1 <- if (is.list(l2)) list() else default
  }
  
  if (!is.list(l2))
  {
    # Try to add elements and return l2 if it fails
    return(suppressWarnings(tryCatch({
        func(l1, l2, additionalList, ...)
      },
      error = function(e){
        l2
      })))
  }
  
  if (length(l2) == 0)
  {
    return(l2)
  }
  
  indices <- if (is.null(names(l2))) c(1:length(l2)) else names(l2)
  
  for (i in indices)
  {
    l1Current <- if (is.numeric(i) && length(l1) < i) NULL else l1[[i]]
    additionalListCurrent <-
      (if (is.list(additionalList)) additionalList[[i]] else NULL)
    l2[[i]] <-
      recursiveApplyLists(l1Current, l2[[i]], default,
                          additionalListCurrent, func, ...)
  }
  
  return(l2)
}

recursive.mean <- function(l1, l2, additionalList, n, ...)
{
  return(l1 + l2/n)
}

recursive.variance <- function(l1, l2, additionalList, n, ...)
{
  return(l1 + ((l2 - additionalList) ^ 2 / n))
}

recursive.sqrt <- function(l1, l2, additionaList, ...)
{
  return(sqrt(l2))
}