# Copyright (C) 2016 Johannes Rueckert
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

#' Benchmarking wrapper for \code{darch}
#' 
#' Simple benchmarking function which wraps around the \code{\link{darch}}
#' function for users who can't or don't want to use the caret package for
#' benchmarking. This function requires the \code{foreach}
#' package to work, and will perform parallel benchmarks if an appropriate
#' backend was registered beforehand.
#' 
#' @param ... Parameters to the \code{\link{darch}} function
#' @param bench.times How many benchmark runs to perform
#' @param bench.save Whether to save benchmarking results to a directory
#' @param bench.dir Path (relative or absolute) including directory where
#'   benchmark results are saved if \code{bench.save} is true
#' @param bench.continue Whether the benchmark is to be continued from an
#'   earlier run. If \code{TRUE}, existing benchmark results are looked for in
#'   the directory given in \code{bench.dir} and new results are appended.
#'   If both this and \code{bench.continue} are \code{FALSE} and
#'   the directory given in \code{bench.dir} does already exist, the training
#'   will be aborted with an error.
#' @param bench.delete Whether to delete the contents of \code{bench.dir} if
#'   \code{bench.continue} is \code{FALSE}. Caution: This will attempt to delete
#'   ALL files in the given directory, use at your own risk!
#' @param bench.seeds Vector of seeds, one for each run. Will be passed to
#'   \code{\link{darch}}.
#' @param output.capture Whether to capture R output in \code{.Rout} files in
#'   the given directory. This is the only way of gaining access to the R
#'   output since the foreach loop will not print anything to the console. Will
#'   be ignored if \code{bench.save} is \code{FALSE}.
#' @return List of \code{DArch} instances; the results of each call to
#'   \code{darch}.
#' @examples
#' \dontrun{
#' data(iris)
#' modelList <- darchBench(Species ~ ., iris, c(0, 50, 0),
#'  preProc.params = list(method = c("center", "scale")),
#'  darch.unitFunction = c("sigmoidUnit", "softmaxUnit"),
#'  darch.numEpochs = 30, bench.times = 10, bench.save = T)
#' }
#' @inheritParams darch
#' @family darch interface functions
#' @export
darchBench <- function(...,
  bench.times = 1,
  bench.save = F,
  bench.dir = "./darch.benchmark",
  bench.continue = T,
  bench.delete = F,
  bench.seeds = NULL,
  output.capture = bench.save,
  logLevel = NULL
)
{
  oldLogLevel <- futile.logger::flog.threshold()
  on.exit(futile.logger::flog.threshold(oldLogLevel))
  setLogLevel(logLevel)
  
  indexStart <- prepareBenchmarkDirectory(bench.dir, bench.save,
    bench.continue, bench.delete)
  
  darchList <- performBenchmark(bench.dir, bench.times, indexStart,
    bench.save = bench.save, output.capture = output.capture,
    bench.seeds = bench.seeds, ...)

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
      "Directory '%s' could not be created.", name))
  }
  
  indexStart <- 1
  
  if (continue)
  {
    indexStart <- max(unlist(sapply(fileType, FUN = function(x)
    {
      fileName <- utils::tail(dir(name, pattern = paste0(".*_\\d{3}", x)), n = 1)
      as.numeric(substr(utils::tail(strsplit((if (length(fileName) > 0) fileName
        else "none_000"), "_")[[1]], n = 1), 1, 3)) + 1
    })))
  }
  
  indexStart
}

performBenchmark <- function(name, iterations = 1, indexStart = 1, ...,
  bench.save = F, output.capture = F, bench.seeds = NULL)
{ 
  if (!suppressMessages(requireNamespace("foreach", quietly = T)))
  {
    stop(futile.logger::flog.error(
      "\"foreach\" package required when using darchBench()."))
  }
  
  if (length(bench.seeds) > 0 && length(bench.seeds) < iterations)
  {
    futile.logger::flog.warn(
      "Invalid length of bench.seeds parameter, generating new seeds")
    bench.seeds <- NULL
  }
  
  if (is.null(bench.seeds))
  {
    bench.seeds <- round(runif(iterations, 0, 10 ^ 6))
  }
  
  futile.logger::flog.info("Starting %d training runs...",
                           iterations)
  
  i <- 0 # to avoid check NOTE due to no visible binding for i
  resultList <-
    foreach::`%dopar%`(foreach::foreach(i = indexStart:(indexStart + iterations - 1)),
  {
    fileName <- paste0(name, "/", basename(name), "_",
      formatC(i, width = 3,flag = "0"))
    outputFile <- if (bench.save && output.capture) 
      paste0(fileName, ".Rout") else NULL

    utils::capture.output(darch <- darch(..., autosave.dir = fileName,
      seed = bench.seeds[i - indexStart + 1]), file = outputFile)
    
    saveDArch(darch, fileName, T)
    
    darch
  })
  
  futile.logger::flog.info("Finished %d training runs", iterations)
  
  resultList
}

# TODO remove?
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