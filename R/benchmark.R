# Make sure the benchmark directory is ready
prepareBenchmarkDirectory <- function (name, save = F, continue = F, delete = F,
                                       fileType=c(".net", ".Rout"))
{
  if (!save) return(1)
  
  # TODO windows?
  if (file.exists(paste0(name, "/")) && !continue && !delete)
  {
    stop(paste0("Benchmark directory \"", name, "\" already exists. Please ",
      "delete the directory manually or pass bench.delete=T to automatically ",
      "delete the contents of the directory (use at your own risk!)."))
  }
  
  suppressWarnings(dir.create(name, recursive = T))
  
  if (!file.exists(paste0(name, "/")))
  {
    stop("Benchmark directory could not be created.")
  }
  
  indexStart <- 1
  
  if (!continue)
  {
    futile.logger::flog.warn(paste("Deleting contents of directory", name,
      "in 5 seconds..."))
    
    Sys.sleep(time=5)
    
    # delete files, but don't delete empty directory
    if (length(dir(name)) > 0 ) file.remove(paste(name, dir(name), sep="/"))
  }
  else
  {
    indexStart <- max(unlist(sapply(fileType, FUN=function(x)
    {
      fileName <- tail(dir(name, pattern=paste0(".*_\\d{3}", x)), n = 1)
      as.numeric(substr(tail(strsplit((if (length(fileName) > 0) fileName
        else "none_001"), "_")[[1]], n = 1), 1, 3)) + 1
    })))
  }
  
  indexStart
}

performBenchmark <- function (name, iterations = 1, indexStart = 1, ...,
                              bench.save = F, output.capture = F)
{ 
  if (!suppressMessages(requireNamespace("foreach", quietly = T)))
  {
    stop("\"foreach\" package required when using darchBench().")
  }
  
  futile.logger::flog.info("Starting %d training runs...",
                           iterations)
  
  resultList <-
    foreach::`%dopar%`(foreach::foreach(i = indexStart:(indexStart+iterations-1)),
  {
    futile.logger::flog.info("Started training run #%d", i)
    
    fileName <- paste0(name, "/", basename(name), "_", formatC(i, 2,flag="0"))
    outputFile <- if (bench.save && output.capture) 
      paste0(fileName, ".Rout") else NULL

    capture.output(darch <- darch(..., autosave.location=fileName),
                   file = outputFile)
    
    futile.logger::flog.info("Finished training run #%d", i)
    
    darch
  })
  
  resultList
}

writeStatistics <- function(name, stats)
{
  fileName <- paste0(name, "/", basename(name), ".stats")
}

aggregateStatistics <- function(darchList)
{
  statsMean <- NULL
  stats <- list()
  
  # calculate means
  for(i in 1:length(darchList))
  {
    stats[[i]] <- darchList[[i]]@stats
    statsMean <- recursiveApplyLists(statsMean, darchList[[i]]@stats, 0, NULL,
                                     recursive.mean, n=numFiles)
  }
  
  statsDeviation <- NULL
  
  # standard deviations
  for (i in 1:length(darchList))
  {
    statsDeviation <-
      recursiveApplyLists(statsDeviation, darchList[[i]]@stats, 0, statsMean,
                          recursive.variance, n=numFiles)
  }
  
  statsDeviation <- recursiveApplyLists(NULL, statsDeviation, 0, NULL,
                                        recursive.sqrt)
  
  list("all" = stats, "mean" = statsMean, "deviation" = statsDeviation)
}

createAllPlots <- function(name, stats, ...)
{
  filePrefix <- paste0(name, "/", basename(name))
  
  createPlotErrorRaw(stats, paste0(filePrefix, "_error_raw.pdf"), ...)
  createPlotErrorClass(stats, paste0(filePrefix, "_error_class.pdf"), ...)
  createPlotTime(stats, paste0(filePrefix, "_time.pdf"), ...)
}

loadAllDArch <- function(dirName)
{
  files <- dir(dirName, pattern="*.net")
  darchList <- vector(mode="list", length=length(files))
  
  if (length(files) > 0)
  {
    # load DArch objects and extract stats
    for(i in 1:length(files))
    {
      darch <- loadDArch(paste0(dirName, "/",
                                substr(files[i], 1, nchar(files[i])-4)))
      
      darchList[[i]] <- darch
    }
  }
  
  darchList
}

recursiveApplyLists <- function(l1=NULL, l2, default=0, additionalList=NULL, func, ...)
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
    return (l2)
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
  return (l1 + l2/n)
}

recursive.variance <- function(l1, l2, additionalList, n, ...)
{
  return(l1 + ((l2 - additionalList)^2/n))
}

recursive.sqrt <- function(l1, l2, additionaList, ...)
{
  return(sqrt(l2))
}