# Make sure the benchmark directory is ready
prepareBenchmarkDirectory <- function (name, save = F, continue = F, delete = F,
                                       fileType=".net")
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
    lastFile <- tail(dir(name, pattern=paste0(".*_\\d{3}", fileType)), n = 1)
    
    if (length(lastFile) > 0)
    {
      indexStart <-
        as.numeric(substr(tail(strsplit(lastFile, "_")[[1]], n = 1), 1, 3)) + 1
    }
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

createPlotErrorRaw <- function(stats, fileName = NULL, ...)
{
  epochs <- c(1:length(stats$times))
  
  # Plot for training and validation raw error
  writePlot(fileName, epochs,
    list(data = stats$dataErrors$raw, valid = stats$validErrors$raw),
    "Network error", "Epoch", "Error",
    legend=list(pos = "topright", labels = c("Training", "Validation")))
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
    rangeY=rangeY)
}

createPlotTime <- function(stats, fileName = NULL, ...)
{
  epochs <- c(1:length(stats$times))
  
  # Plot for times
  writePlot(fileName, epochs, list(times=stats$times),
            "Runtime", "Epoch", "Time (sec)")
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

writePlot <- function(fileName=NULL, x, y=list(), main, xlab, ylab, legend=NULL, rangeY=NULL)
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
  
  if (!is.null(legend))
  {
    legend(legend$pos, legend=legend$label, lty=c(1:length(y)), xpd=T)
  }
  
  if (!is.null(fileName)) dev.off()
}