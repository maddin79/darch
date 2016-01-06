# Copyright (C) 2013-2015 Martin Drees
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

#' Function for generating .RData files of the MNIST Database
#' 
#' This function reads the MNIST-Database, randomized it and saves it in the 
#' files "train" for the training data and "test" for test data.
#' 
#' @details
#' When the data is read the variables for the training data is \code{trainData} 
#' and \code{trainLabels} and for the test data \code{testData} and 
#' \code{testLabels}. To start the function
#' The files "train-images-idx3-ubyte", "train-labels-idx1-ubyte', 
#' "t10k-images-idx3-ubyte", and "t10k-labels-idx1-ubyte" have to be in the
#' folder given by the parameter \code{folder}. The folder name must end with 
#' a slash.
#' 
#' @param folder The location of the MNIST-Database files.
#'
#' @export
readMNIST <- function(folder){
  flog.info("Loading the MNIST data set.")
  
  # This function reads the data and labels from the two files given by
  # dataName and labelName. Afterwards it puts the data and labels
  # together in one matrix and sorted it by the labels. The label is in
  # the last column. Then it returns the sorted matrix.
  loadData <- function(dataName, labelName){
    fileFunction <- file
    
    # Switch to gzfile function if necessary
    if (file.exists(paste0(dataName,".gz")))
    {
      dataName <- paste0(dataName, ".gz")
      labelName <- paste0(labelName, ".gz")
      fileFunction <- gzfile
    }
    
    # Read the data
    file <- fileFunction(dataName,'rb')
    readBin(file,'integer',n=1,size=4,endian='big')
    rows <- readBin(file,'integer',n=1,size=4,endian='big')
    numRow <- readBin(file,'integer',n=1,size=4,endian='big')
    numCol <- readBin(file,'integer',n=1,size=4,endian='big')
    columns <- numRow*numCol
    buffer <- readBin(file,'integer',n=rows*columns,size=1,signed=F)
    data <- matrix(buffer, nrow=rows, byrow=T)/255
    rm(buffer)
    close(file)
    gc()
    
    # Read the labels
    file <- fileFunction(labelName,'rb')
    readBin(file,'integer',n=1,size=4,endian='big')
    num <- readBin(file,'integer',n=1,size=4,endian='big')
    labels <- readBin(file,'integer',n=num,size=1,signed=F)+1
    close(file)
    gc()
    
    # Sort the data by the labels
    sortedData <- cbind(data[],labels[]) # putting data and labels together
    sortedData <- sortedData[order(sortedData[,columns+1]),] # sort the data by the column 785 (the label)
    
    rm(data)
    rm(labels)
    gc()
    return(sortedData)
  }
  
  # Bring the sorted data matrices in a random order
  generateData <- function(data,random,dims){
    # Mix the train data
    randomData <- cbind(data[],random)
    randomData <- randomData[order(randomData[,dims[2]+1]),]
    rdata <- randomData[,1:(dims[2]-1)]
    return(rdata)
  }
  
  generateLabels <- function(counts,random,rows){
    # generate a label matrix with rows of kind c(1,0,0,0,0,0,0,0,0,0) and 
    # mix the train labels
    rlabels <- matrix(0, nrow=rows, ncol=10)
    start <- 1
    end <- 0
    for(i in 1:10){
      c <- rep(0,10) 
      c[i] <- 1
      l <- matrix(c,nrow=counts[i],ncol=10,byrow=TRUE)
      end <- end + counts[i]
      rlabels[start:end,] <- l
      start <- start + counts[i]
      flog.info(paste0("class ", (i-1)," = ", counts[i], " images"))
    } 
    
    randomLabels <- cbind(rlabels, random)
    randomLabels <- randomLabels[order(randomLabels[,11]),]
    rlabels <- randomLabels[,1:10]
    return(rlabels)
  }
  
  flog.info("Loading train set with 60000 images.")
  train <- loadData(paste(folder,"train-images-idx3-ubyte",sep=""), paste(folder,"train-labels-idx1-ubyte",sep=""))
  dims <- dim(train)
  random <- sample(1:dims[1])
  counts <- table(train[,dims[2]])
  flog.info("Generating randomized data set and label matrix")
  trainData <- generateData(train,random,dims)		
  trainLabels <- generateLabels(counts,random,dims[1])
  flog.info("Saving the train data (filename=train)")
  save(trainData, trainLabels, file=paste0(folder, "train.RData"), precheck=T, compress=T)
  
  flog.info("Loading test set with 10000 images.")
  test <- loadData(paste(folder,"t10k-images-idx3-ubyte",sep=""),paste(folder,"t10k-labels-idx1-ubyte",sep=""))
  dims <- dim(test)
  random <- sample(1:dims[1])
  counts <- table(test[,dims[2]])
  flog.info("Generating randomized data set and label matrix")
  testData <- generateData(test,random,dims)		
  testLabels <- generateLabels(counts,random,dims[1])
  print(paste("Saving the test data (filename=test)"))
  save(testData, testLabels, file=paste0(folder, "test.RData"), precheck=T, compress=T)
  flog.info("Finished")
}

#' Provides MNIST data set in the given folder.
#' 
#' This function will, if necessary and allowed, download the compressed MNIST
#' data set and save it to .RData files using \code{\link{readMNIST}}.
#' 
#' @param download Logical indicating whether download is allowed.
#' @param folder Folder name, including a trailing slash.
#' @return Boolean value indicating success or failure.
#' 
#' @export
provideMNIST <- function (folder="data/", download=F)
{
  # TODO: does not work on windows, will generate warning message because it
  # tries to create the directory even if it exists
  if (!file.exists(folder))
  {
    dir.create(folder)
  }
  
  fileNameTrainImages <- "train-images-idx3-ubyte.gz"
  fileNameTrainLabels <- "train-labels-idx1-ubyte.gz"
  fileNameTestImages <- "t10k-images-idx3-ubyte.gz"
  fileNameTestLabels <- "t10k-labels-idx1-ubyte.gz"
  
  mnistUrl <- "http://yann.lecun.com/exdb/mnist/"
  
  if (file.exists(paste0(folder, "train.RData")) &&
        file.exists(paste0(folder, "test.RData")))
  {
    flog.info("MNIST data set already available, nothing left to do.")
    return(T)
  }
  
  if (download && any(
    !file.exists(paste0(folder,fileNameTrainImages)),
    !file.exists(paste0(folder,fileNameTrainLabels)),
    !file.exists(paste0(folder,fileNameTestImages)),
    !file.exists(paste0(folder,fileNameTestLabels))
  ))
  {
    flog.info("Compressed MNIST files not found, attempting to download...")
    
    statusCodes <- c()
    
    for (file in c(fileNameTrainImages, fileNameTrainLabels,
                   fileNameTestImages, fileNameTestLabels))
    {
      statusCodes <- c(statusCodes,
                        utils::download.file(paste0(mnistUrl, file),
                        paste0(folder, file)))
    }
    
    if (any(statusCodes>0))
    {
      flog.error(paste("Error downloading MNIST files. Download manually",
                        "from", mnistUrl, "or try again."))
      return(F)
    }
    
    flog.info("Successfully downloaded compressed MNIST files.")
  }
  else
  {
    flog.info(paste("Compressed MNIST files found or download disabled,",
              "skipping download."))
  }
  
  readMNIST(folder)
}