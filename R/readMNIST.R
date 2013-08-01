#' Function for generating ff files of the MNIST Database
#' 
#' This function reads the MNIST-Database, randomized it and saved it in the 
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
#' @usage readMNIST(folder)
#' 
#' @docType methods
#' @rdname readMNIST
#' @export
readMNIST <- function(folder){
  print(paste("Loading the the MNIST data set."))
  
  # This function reads the data and labels from the two files given by
  # dataName and labelName. Afterwards it puts the the data and labels
  # together in one matrix and sorted it by the labels. The label is in
  # the last column. Then it returns the sorted matrix.
  loadData <- function(dataName, labelName){
    # Read the data
    file <- file(dataName,'rb')
    readBin(file,'integer',n=1,size=4,endian='big')
    rows <- readBin(file,'integer',n=1,size=4,endian='big')
    numRow <- readBin(file,'integer',n=1,size=4,endian='big')
    numCol <- readBin(file,'integer',n=1,size=4,endian='big')
    columns <- numRow*numCol
    data <- ff(vmode="single", dim=c(rows, columns))
    buffer <- ff(vmode="single", dim=c(rows*columns))
    buffer[] <- readBin(file,'integer',n=rows*columns,size=1,signed=F)
    data[] <- matrix(buffer[], nrow=rows, byrow=T)
    data[] <- data[]/255
    delete(buffer)
    rm(buffer)
    close(file)
    gc()
    
    # Read the labels
    file <- file(labelName,'rb')
    readBin(file,'integer',n=1,size=4,endian='big')
    num <- readBin(file,'integer',n=1,size=4,endian='big')
    labels <- ff(vmode="integer", dim=c(num))
    labels[] <- readBin(file,'integer',n=num,size=1,signed=F)
    labels[] <- labels[]+1
    close(file)
    gc()
    
    # Sort the data by the labels
    sortedData <- ff(vmode="single", dim=c(rows, columns+1))
    sortedData[] <- cbind(data[],labels[]) # putting data and labels togehter
    sortedData[] <- sortedData[order(sortedData[,columns+1]),] # sort the data by the column 785 (the label)
    
    delete(data)
    rm(data)
    delete(labels)
    rm(labels)
    gc()
    return(sortedData)
  }
  
  # Bring the sorted data matrices in a random order
  generateData <- function(data,random,dims){
    
    randomData <- ff(vmode="single",dim=c(dims[1],dims[2]+1))
    rdata <- ff(vmode="single", dim=c(dims[1],dims[2]-1)) 
    
    # Mix the train data
    randomData[] <- cbind(data[],random)
    randomData[] <- randomData[order(randomData[,dims[2]+1]),]
    rdata[] <- randomData[,1:(dims[2]-1)]
    return(rdata)
  }
  
  generateLabels <- function(counts,random,rows){
    # generate a label matrix with rows of kind c(1,0,0,0,0,0,0,0,0,0) and 
    # mix the train labels
    randomLabels <- ff(vmode="integer",dim=c(rows,11))
    rlabels <- ff(vmode="byte", dim=c(rows,10))
    start <- 1
    end <- 0
    for(i in 1:10){
      c <- rep(0,10) 
      c[i] <- 1
      l <- matrix(c,nrow=counts[i],ncol=10,byrow=TRUE)
      end <- end + counts[i]
      rlabels[start:end,] <- l
      start <- start + counts[i]
      print(paste("class ", (i-1)," = ", counts[i], " images", sep=""))
    } 
    
    randomLabels[] <- cbind(rlabels[],random)
    randomLabels[] <- randomLabels[order(randomLabels[,11]),]
    rlabels[] <- randomLabels[,1:10]
    return(rlabels)
  }
  
  print(paste("Loading train set with 60000 images."))
  train <- loadData(paste(folder,"train-images-idx3-ubyte",sep=""), paste(folder,"train-labels-idx1-ubyte",sep=""))
  dims <- dim(train)
  random <- sample(1:dims[1])
  counts <- table(train[,dims[2]])
  print(paste("Generating randomized data set and label matrix"))
  trainData <- generateData(train,random,dims)		
  trainLabels <- generateLabels(counts,random,dims[1])
  print(paste("Saving the test data (filename=train)"))
  ffsave(trainData,trainLabels,file="train",add=FALSE)
  
  print(paste("Loading test set with 10000 images."))
  test <- loadData(paste(folder,"t10k-images-idx3-ubyte",sep=""),paste(folder,"t10k-labels-idx1-ubyte",sep=""))
  dims <- dim(test)
  random <- sample(1:dims[1])
  counts <- table(test[,dims[2]])
  print(paste("Generating randomized data set and label matrix"))
  testData <- generateData(test,random,dims)		
  testLabels <- generateLabels(counts,random,dims[1])
  print(paste("Saving the test data (filename=test)"))
  ffsave(testData,testLabels,file="test",add=FALSE)
  print(paste("Finished"))
}