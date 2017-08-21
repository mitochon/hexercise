corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  files <- list.files(directory)
  #counter <- function (file) length(which(complete.cases(read.csv(paste(directory, file, sep="/")))))
  #qlfySet <- as.vector(which(lapply(files, counter) > threshold))
  
  #print(qlfySet)
  
  #if (length(qlfySet) < 1) {
  #  return(as.numeric())
  #}
  
  result <- numeric(0)
  
  for (i in files) {
    wholeData <- read.csv(paste(directory, i, sep="/"))
    completeData <- wholeData[complete.cases(wholeData),]
    if (length(which(complete.cases(completeData))) > threshold) {
      result[i] <- cor(completeData[,c('sulfate')], completeData[,c('nitrate')])
      #print(cor(completeData[,c('sulfate')], completeData[,c('nitrate')]))
    }
  } 
  
  return(result)
  
  #completeData <- lapply(dataSet, function (data) data[complete.cases(data)],)
  #corrData <- lapply(completeData, function (data) cor(data[,c('sulfate')], data[,c('nitrate')]))
  
  #print(data)
                      
  ## Return a numeric vector of correlations
}