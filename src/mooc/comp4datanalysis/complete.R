complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  files <- lapply(id, function(x) paste(directory, paste0(formatC(as.integer(x), 2, flag=0), ".csv"), sep="/"))
  counter <- function (file) length(which(complete.cases(read.csv(file))))
  nobs <- unlist(lapply(files, counter))
  
  return (data.frame(id, nobs))
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}