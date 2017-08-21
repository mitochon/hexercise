getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  id <- paste0(formatC(as.integer(id), 2, flag=0), ".csv")
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  data <- read.csv(paste(directory, id, sep="/"))
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  if (summarize) print (summary(data))
  return (as.data.frame(data))
}