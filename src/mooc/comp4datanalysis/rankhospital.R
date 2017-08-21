rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
  
  ## Check that state and outcome are valid
  states <- unique(df[,"State"])
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  validNonIntNum <- c("best", "worst")
  
  if (!is.element(state, states)) stop("invalid state")
  if (!is.element(outcome, validOutcomes)) stop("invalid outcome")
  if (!is.element(num, validNonIntNum) && !is.numeric(num)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## [2] "Hospital.Name"
  ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  ##if (outcome == validOutcomes[1]) colIdx <- c(11)
  colIdx <- c(11)
  if (outcome == validOutcomes[2]) colIdx <- c(17)
  else if (outcome == validOutcomes[3]) colIdx <- c(23)
  
  # columns
  fState <- df[,"State"]
  hName <- df[,"Hospital.Name"]
  metricName <- as.numeric(df[,colIdx[1]])
  
  # create data frame and filter
  all <- data.frame(fState, hName, metricName)
  # filter out incompletes
  valid <- all[complete.cases(all),]
  # filter by State
  validByState <- valid[valid$fState == state,]
  # select output
  if (num == "best") rowIdx <- 1
  else if (num == "worst") rowIdx <- nrow(validByState)
  else rowIdx <- as.numeric(num)
  
  if (rowIdx > nrow(validByState)) return (NA)
  
  return(as.vector(validByState[order(validByState$metricName, validByState$hName),][rowIdx[1],2]))
  
}