best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
    
  ## Check that state and outcome are valid
  states <- unique(df[,"State"])
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!is.element(state, states)) stop("invalid state")
  if (!is.element(outcome, validOutcomes)) stop("invalid outcome")
  
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
  
  return(as.vector(validByState[order(validByState$metricName, validByState$hName),][1,2]))
}
