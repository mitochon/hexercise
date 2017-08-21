rankall <- function(outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses ="character")
  
  ## Check that state and outcome are valid
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  validNonIntNum <- c("best", "worst")
  
  if (!is.element(outcome, validOutcomes)) stop("invalid outcome")
  if (!is.element(num, validNonIntNum) && !is.numeric(num)) stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  byState <- split(df, df$State)
  
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
  
  out <- data.frame(hospital = character(), state = character())
  
  # loop by State
  for (perState in byState) {
    
    # columns
    hName <- perState[,"Hospital.Name"]
    metricName <- as.numeric(perState[,colIdx[1]])
    
    # create data frame and filter
    all <- data.frame(hName, metricName)
    # filter out incompletes
    valid <- all[complete.cases(all),]
    
    # select output
    if (num == "best") rowIdx <- 1
    else if (num == "worst") rowIdx <- nrow(valid)
    else rowIdx <- as.numeric(num)
    
    if (rowIdx > nrow(valid)) {
      out <- rbind(out, data.frame(hospital=NA, state=perState$State[1]))
    } else {
      selHosp <- valid[order(valid$metricName, valid$hName),][rowIdx[1],1]
      out <- rbind(out, data.frame(hospital=selHosp, state=perState$State[1]))
    }
  }  
  return(out)
}