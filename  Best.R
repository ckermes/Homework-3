best <- function(state, outcome){
  ##Read outcome data
  dataraw <- read.csv('outcome-of-care-measures.csv',
                      colClasses = "character", 
                      na.strings = "Not Available", 
                      stringsAsFactors = FALSE)
  
  data <- dataraw[, c(2,7,11,17,23)]
  
  names(data) <- c("hospital", "state", "heart attack",
                   "heart failure", "pneumonia")
  
  data[,3] <- as.numeric(data[,3])
  data[,4] <- as.numeric(data[,4])
  data[,5] <- as.numeric(data[,5])
  
  ##Check that state and outcome are valid
  if(any(state == data$state))
    dataState <- data[(data$state == state),]
  else
    stop("invalid state")

  if (outcome == names(data)[3])
    outtemp <- 3
  else if(outcome == names(data)[4])
    outtemp <- 4
  else if(outcome == names(data)[5])
    outtemp <- 5
  else
      stop("invalid outcome")
  
  ##Return hospital name in that state with lowest 30-day death rate
  sort <- dataState[order(dataState[outtemp], dataState[1]),]
  
  sort[1,1]

}

