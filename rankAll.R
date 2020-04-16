rankall <- function(outcome, num = "best"){
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
  
  ##Check that outcome is valid
  if (outcome == names(data)[3])
    outtemp <- 3
  else if(outcome == names(data)[4])
    outtemp <- 4
  else if(outcome == names(data)[5])
    outtemp <- 5
  else
    stop("invalid outcome")
  
  ##For each state, find the hospital of the given rank
  
  ##Clean data of NA values. ranked is new cleaned data
  cleanout <- data[!is.na(data[, outtemp]),]
  ranked <- cleanout[order(cleanout[2], cleanout[outtemp],
                    cleanout[1]),]
  
  sorted <- split(ranked, as.factor(ranked$state))
  hospName <- vector("character", length = length(sorted))
  
  if(num == "best"){
    for(i in 1:length(sorted))
      hospName[i] <- sorted[[i]][1,1]
  }
  else if(num == "worst"){
    for(i in 1:length(sorted)){
        index <- nrow(sorted[[i]])
        hospName[i] <- sorted[[i]][index,1]
    }
  }
  else if(num %% 1 == 0){
    for(i in 1:length(sorted))
        hospName[i] <- sorted[[i]][num,1]
  }
  ##Return a data frame with the hospital names and the 
  ##(abbreviated) state name
  stateName <- names(sorted)
  result <- data.frame(hospital = hospName, state = stateName,
                       row.names = stateName)
}