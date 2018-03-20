

## The best Function


best <- function(state , outcome) {
  outcome_data <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome_vector <- c("heart attack", "heart failure", "pneumonia")
  
  state_vector <- unique(outcome_data[, 7])
  
  if (sum(state_vector == state) != 1) {
    stop("invalid state")
  }
  
  else if (sum(outcome_vector == outcome) != 1) {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    col_num = 11
  }
  else if (outcome == "heart failure") {
    col_num = 17
  }
  else if (outcome == "pneumonia") {
    col_num = 23
  }
  
  ## making state data frame
  
  state_data <-
    outcome_data[outcome_data$State == state & !is.na(outcome_data[,col_num]), c(2, 7, col_num)]
  
  state_data[,3] <- as.numeric(state_data[,3])
  
  state_data <- state_data[order(state_data[,1]),]
  
  state_data$rank <-rank(state_data[,3],ties.method = "first")
  
  
  state_data <- state_data[order(state_data[,4],state_data[,1]),]
  state_data[1,1]
  
}