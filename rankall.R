


## The best Function


rankall <- function(outcome, num = "best" ) {
  
  inp_rank <<- num
  outcome_data <-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome_vector <- c("heart attack", "heart failure", "pneumonia")
  
  state_vector <- unique(outcome_data[, 7])
  
  
  
  if (sum(outcome_vector == outcome) != 1) {
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
  
  globalVariables(num)
  
  
  ## making state data frame
  
  allstate_data_na <-outcome_data[, c(2, 7, col_num)]
  
  allstate_data_na[,3] <- as.numeric(allstate_data_na[,3])
  
  allstate_data <- allstate_data_na[!is.na(allstate_data_na[,3]),]
  
  
  split_statedata <- split(allstate_data,allstate_data[,2])
  
  
  split_statedata <- lapply(split_statedata,give_rank)
  
  
  rank_data_frame <-lapply(split_statedata,make_frame)
  
  
  hospital <-lapply(rank_data_frame,"[",,1)
  state <-lapply(rank_data_frame,"[",,2)
  
  ff <- cbind(hospital,state)
  
  data_frame_result <- as.data.frame(ff)
  
  #return(rank_data_frame)
 # if( (hospitalsnos < as.numeric(num)) && is.numeric(num)){ return(NA)}
  
}

give_rank <- function(state_data){
  
  
  #state_data <- na.omit(state_data)
  state_data <- state_data[order(state_data[,1]),]
  
  state_data$rank <- rank(state_data[, 3],na.last = "keep" ,ties.method = "first")
  state_data <- state_data[order(state_data[, 4], state_data[, 1]), ]
  
  return(state_data)
  
}

make_frame <- function(state_data){
  
  
  
  if(inp_rank == "best"){inp_rank <-1}
  
  else if(inp_rank == "worst"){inp_rank = max(state_data[,4])}
  
  
  if(as.numeric(inp_rank) <= max(state_data[,4]) ){
   return(state_data[as.numeric(inp_rank),c(1,2)])
  }
  else{ 
    
    state_data[1,1] <- NA
    
    return(state_data[1,c(1,2)])}
   }