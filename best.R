setwd("D:/R_programming/week4_progg_assignment")



  
  
  
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
best <- function(state , outcome){
  
  
  if(state %in% data$State == FALSE){
    return("INVALID STATE")
  }
  
  
  
  ## NAME COLUMN - data[2]
  ## HEART ATTACK COLUMN - data[11]
  ## HEART FAILURE COLUMN - data[17]
  ## PNEUMONIA COLUMN - data[23]
  
  
  
  
  
  
  data <- subset(data, data$State == state)
  if(outcome=="heart attack"){ vals <- data[,11]}
    
  
  else if(outcome=="heart failure"){ vals<- data[,17]}
  
  else if(outcome=="pneumonia"){ vals <- data[,23]}
  
  else { return( " INVALID OUTCOME")}
  
  
  
  
  minimums <- which.min(vals)
  
  return( data[minimums,2])
  
  
  }
  
  
  
  
  
  
  
  
  
  
  
