setwd("D:/R_programming/week4_progg_assignment")

d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
d <- d[c(2,7,11,17,23)]

names(d)[1] <- "name"
names(d)[2] <- "State"
names(d)[3] <- "heart attack"
names(d)[4] <- "heart failure"
names(d)[5] <- "pneumonia"





rankhospital <- function(state, outcome , num = "best" ){
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){ return("INVALID OUTCOME")}

    
    
    d <- subset( d , d$State == state )
    
  
  for (i in 3:5) {
    
    d[,i] <- as.numeric(d[,i])
    
  }
    
    
     d <- d[complete.cases(d),]
    d <- d[order(d$name),]
    d <- d[order(d[,outcome]),]
     
    if ( num == "best"  ){ num <- 1}
    else if (num == "worst"){num <- nrow(d)}
    else if ( !(num >=1 && num <= nrow(d))){ return(NA)}
     
    return(d[num,"name"])
  
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
