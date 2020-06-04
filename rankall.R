setwd("D:/R_programming/week4_progg_assignment")
datas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
datas<- data[c(2,7,11,17,23)]

names(datas)[1] <- "name"
names(datas)[2] <- "State"
names(datas)[3] <- "heart attack"
names(datas)[4] <- "heart failure"
names(datas)[5] <- "pneumonia"

rankall <- function( outcome , num = "best") {
  
 
  
  

  
  
  
  
  co<- data.frame( "name" = character(), "state" = character())
  
  if ( num == "best"  ){ num <- 1}
 ## else if (num == "worst"){num <- nrow(d)}
  else if ( !(num >=1 && num <= nrow(d))){ return(NA)}
  

                  
                  p <- 0
                  datas <- datas[c("name", "State" , outcome)]
                  

                  #as.numeric(data$outcome)
                  #data <- data[!is.na(data$outcome),]
                
                  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){ return("INVALID OUTCOME")}
            
                  states <- unique(datas$State)
                  
                 for (i in states) {
                  p <- (p+1)
                  
                  t <- subset(datas, data$State == i)
                  #t <- t[order(t$name),]
                 t <- t[order(t$name),]
                 t <- t[order(t[outcome]),]
                 
                 for (r in t[,outcome]) {
                   
                   if( r == "Not Available") { r <- NA}
                   
                 }
                 
                 t <- t[!is.na(t[outcome])]
                if ( num == "best"  ){ num <- 1}
                  else if (num == "worst"){num <- nrow(t)}
                  else if ( num > nrow(t)){ return("NA")}
                  
                  
                  t <- t[num, ]
                
                  
                  co[p,"name"] <- t$name
                  co[p,"state"] <- i
                       
                        
                         
                           
                  
                 }
                  
                  co <- co[order(co$state),]
             return(co)
                
  
  
}