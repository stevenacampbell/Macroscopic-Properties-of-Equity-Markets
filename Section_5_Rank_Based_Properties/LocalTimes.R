### R Code file to compute intensities of rank switching (discretized
### local time)
### NOTE: Current specifications take a long time to run


# Load Libraries

library(readr)
library(tidyr)

# Reset Environment

rm(list=ls())

# Import caps

caps <- read_csv("caps_common_1962_2023_ffill.csv",
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import dates

dates <-  read_csv("dates_common_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]


topNCaps <- function(mat, j, N){
  # Get the top N caps and their indices in the data
  
  # Extract the jth column
  column <- mat[,j]
  
  # Get the indices of the top N capitalizations
  indices <- order(column, decreasing = TRUE)[1:N]
  
  # Get the top N capitalizations
  capitalizations <- column[indices]
  
  # Create a data frame with the results
  result <- data.frame(indices = indices, capitalizations = capitalizations)
  
  return(result)
}

LocalTimeIncrement<-function(mat,i,N){
  # Compute the increment of the local times for the top N caps
  # stored in the matrix mat at the time corresponding to column i.
  
  init_vals<-mat[,i]
  new_vals<-mat[,i+1]
  top_caps<-topNCaps(mat,i,N)
  sorted_init_vals <- top_caps[[2]]
  sorted_init_ids <- top_caps[[1]]
  sorted_new_vals<- -1*sort(-new_vals)[1:N]
  
  
  Lambda_Equations<-rep(0,N)
  dLambda<-rep(0,N)
  dRanked<-sorted_new_vals-sorted_init_vals
  init_update<-mat[sorted_init_ids,i+1]
  init_update[init_update<0]<-0
  dVals<-init_update-sorted_init_vals
  Lambda_Equations<-2*(dRanked-dVals)
  dLambda[1]<-Lambda_Equations[1]
  for(i in seq(2,N,1)){
    dLambda[i]<-Lambda_Equations[i]+dLambda[i-1]
  }
  return(dLambda)
  
}

GetLocalTimes<-function(caps,N){
  # Compute local times of the top N capitalizations stored in 'caps'
  
  times<-ncol(caps)
  Lambda<-matrix(0,nrow=N,ncol=times)
  for(i in seq(2,times,1)){
    Lambda[,i]<-Lambda[,i-1]+LocalTimeIncrement(caps,i-1,N)
    if(i %% 100==1){
      print(i)
    }
  }
  return(Lambda)
}

N<-500 # Number of Stocks

# Prep Data
caps[is.na(caps)]<-0
log_caps<-log(caps)

# Get Local Times
Lambda<-GetLocalTimes(log_caps,N)

# Plot Local Times
#pdf(file = "local_time.pdf", width = 6, height = 5)
par(mfrow = c(1, 1))
gradient_col <- gray.colors(N, start = 0.1, end = 0.8)
idx <- seq(1, length(dates), by = 10)  # to reduce size of figure
plot(dates[idx],rep(0, length(dates[idx])), col="white",type="l",
     ylab="",xlab="Time", ylim=c(min(Lambda),max(Lambda)),
     main="Intensity of rank switching",lwd=0.5)
for(i in seq(2,N,1)){
  lines(dates[idx],Lambda[i,idx], col=gradient_col[i],lwd=0.5)
}
#dev.off()
