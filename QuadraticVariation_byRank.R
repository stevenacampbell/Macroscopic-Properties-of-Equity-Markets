### R Code file to compute discreted quadratic variation
### of ranked log market capitalization

# Load Libraries

library(readr)
library(tidyr)

# Reset Environment

rm(list=ls())

# Import caps

caps <- read_csv("caps_common_1962_2023_ffill.csv",
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import rets

rets <- read_csv("rets_common_1962_2023.csv",
                 col_types =  cols(.default = "d"))
rets <- as.data.frame(rets)

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


QV_incr_ranks<-function(caps,N){
  # Computes (Estimated) Quadratic Variation Increments for ranked caps using:
  # caps: market capitalizations
  # N: Top number of stocks to include
  
  qv <- matrix(0, nrow=N, ncol=ncol(caps))
  
  del<-0 # Track delistings
  for(i in seq(1,ncol(caps)-1,1)){
    
    if(i %% 100 == 1){
      print(i)
      print(paste0("# of Delistings: ", del))
    }
    
    res <- topNCaps(caps,i,N)
    ids <- res[[1]]
    X0 <- caps[ids,i]
    X1 <- caps[ids,i+1]
    del<-del+sum(is.na(X1))
    X1[is.na(X1)]<-X0[is.na(X1)] # Keep same value on delisting
    qv[,i+1] <- (log(X1/X0))^2
  }
  return(qv)
}

N<-500 # Number of stocks
output<-QV_incr_ranks(caps,N) # Compute estimated QV increments
output[abs(output)>0.5]<-0 # Remove outliers
qv_series<-t(apply(output,1,cumsum)) # Compute QV series

# Plot QV as a function of time
color_palette <- colorRampPalette(c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)),alpha=TRUE)(nrow(qv_series))
plot(dates,qv_series[1,],type="l",col=color_palette[1],ylim=c(0,11),xlab="Date", ylab="Value")
for(i in seq(1,N,1)){
  lines(dates,qv_series[i,],type="l",col=color_palette[i])
}



#pdf(file = "QV.pdf", width = 10, height = 5)  
par(mfrow = c(1, 2))
idx <- seq(1, length(dates), by = 10)  # to reduce size of figure
plot(dates[idx], qv_series[N, idx], type = "l",
     xlab = "Time", ylab = "", ylim = c(0, 10),
     main = "Quadratic variations of ranked log market caps")
cols <- gray.colors(N, start = 0.1, end = 0.8)
for (k in N:1) {
  points(dates[idx], qv_series[k, idx], type = "l", col = cols[k])
}

terminal_value <- qv_series[, dim(qv_series)[2]]
plot(1:N, terminal_value, col = cols,
     xlab = "Rank", ylab = "",
     main = "Terminal values")
points(lowess(1:N, terminal_value, 0.2),  # slightly tuned to get better results for small ranks
       type = "l", lwd = 2, col = "blue")
#dev.off()


