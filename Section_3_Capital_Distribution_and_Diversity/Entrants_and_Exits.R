## R Code file to plot entrances and exits


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


weights_entrants_ts<-function(data,dates,window){
  # Determine the weights for entrants to the market
  # data: data frame of caps with time as columns
  # dates: dates for the data
  # window: lookback window (if a stock has entered after being absent for
  #         at least this number of periods we consider it a true entrance.)
  
  N <- ncol(data)
  relevant_weights <- c()
  relevant_times <- c()
  
  # Loop through columns
  for (i in 1:N) {
    if(i %% 100==1){
      print(as.Date(dates[i]))
    }
    if(i>=window+1){
      
      # Check entrant condition
      for(j in seq(i-window,i-1,1)){
        if(j==i-window){
          logical<- (data[,j]<=0)
        }else{
          logical<- (data[,j]<=0) & logical
        }
      }
      logical<- (data[,i]>0) & logical
      
      # Identify rows where the criteria is met
      indices <- which(logical)
      
      # Compute weights
      v<-data[,i]
      v[v<=0]<-0
      mu<- v/sum(v)
      weights<- mu[indices]
      
      # Retrieve ranks for the identified rows
      relevant_weights <- c(relevant_weights, c(weights))
      relevant_times <- c(relevant_times, c(rep(i,length(weights))))
    }
  }
  return(list(relevant_weights,relevant_times))
}

weights_exits_ts<-function(data,dates,window){
  # Determine the weights for exits from the market
  # data: data frame of caps with time as columns
  # dates: dates for the data
  # window: lookforward window (if a stock has exited for at least
  #         this number of periods we consider it a true exit.)
  N <- ncol(data)
  relevant_weights <- c()
  relevant_times <- c()
  
  # Loop through columns
  for (i in 1:(N-1)) {
    
    if(i %% 100==1){
      print(as.Date(dates[i]))
    }
    
    if(i<=N-window){
      
      # Check exit condition
      for(j in seq(i+1,i+window,1)){
        if(j==i+1){
          logical<- (data[,j]<=0)
        }else{
          logical<- (data[,j]<=0) & logical
        }
      }
      logical<- (data[,i]>0) & logical
      
      # Identify rows where the criteria is met
      indices <- which(logical)
      
      # Compute weights
      v<-data[,i]
      v[v<=0]<-0
      mu<- v/sum(v)
      weights<- mu[indices]
      
      # Retrieve ranks for the identified rows
      relevant_weights <- c(relevant_weights, c(weights))
      relevant_times <- c(relevant_times, c(rep(i,length(weights))))
      
    }
    
  }
  return(list(relevant_weights,relevant_times))
}


caps[is.na(caps)]<-0 # Replace NAs with 0 to be compatible with function
wind<-60 # lookback/lookforward window length for entrants/exits

# Obtain results
entr_output <-weights_entrants_ts(caps,dates,wind)
entr_output

exit_output <-weights_exits_ts(caps,dates,wind)
exit_output

# Number of entrants/exits
length(entr_output[[1]])
length(exit_output[[1]])


## Plot entrant and exit weights on log scale
max_val<- max(log10(exit_output[[1]]),log10(entr_output[[1]]))
min_val<- min(log10(exit_output[[1]]),log10(entr_output[[1]]))

#pdf(file = "evolution.pdf", width = 10, height = 6)
par(mfrow = c(1, 2))
plot(dates[entr_output[[2]]], log10(entr_output[[1]]),
     col=rgb(0, 1, 0, 0.25), pch=15, cex = 0.2, ylim=c(min_val,max_val),
     ylab = "Weights", xlab="Date", yaxt="n", main = "Entrances")
axis(2, at=c(-2,-4,-6,-8), labels=c(latex2exp::TeX("$10^{-2}$"),latex2exp::TeX("$10^{-4}$"),
                                    latex2exp::TeX("$10^{-6}$"),latex2exp::TeX("$10^{-8}$")))

plot(dates[exit_output[[2]]], log10(exit_output[[1]]),
     col=rgb(1, 0, 0, 0.25), pch=15, cex = 0.2,
     ylim=c(min_val,max_val), ylab = "Weights", xlab="Date",yaxt="n",
     main = "Exits")
axis(2, at=c(-2,-4,-6,-8), labels=c(latex2exp::TeX("$10^{-2}$"),latex2exp::TeX("$10^{-4}$"),
                                    latex2exp::TeX("$10^{-6}$"),latex2exp::TeX("$10^{-8}$")))
#dev.off()
