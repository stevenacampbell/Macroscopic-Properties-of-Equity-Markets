### R Code file to plot joint behaviour of Diversity & EGR

# Load Libraries

library(readr)
library(tidyr)
library(ggplot2)
library(plotly)

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

Diversity<-function(weights,p){
  # p Diversity funciton
  return(sum(weights^p)^(1/p))
}

Entropy<-function(weights){
  -sum(weights*log(weights), na.rm = TRUE)
}

EGR<-function(weights,returns){
  # Computes EGR
  egr<- log(sum(weights * exp(returns))) - sum(weights * returns)
  return(egr)
}

EGR_DIV_series<-function(caps,R,N,freq){
  # Computes EGR and Diversity Time Series using:
  # caps: market capitalizations
  # R: Cumulative stock returns
  # N: Top number of stocks to include
  # freq: Sampling frequecy
  # p: Diversity p
  
  end_id <- ncol(caps)
  times <- seq(1,end_id-freq,freq)
  egr_ts <- rep(0,length(times))
  div_ts <- rep(0,length(times)+1)
  j<-1
  
  for(i in times){
    
    if(i %% 100 == 1){
      print(i)
    }
    
    res <- topNCaps(caps,i,N)
    ids <- res[[1]]
    mkt_caps <- res[[2]]
    weights <- mkt_caps/sum(mkt_caps)
    R_init <- R[ids,i]
    R_end <- R[ids,i+freq]
    returns <- R_end - R_init
    egr_ts[j]<-EGR(weights,returns)
    div_ts[j]<-Entropy(weights)
    j<-j+1
  }
  res <- topNCaps(caps,end_id,N)
  ids <- res[[1]]
  mkt_caps <- res[[2]]
  weights <- mkt_caps/sum(mkt_caps)
  div_ts[j]<-Entropy(weights)
  
  return(list(egr_ts,div_ts))
}




# Convert returns to log scale and obtain cumulative returns
r <- log(1 + rets)
r[is.na(r)]<-0
R <- t(apply(r, 1, cumsum)) # Note: Keeps return the same on exit

# Number of stocks
N<-1000


# Daily EGR and Diversity
freq<-1
res<-EGR_DIV_series(caps,R,N,freq)
cum_egr<-as.vector(c(0,cumsum(res[[1]]))) # cumulative egr
div<-as.vector(res[[2]]) # diversity

#pdf(file = "joint.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
k <- 1
indices<-seq(1, length(div), by = k)
cum_egr_idx<-cum_egr[indices]
x <- diff(div[indices])  # change in log entropy
y <- diff(cum_egr_idx)  # excess growth rate
plot(x, y,
     xlim = c(-0.025, 0.025), ylim = c(0, 0.0025),
     xlab = "Change of entropy",
     ylab = expression("Excess growth rate ("*gamma*")"),
     main = expression(Delta*"t = 1"))
local_fit <- lowess(x, y)  # local regression fit
points(local_fit$x, local_fit$y, type = "l", col = "blue")

k <- 20
indices<-seq(1, length(div), by = k)
cum_egr_idx<-cum_egr[indices]
y <- diff(cum_egr_idx)  # cumulative excess growth rate
length(y)
range_seq <- numeric(length(y))
for (i in 1:length(range_seq)) {
  max_i <- max(div[(1 + k*(i - 1)):min(k*i, length(div))])
  min_i <- min(div[(1 + k*(i - 1)):min(k*i, length(div))])
  #cat(max_i, " ", min_i, "\n")
  range_seq[i] <- max_i - min_i
}
x <- range_seq
plot(x, y,
     xlim = c(0, 0.1), ylim = c(0, 0.015),
     xlab = expression("Range of entropy"),
     ylab = expression("Cumulative excess growth rate ("*Delta*Gamma*")"),
     main = expression(Delta*"t = 20"))
local_fit <- lowess(x, y)
points(local_fit$x, local_fit$y, type = "l", col = "blue")

#dev.off()



