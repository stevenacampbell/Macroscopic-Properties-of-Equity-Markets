# Required Libraries
library(readr)

# Reset Environment
rm(list=ls())

### Obtain Annual Change in Entropy, Annual Excess Growth Rate
### and Annual Diversity Drawdown.

## Import Data

# Import caps

caps <- read_csv("Data/caps_common_1962_2023_ffill.csv",
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import rets

rets <- read_csv("Data/rets_common_1962_2023.csv",
                 col_types =  cols(.default = "d"))
rets <- as.data.frame(rets)

# Import dates

dates <-  read_csv("Data/dates_common_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

## Helper Functions

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

drawdown<-function(x){
  # Compute drawdown time series of vector
  r_max<-cummax(x)
  dd<-x-r_max
  return(dd)
}

max_drawdown<-function(x){
  # Compute maximum drawdown of vector
  dd<-drawdown(x)
  return(max(abs(dd)))
}

max_per_drawdown<-function(x){
  # Compute maximum percent drawdown of vector
  dd<-drawdown(x)
  per_dd<-dd/cummax(x)
  return(max(abs(per_dd)))
}

Entropy<-function(weights){
  return(-sum(weights*log(weights)))
}

EGR<-function(weights,returns){
  # Computes EGR
  egr<- log(sum(weights * exp(returns))) - sum(weights * returns)
  return(egr)
}

EGR_ENTR_series<-function(caps,R,N,freq){
  # Computes EGR and Entropy Time Series using:
  # caps: market capitalizations
  # R: Cumulative stock returns
  # N: Top number of stocks to include
  # freq: Sampling frequecy
  
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

## Prep Data

# Convert returns to log scale and obtain cumulative returns
r <- log(1 + rets)
r[is.na(r)]<-0
R <- t(apply(r, 1, cumsum)) # Note: Keeps return the same on exit


## Compute EGR and Entropy for the full history

# Number of stocks (top N)
N<-500

# Daily EGR and Entropy
freq<-1
res<-EGR_ENTR_series(caps,R,N,freq)

cum_egr<-as.vector(c(0,cumsum(res[[1]]))) # cumulative egr
entr<-as.vector(res[[2]]) # entropy

# Illustration
plot(dates,cum_egr,type="l",xlab="Date",ylab=expression(Gamma * "*"),lwd=2)
plot(dates,entr,type="l",xlab="Date",ylab="Entropy",lwd=2)


## Compute EGR and Change in Entropy for each year

# Loop over years
years<-seq(1962,2023,1)

# Number of stocks
N<-500

# Daily EGR and Entropy
freq<-1

ann_ddiv<-rep(0,length(years)) # Change in Diversity/Entropy series
ann_egr_d<-rep(0,length(years)) # Annual Daily EGR series
ann_egr_y<-rep(0,length(years)) # Annual Yearly EGR series

for(i in seq(1,length(years),1)){
  print(paste0("YEAR: ",years[i]))
  
  # Subset period
  start_id<-min(which(dates>=paste0(years[i],"-01-01")))
  end_id<-max(which(dates<=paste0(years[i],"-12-31")))
  
  res<-EGR_ENTR_series(caps[,start_id:end_id],R[,start_id:end_id],N,freq)
  
  egr<-sum(res[[1]]) # cumulative egr
  div<-as.vector(res[[2]]) # diversity
  ddiv<-log(div[length(div)]/div[1])
  ann_ddiv[i]<-ddiv
  ann_egr_d[i]<-egr
  
  # Yearly egr
  max_freq<-end_id-start_id
  res<-EGR_ENTR_series(caps[,start_id:end_id],R[,start_id:end_id],N,max_freq)
  egr_y<-sum(res[[1]]) # cumulative egr
  ann_egr_y[i]<-egr_y
}

# Write output to csv
write_csv(as.data.frame(ann_ddiv), file = "Annual_Change_Entropy.csv")
write_csv(as.data.frame(ann_egr_d), file = "Annual_EGR.csv")
write_csv(as.data.frame(ann_egr_y), file = "Annual_EGR_Yearly.csv")

## Diversity/Entropy Drawdown

div_dd<-rep(0,length(years)) # Annual Diversity/Entropy Drawdown series
ann_ddiv<-rep(0,length(years)) # Change in Diversity/Entropy series

for(i in seq(1,length(years),1)){
  print(paste0("YEAR: ",years[i]))
  
  # Subset period
  start_id<-min(which(dates>=paste0(years[i],"-01-01")))
  end_id<-max(which(dates<=paste0(years[i],"-12-31")))
  
  res<-EGR_ENTR_series(caps[,start_id:end_id],R[,start_id:end_id],N,freq)
  
  egr<-sum(res[[1]]) # cumulative egr
  div<-as.vector(res[[2]]) # entropy
  div_dd[i]<-max_per_drawdown(div) # % Drawdown in diversity
  ddiv<-log(div[length(div)]/div[1])
  ann_ddiv[i]<-ddiv
}

write_csv(as.data.frame(div_dd), file = "Annual_Div_DD.csv") # write output to csv
