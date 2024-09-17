library(readr)

# Reset Environment
rm(list=ls())

### Data Set Specifications

# 5 Stocks
# 21 Days
# One delisting at day 9 with delisting return
# One entrance at day 11
# Dividends every 10 days for 2 of the active stocks

# Load Libraries
source("Backtesting_and_Helper_Functions/AtlasModelFunctions.R")
source("Backtesting_and_Helper_Functions/PortfolioFunctions_CRSP.R")

# Import dates
dates <-  

set.seed(456)

# Initialize Atlas Model Parameters
n<-5 # number of securities
N<-20 # number of time steps
T<-21/252 # terminal time
gamma<-0.1 # Atlas baseline drift
gs<-((-seq(n,1,-1)+(n+1)/2)/((n+1)/2))*0.1 # Atlas drifts (by rank)
sigmas<-(1+2*seq(0,n-1,1)/(n-1))*sqrt(0.1) # volatilities (by rank)
times<-seq(0,T,T/N) # time vector

# Simulate
X0<-1+runif(n)/n
X<-AtlasModel(n,N,T,gamma,gs,sigmas,X0)

# Compute caps and returns
caps<-t(exp(X))
rets<-rbind(rep(NA,n),caps[2:(N+1),]/caps[1:N,]-1)

# Dates
dates_test<-c("1962-01-02", "1962-01-03", "1962-01-04", "1962-01-05", "1962-01-08",
              "1962-01-09","1962-01-10","1962-01-11","1962-01-12","1962-01-15",
              "1962-01-16", "1962-01-17","1962-01-18","1962-01-19","1962-01-22",
              "1962-01-23", "1962-01-24", "1962-01-25","1962-01-26","1962-01-29",
              "1962-01-30")
dates_test<-as.Date(dates_test)

# Create fake permnos
permnos_test<-c(101,102,103,104,105)

# Delisting return at time 9 for stock 1
dlret_test<-matrix(NA, ncol=5,nrow=N+1)
dlret_test[9,1]<--0.1

# Stock 5 inactive for first 10 days
caps[1:10,5]<-NA
rets[1:11,5]<-NA

# Stock 1 delisting after day 9
caps[10:21,1]<-NA
rets[10:21,1]<-NA

# Add Dividend Return for Stock 3 at times 5 and 15 and Stock 5 at time 15
rets[5,3]<-rets[5,3]+0.03
rets[15,3]<-rets[15,3]+0.03
rets[15,5]<-rets[15,5]+0.04

# Plot Caps Data
color_palette <- colorRampPalette(c("blue", "red"),alpha=FALSE)(n)
plot(dates_test,caps[,1],type="l",ylim=c(2.4,4.4),
     col=color_palette[1],ylab="Caps",xlab="Dates")
for(i in seq(1,5,1)){
  lines(dates_test,caps[,i],col=color_palette[i])
}

# Plot Return Data
color_palette <- colorRampPalette(c("blue", "red"),alpha=FALSE)(n)
plot(dates_test,rets[,1],type="p",ylim=c(-0.17,0.17),
     col=color_palette[1],ylab="Return",xlab="Dates")
for(i in seq(1,5,1)){
  points(dates_test,rets[,i],col=color_palette[i])
}

# Plot Delisting Return Data
color_palette <- colorRampPalette(c("blue", "red"),alpha=FALSE)(n)
plot(dates_test,dlret_test[,1],type="p",ylim=c(-0.17,0.17),
     col=color_palette[1],ylab="Delisting Return",xlab="Dates")
for(i in seq(1,5,1)){
  points(dates_test,dlret_test[,i],col=color_palette[i])
}


#Write test data to csv
write_csv(as.data.frame(t(caps)), 
          file = "Test_Data/test_caps.csv",
          col_names=TRUE)

write_csv(as.data.frame(t(rets)), 
          file = "Test_Data/test_rets.csv",
          col_names=TRUE)

write_csv(as.data.frame(t(dlret_test)), 
          file = "Test_Data/test_dlrets.csv",
          col_names=TRUE)

write_csv(as.data.frame(dates_test), 
          file = "Test_Data/test_dates.csv",
          col_names=TRUE)

write_csv(as.data.frame(permnos_test), 
          file = "Test_Data/test_permnos.csv",
          col_names=TRUE)
