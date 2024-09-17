## R Code file to plot excess growth rate 
## sampled at different frequencies


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


EGR<-function(weights,returns){
  # Computes EGR
  egr<- log(sum(weights * exp(returns))) - sum(weights * returns)
  return(egr)
}

EGR_series<-function(caps,R,N,freq){
  # Computes EGR Time Series using:
  # caps: market capitalizations
  # R: Cumulative stock returns
  # N: Top number of stocks to include
  # freq: Sampling frequecy
  
  end_id <- ncol(caps)
  times <- seq(1,end_id-freq,freq)
  egr_ts <- rep(0,length(times))
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
    j<-j+1
  }
  return(egr_ts)
}



market_return_series<-function(caps,R,N,freq){
  # Computes weighted average return (using cap weights)
  # caps: market capitalizations
  # R: Cumulative stock returns
  # N: Top number of stocks to include
  # freq: Sampling frequecy
  
  end_id <- ncol(caps)
  times <- seq(1,end_id-freq,freq)
  mr_ts <- rep(0,length(times))
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
    mr_ts[j]<-log(sum(weights*exp(returns)))
    j<-j+1
  }
  return(mr_ts)
}







# Convert returns to log scale and obtain cumulative returns
r <- log(1 + rets)
r[is.na(r)]<-0
R <- t(apply(r, 1, cumsum)) # Note: Keeps return the same on exit

# Number of stocks
N<-1000

# Daily EGR
freq<-1
egr_day<-EGR_series(caps,R,N,freq)
date_id_day<-seq(1,ncol(caps)-freq,freq)

# Weekly EGR
freq<-5
egr_week<-EGR_series(caps,R,N,freq)
date_id_week<-seq(1,ncol(caps)-freq,freq)
mr_week<-market_return_series(caps,R,N,freq)


# to get growth of EGR per year
library(lubridate)
whole_interval <- decimal_date(dates[length(dates)]) - decimal_date(dates[1])
sum(egr_week)/whole_interval  # for weekly case

plot(egr_week, mr_week^2)  # there are outliers
cor(egr_week, mr_week^2)  # usual correlation
library(robustHD)
corHuber(egr_week, mr_week^2)  # winsorized correlation
help(corHuber)


# Month EGR
freq<-20
egr_month<-EGR_series(caps,R,N,freq)
date_id_month<-seq(1,ncol(caps)-freq,freq)

# Quarter EGR
freq<-60
egr_quarter<-EGR_series(caps,R,N,freq)
date_id_quarter<-seq(1,ncol(caps)-freq,freq)


# Plot EGR

# Recession dates to highlight
# source: https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States#Great_Depression_onward_(1929%E2%80%93present)
recession_start <- as.Date(c("1969-12-01",
                     "1973-11-01",
                     "1980-01-31",
                     "1981-07-31",
                     "1990-07-31",
                     "2001-03-31",
                     "2007-12-31",
                     "2020-02-29"))
recession_end   <- as.Date(c("1970-11-30",
                     "1975-03-31",
                     "1980-07-31",
                     "1982-11-30",
                     "1991-03-31",
                     "2001-11-30",
                     "2009-06-30",
                     "2020-04-30"))
recession_names <- c("Recession of 1969-1970",
                     "1973-1975 recession",
                     "1980 recession",
                     "1981-1982 recession",
                     "Early 1990s recession",
                     "Early 2000s recession",
                     "Great Recession",
                     "COVID-19 recession")

min_val<- min(egr_day,egr_week,egr_month,egr_quarter)
max_val<- max(egr_day,egr_week,egr_month,egr_quarter)

#pdf(file = "EGR.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
# Plot Cumulative EGR
plot(dates[date_id_quarter], cumsum(egr_quarter),type="l",
     main="Cumulative excess growth rate", 
     xlab="Time", ylab="", col="white",
     xlim=c(dates[1], dates[length(dates)]), ylim=c(0,2.2), lwd=2)
# Higlight recession periods
for(i in seq(1,length(recession_start),1)){
  rect(xleft = recession_start[i], xright = recession_end[i], ybottom = -0.2, 
       ytop = 2.5, col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)
}
lines(dates[date_id_quarter],cumsum(egr_quarter), col=rgb(0.75,0.75,0.75,1),lwd=1)
lines(dates[date_id_month],cumsum(egr_month), col=rgb(0.5,0.5,0.5,1),lwd=1)
lines(dates[date_id_week],cumsum(egr_week), col=rgb(0.25,0.25,0.25,1),lwd=1)
lines(dates[date_id_day],cumsum(egr_day), col=rgb(0,0,0,1),lwd=1)
legend("topleft", legend = c(expression(Delta*t*" = "*1),
                             expression(Delta*t*" = "*5),
                             expression(Delta*t*" = "*20),
                             expression(Delta*t*" = "*60)),
       lty=c(1,1,1,1),
       col = c(rgb(0,0,0,1),rgb(0.25,0.25,0.25,1),
               rgb(0.5,0.5,0.5,1),rgb(0.75,0.75,0.75,1)),
       lwd=c(1,1,1,1))

# Plot per period EGR
plot(dates[date_id_quarter],egr_quarter,type="l",
     ylab="", xlab="Time",col="white",
     main="Per period excess growth rate",
     ylim=c(0,max_val), lty = 1)
# Higlight recession periods
for(i in seq(1,length(recession_start),1)){
  rect(xleft = recession_start[i], xright = recession_end[i], ybottom = 0, 
       ytop = max_val, col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)
}
lines(dates[date_id_quarter],egr_quarter,col=rgb(0.75,0.75,0.75,1))
lines(dates[date_id_month],egr_month,col=rgb(0.5,0.5,0.5,1))
lines(dates[date_id_week],egr_week,col=rgb(0.25,0.25,0.25,1))
lines(dates[date_id_day],egr_day,col=rgb(0,0,0,1))
#dev.off()


#pdf(file = "EGR_graph.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
acf(egr_month, main = "ACF of excess growth rate")
breaks <- seq(0.9*log10(min(egr_month)), log10(1.5*max(egr_month)), by = 0.03)
hist(log10(egr_month), col = rgb(1, 0, 0, 0.2), # semi-transparent
     ylim=c(0,2.5),xlim = c(-3.3, -1),
     breaks=50,
     probability = TRUE, # density scale
     main = "Unconditional distribution",
     xlab = "Normalized EGR (Log Scale)",
     ylab = "Density",
     xaxt = 'n') # suppress x-axis
# Define the positions of the tick marks
tick_positions <- c(-3.5, -3, -2.5, -2, -1.5)
# Define the labels in the form 10^x
labels <- sapply(tick_positions, function(x) as.expression(bquote(10^.(x))))
# Add custom x-axis with the desired labels
axis(1, at = tick_positions, labels = labels)
#dev.off()
