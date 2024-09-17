## R Code file to extract and plot annual summary features
## of the CRSP data set including:
## - number of entrances and exits
## - total number of traded assets
## - minimum number of assets required to cover a given percentage of
##   total market cap


# Load Libraries
library(readr)
library(tidyr)
library(zoo)

# Reset Environment
rm(list=ls())

## Helper Functions

CountTopPercentMktCap <- function(mat, percent) {

  # Function to compute the minimum number of securities needed to
  # account for a percentage of the total market capitalization.
  # Input: capitalization time series data and desired percentage.
  # Output: daily threshold number and total market cap.
  
  # Calculate the number of time periods
  total_days <- ncol(mat)
  
  # Initialize an array to store the results
  results <- matrix(0,ncol=total_days,nrow=2)
  
  # Loop over the time periods
  for (i in seq_len(total_days)) {
    
    cap_vals <- mat[,i]
    cap_vals[cap_vals<0]<-0
    cap_vals_sorted <- sort(cap_vals, decreasing = TRUE)
    mktcap <- sum(cap_vals_sorted)
    weights <- cap_vals_sorted/mktcap
    cum_weight<-cumsum(weights)
    is_below_per <- (cum_weight<percent)
    
    results[1,i]<-sum(is_below_per)+1 #minimum number of stocks to exceed percent threshold
    results[2,i]<-mktcap 
  }
  rownames(results)<-c("Number to Exceed Threshold", "Mkt Cap")
  return(results)
}


getMarketStats <- function(stock_matrix) {
  
  # Function that returns summary statistics about market data.
  # Input: A matrix of capitalizations or stock prices for each day
  # Output: The daily number of active, newly listed and delisted stocks.
  
  # Get dimensions
  num_days <- ncol(stock_matrix)
  num_stocks <- nrow(stock_matrix)
  
  # Replace NAs with 0s
  stock_matrix[is.na(stock_matrix)]<-0
  
  # Preallocate result lists
  stocks_traded <- integer(num_days)
  stocks_stopped <- integer(num_days)
  stocks_began <- integer(num_days)
  
  for (i in 2:num_days) {
    # Get traded stocks
    stocks_traded[i] <- sum(stock_matrix[, i] > 0)
    
    # Get stopped stocks
    stopped_stocks <- (stock_matrix[, i-1] > 0) & (stock_matrix[, i]<=0)
    stocks_stopped[i] <- sum(stopped_stocks)
    
    # Get started stocks
    began_stocks <- (stock_matrix[, i-1] <= 0) & (stock_matrix[, i] > 0)
    stocks_began[i] <- sum(began_stocks)
  }
  
  # Make a data frame with market stats
  market_stats <- data.frame(Day = 2:num_days, # first day not included
                              Traded = stocks_traded[-1],
                              Stopped = stocks_stopped[-1],
                              Began = stocks_began[-1])
  
  return(market_stats)
}

sumEventsEveryKDays <- function(events, k) {
  # Function to aggregate a vector that contains daily event counts.
  # Input: Vector of event counts per day and aggregation window (k days)
  # Output: Vector of total events every k days
  
  # Check if the length of events is a multiple of k
  remainder <- length(events) %% k
  
  # If not, pad the events vector with zeros
  if (remainder != 0) {
    padding <- numeric(k - remainder)
    events <- c(events, padding)
  }
  
  # Reshape the data into a matrix with k columns
  event_matrix <- matrix(events, ncol = k, byrow = TRUE)
  
  # Sum each row to get the total events every k days
  events_k_days <- rowSums(event_matrix)
  
  return(events_k_days)
}

# Load forward filled capitalizations
caps_ffill <- read_csv("caps_common_1962_2023_ffill.csv", 
                 col_types =  cols(.default = "d"))
caps_ffill <- as.data.frame(caps_ffill)


# Import dates
dates <-  read_csv("dates_common_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

# Get output for number of stocks accounting for 90% of the market cap
out_counts<-CountTopPercentMktCap(caps_ffill, 0.9)
#plot(out_counts[1, ])



# Get data for newly listed, delisted, and active securities each day
traded_securities_stats<-getMarketStats(caps_ffill)
traded_securities_stats

# Store the number of securities that listed/delisted
listed<-traded_securities_stats[[4]]
delisted<-traded_securities_stats[[3]]

# Aggregate the delistings over 250 trading days (~ 1 trading year)
k_days<-250
yearly_listed<-sumEventsEveryKDays(listed,k_days)
yearly_delisted<-sumEventsEveryKDays(delisted,k_days)

# Aggregate by year
years_from_dates <- as.numeric(substr(dates[-1], 1, 4))
first_year <- years_from_dates[1]
last_year <- years_from_dates[length(years_from_dates)]
idx_seq <- numeric(last_year - first_year + 1)  # first index for each year
for (i in 1:length(idx_seq)) {
  year_i <- first_year + i - 1
  idx_seq[i] <- match(year_i, years_from_dates)
}
yearly_listed_new <- numeric(last_year - first_year + 1)
yearly_delisted_new <- numeric(last_year - first_year + 1)
for (i in 1:length(idx_seq)) {
  idx_begin <- idx_seq[i]
  if (i == length(idx_seq)) {
    idx_end <- length(years_from_dates - 1) # first day not included
  } else {
    idx_end <- idx_seq[i + 1] - 1
  }
  yearly_listed_new[i] <- sum(listed[idx_begin:idx_end])
  yearly_delisted_new[i] <- sum(delisted[idx_begin:idx_end])
}





## Create summary plot for the data

# Save default margin size
old_mar <- par("mar")

# Set up the secondary axis
pdf(file = "universe.pdf", width = 6, height = 5)
par(mar = c(5.1,4.1,4.1,2.1))
plot(NULL, NULL, xlim = c(1960, 2024), ylim = c(-1000, 8000),
     xlab = "Time", ylab = "", main = "Number of securities")
library("lubridate")
# listed and delisted
for (i in 1:(last_year - first_year + 1)) {
  year_i <- first_year + i - 1
  rect(year_i, 0, year_i + 1, yearly_listed_new[i], density = NULL, angle = 45,
       col = "green", border = "black", lty = 1, lwd = 1)
  rect(year_i, -yearly_delisted_new[i], year_i + 1, 0, density = NULL, angle = 45,
       col = "red", border = "black", lty = 1, lwd = 1)
}
# total number
points(decimal_date(dates[2:length(dates)]), traded_securities_stats[[2]],
       col = "black", type = "l", lwd = 2)
# number to cover 90%
points(decimal_date(dates[2:length(dates)]), out_counts[1,2:ncol(out_counts)], 
       col = "dark grey", type = "l", lwd = 1)
dev.off()



