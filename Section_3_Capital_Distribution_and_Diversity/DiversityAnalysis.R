## R Code file to examine behaviours of market diversity


# Load Libraries

library(readr)
library(tidyr)

#Reset Environment

rm(list=ls())


###IMPORT DATA

# Import caps

caps <- read_csv("caps_common_1962_2023_ffill.csv",
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import dates

dates <-  read_csv("dates_common_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

# Function to get the top caps and their indices
topNCaps <- function(mat, j, N) {
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

# Function to create (sub) market weights
GetSubMktWeights <- function(x){
  return(x/sum(x))
}

# Shannon entropy
Diversity <- function(x){
  return(-sum(x*log(x), na.rm = TRUE))
}

# Function to obtain diversity of a set of capitalizations
EvaluateDiversityfromCaps <- function(x){
  mu <- GetSubMktWeights(x)
  return(Diversity(mu))
}

# Function to evaluate diversity over time (fixed securities per period)
EvaluateDiversityOverTime <- function(mat, M, N) {
  # Calculate the number of time periods
  periods <- ceiling(ncol(mat)/M)
  
  # Initialize an array to store the results
  results <- matrix(0,nrow=periods,ncol=M)
  
  # Loop over the time periods
  for (i in seq_len(periods)) {
    # Calculate the start and end columns for this period
    start <- (i-1)*M + 1
    end <- min(i*M, ncol(mat))
    
    # Get the top N caps and their indices from this column
    top <- topNCaps(mat, start, N)
    
    ids <- top[[1]]
    
    cap_series <- mat[ids,start:end]
    cap_series[cap_series < 0] <- 0
    
    for (j in 1:(end - start + 1)){
      # Calculate the diversity of the top N caps for this period
      diversity <- EvaluateDiversityfromCaps(cap_series[,j])
      
      # Store the result
      results[i,j] <- diversity
    }
    
  }
  
  return(results)
}

# Function to evaluate diversity over time where the top stocks are renewed
EvaluateRebalancedDiversityOverTime <- function(mat, M, N) {
  # Calculate the number of time periods
  periods <- ceiling(ncol(mat)/M)
  
  total_days <- ncol(mat)
  
  # Initialize an array to store the results
  results <- matrix(0,ncol=total_days,nrow=1)
  
  # Loop over the time periods
  for (i in seq_len(total_days)) {
    
    # Get the top N caps and their indices from this column
    top <- topNCaps(mat, i, N)
    
    ids <- top[[1]]
    
    cap_vals <- top[[2]]
    
    results[1,i]<-EvaluateDiversityfromCaps(cap_vals)
    
  }
  
  return(results)
}

# Function to obtain diversity of the full market
EvaluateFullMarketDiversityOverTime <- function(mat, M) {
  # Calculate the number of time periods
  periods <- ceiling(ncol(mat)/M)
  total_days <- ncol(mat)
  
  # Initialize an array to store the results
  results <- matrix(0,ncol=total_days,nrow=1)
  
  # Loop over the time periods
  for (i in seq_len(total_days)) {
    
    cap_vals <- mat[,i]
    cap_vals[cap_vals < 0] <- 0
    
    results[1,i]<-EvaluateDiversityfromCaps(cap_vals)
    
  }
  
  return(results)
}

# Evaluate diversity over time of a random collection
EvaluateDiversityOverTime_Random <- function(mat, M, N, k) {
  # Calculate the number of time periods
  periods <- ceiling(ncol(mat)/M)
  
  # Initialize an array to store the results
  results <- matrix(0,nrow=periods,ncol=M)
  
  # Loop over the time periods
  for (i in seq_len(periods)) {
    # Calculate the start and end columns for this period
    start <- (i-1)*M + 1
    end <- min(i*M, ncol(mat))
    
    # Get the top N caps and their indices from this column
    top <- topNCaps(mat, start, N)
    
    ids <- top[[1]]
    
    sample_ids <- sample(ids, size = k)
    
    cap_series <- mat[sample_ids,start:end]
    cap_series[cap_series < 0] <- 0
    
    for (j in 1:(end - start + 1)){
      # Calculate the diversity of the top N caps for this period
      diversity <- EvaluateDiversityfromCaps(cap_series[,j])
      
      # Store the result
      results[i,j] <- diversity
    }
    
  }
  
  return(results)
}


# Figure Parameters
M <- ceiling(dim(caps)[2]/4)  # days (defining a measurement 'period' in the data)
N <- 500 # Top N stocks
N_R <- 1000 # Top N universe of stocks (for random sampling)
k <- 500 # sample size (for random sampling)
n <- 15 # number of samples (for random selection)
set.seed(111) # set seed for sampling


# Create Figure
total_periods <- ceiling(ncol(caps)/M)


selected_dates<-dates
div_list <-list()
caps[is.na(caps)]<-0

for (j in seq(1,n,1)){
  diversities<-EvaluateDiversityOverTime_Random(caps,M,N_R,k)
  
  div_list<-c(div_list,list(diversities))
}

main_diversities<-EvaluateDiversityOverTime(caps,M,N)
diversities_rebal <-EvaluateRebalancedDiversityOverTime(caps,M,N)
diversity_full<-EvaluateFullMarketDiversityOverTime(caps, M)

#pdf(file = "diversity.pdf", width = 10, height = 8)
cex_text <- 1.3
cex_lab <- 1.2
cex_axis <- 1.3
par(mfrow=c(2,1))
par(mai=c(0.5,0.5,0.1,0.1), mgp=c(0.5,0.5,0))
y_min <- 5.4
y_max <- 7
plot(selected_dates,diversity_full ,type="l", ylim=c(y_min,y_max),
     lwd=2, ylab="", xlab="",yaxt="n", xaxt="n")
axis.Date(1,at=pretty(range(selected_dates)),tck=-0.03,
          cex.axis=cex_axis,cex.lab=cex_lab)
axis(2,at=pretty(c(y_min,y_max)),tck=-0.03,
     cex.axis=cex_axis,cex.lab=cex_lab)
mtext("Time",side=1, line=1, cex=cex_text)
mtext("Entropy",side=2, line=1.5, at=(y_min + y_max)/2, cex=cex_text)
par(mai=c(0.5,0.5,0.1,0.1))
y_min <- 4.1
y_max <- 5.8
plot(NULL, NULL, ylab="", xlab="", ylim=c(y_min,y_max), xlim=range(selected_dates),xaxt="n",yaxt="n")
lines(selected_dates,diversities_rebal, col="black", lwd=2, type="l")
for(j in seq(1,n,1)){
  div_vals<-div_list[[j]]
  for(i in seq(1,total_periods,1)){
    start <- (i-1)*M + 1
    end <- min(i*M, ncol(caps))
    lines(selected_dates[start:end],div_vals[i,1:(end-start+1)], col=rgb(0, 0, 0, 0.25), type="l")
  }
}
for(i in seq(1,total_periods,1)){
  start <- (i-1)*M + 1
  end <- min(i*M, ncol(caps))
  lines(selected_dates[start:end],main_diversities[i,1:(end-start+1)], col="red", type="l")
}
axis(2,at=pretty(c(y_min,y_max)),tck=-0.03,
     cex.axis=cex_axis,cex.lab=cex_lab)
axis.Date(1,at=pretty(range(selected_dates)),tck=-0.03,
          cex.axis=cex_axis,cex.lab=cex_lab)
mtext("Entropy",side=2, line=1.5, at=(y_min + y_max)/2,
      cex=cex_text)
mtext("Time",side=1, line=1,
      cex=cex_text)
#dev.off()
