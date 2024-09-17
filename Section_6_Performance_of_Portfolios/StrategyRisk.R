# Required Libraries
library(readr)

# Reset Environment
rm(list=ls())

## Code to plot strategy risk figures

# Define the sequence of years and p-values
years <- seq(1962, 2023, 1)
pvals <- seq(0.0, 1, 0.01)
freq <- 20
tcb <- 0.0025

# Create an empty list to store data
data_list <- list()

# Loop through each year and p-value to read the data
for (i in seq(1,length(years),1)) {
  for (j in seq_along(pvals)) {
    year <- years[i]
    p <- pvals[j]
    
    # Print the current year and p-value
    print(paste0("YEAR: ", year, ", P=", p))
    
    # Construct the file path
    file_path <- paste0("Backtest_Outputs/lV_tc_", tcb * 10000, "bps", "_freq_",
                        freq, "_p_", p, "_", year, "_", years[i], ".csv")
    
    # Read the data from the CSV file
    data <- read_csv(file_path, col_types = cols(.default = "d"))
    data <- as.data.frame(data)[[1]]
    
    # Store the data in the list with a descriptive name
    data_list[[paste0("year_", year, "_p_", p)]] <- data
  }
}

# Function to query the data by year and p-value
get_data <- function(year, p) {
  data_key <- paste0("year_", year, "_p_", p)
  if (data_key %in% names(data_list)) {
    return(data_list[[data_key]])
  } else {
    return(NULL)
  }
}

## Helper Functions

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

sharpe_ratio<-function(x){
  # Compute sharpe ratio from return series
  m<-mean(x)
  s<-sd(x)
  return(m/s)
}


## Compute max (annual) percent drawdown of diversity portfolio for each p
max_dd<-matrix(0,ncol=length(pvals),nrow=length(seq(1,length(years),1)))
id<-0
for (i in seq(1,length(years),1)) {
  id<-id+1
  for (j in seq_along(pvals)) {
    lV<-get_data(years[i], pvals[j])
    max_dd[id,j]<-max_per_drawdown(10^(lV))
  }
}
rownames(max_dd)<-years[seq(1,length(years),1)]
colnames(max_dd)<-pvals

# Compute (annual) Sharpe ratio of diversity portfolio for each p
sr<-matrix(0,ncol=length(pvals),nrow=length(seq(1,length(years),1)))
id<-0
for (i in seq(1,length(years),1)) {
  id<-id+1
  for (j in seq_along(pvals)) {
    lV<-get_data(years[i], pvals[j])
    period_log_ret<-lV-lV[1]
    sr[id,j]<-sharpe_ratio(diff(period_log_ret))
  }
}
rownames(sr)<-years[seq(1,length(years),1)]
colnames(sr)<-pvals

## Plot Sharpe Ratio vs. Annual Drawdown

plot(NULL, NULL, ylim=c(log10(0.02), log10(0.64)), xlim=c(-0.11, 0.26), 
     xlab="Sharpe ratio", ylab="Max drawdown (log scale)", yaxt="n",
     cex.lab=1.3, cex.axis=1.1)

# Define the labels and their positions
y_ticks <- c(0.02, 0.04, 0.08, 0.16, 0.32,0.64)
y_labels <- as.character(paste0(y_ticks*100,"%"))
axis(2, at=log10(y_ticks), labels=y_labels, cex.axis=1.1)

# Plot regression lines
abline(lm(log10(max_dd[,50])~sr[,50]), col="black", lwd=2)
abline(lm(log10(max_dd[,1])~sr[,1]), col="black", lwd=2, lty=2)
abline(lm(log10(max_dd[,101])~sr[,101]), col="black", lwd=2, lty=3)

# Plot points
color_palette <- colorRampPalette(c("blue", "red"),alpha=0.05)(length(pvals))
for(j in seq(1, length(years), 1)) {
  for(i in seq(1, length(pvals), 1)) {
    points(sr[j,i], log10(max_dd[j,i]), col=color_palette[i], pch=19, cex=0.5)
  }
}
for(j in seq(1,length(years),1)){
  points(sr[j,1], log10(max_dd[j,1]),col="black",pch=1,cex=1,lwd=1)
  points(sr[j,101], log10(max_dd[j,101]),col="black",pch=4,cex=1,lwd=1)
}
legend("bottomleft", legend=c("p=0", "p=0.5", "p=1"), 
       col="black", lwd=2, lty=c(2, 1, 3),cex=1.3)


## Plot Diversity/Entropy Drawdown vs. Annual Drawdown

# Load drawdown in diversity/entropy
div_dd<-as.data.frame(read_csv("Annual_Div_DD.csv",col_types =  cols(.default = "d")))[[1]]

color_palette <- colorRampPalette(c(rgb(0.0,0.0,1.0,0.5), rgb(1.0,0.0,0.0,0.5)), alpha = TRUE)(length(pvals))

plot(NULL,NULL,ylim=c(log10(0.02),log10(0.64)),xlim=c(log10(0.003),log10(0.045)),
     xlab="Diversity drawdown (log scale)",ylab="Max drawdown (log scale)",
     xaxt="n",yaxt="n",cex.lab=1.3, cex.axis=1.1)

# Define the labels and their positions
y_ticks <- c(0.02, 0.04, 0.08, 0.16, 0.32,0.64)
y_labels <- as.character(paste0(y_ticks*100,"%"))
axis(2, at=log10(y_ticks), labels=y_labels,cex.axis=1.1)
x_ticks <- c(0.005, 0.01, 0.02, 0.04)
x_labels <- as.character(paste0(x_ticks*100,"%"))
axis(1, at=log10(x_ticks), labels=x_labels,cex.axis=1.1)

# Add regression lines
abline(lm(log10(max_dd[,50])~log10(div_dd)),col="black",lwd=2)
abline(lm(log10(max_dd[,1])~log10(div_dd)),col="black",lwd=2,lty=2)
abline(lm(log10(max_dd[,101])~log10(div_dd)),col="black",lwd=2,lty=3)

# Plot points
for(j in seq(1,length(years),1)){
  for(i in seq(1,length(pvals),1)){
    points(log10(div_dd[j]),log10(max_dd[j,i]),col=color_palette[i],pch=19,cex=0.5)
  }
}
for(j in seq(1,length(years),1)){
    points(log10(div_dd[j]),log10(max_dd[j,1]),col="black",pch=1,cex=1,lwd=1)
    points(log10(div_dd[j]),log10(max_dd[j,101]),col="black",pch=4,cex=1,lwd=1)
}
legend("bottomright", legend=c("p=0", "p=0.5", "p=1"), 
       col="black", lwd=2, lty=c(2, 1, 3),cex=1.3)

