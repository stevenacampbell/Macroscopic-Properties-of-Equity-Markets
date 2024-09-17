## R Code file to illustrate trajectories of ranks over time



# Load Libraries

library(readr)
library(tidyr)
library(grDevices)

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

# Start and End Indices for Analysis

start<-min(which(dates>="2010-01-01"))
end<-max(which(dates<="2020-12-31"))


# Plot settings
y_max<-6000 # Maximum rank for y-axis of plot
date_id<- start:end




# plot results
pdf(file = "rank_trajectories.pdf", width = 6, height = 5)
### PLOT FIRST SET OF RANKS
# Select Rank trajectories to plot
start_rank<-1
end_rank<-50
# Select the indices of the rows
rank_indices <- order(-caps[, start])[start_rank:end_rank]
# Default Times
d_times<-rep(NA,end_rank-start_rank+1)
for(j in seq(1,end_rank-start_rank+1,1)){
  test<-sum(is.na(caps[rank_indices[j],start:end]))
  if(test>0){
    d_times[j]<-min(which(is.na(caps[rank_indices[j],start:end])))
  }
}
# Rank the next columns and track the rank of the indices
results <- matrix(0, nrow = end_rank-start_rank+1, ncol = end-start+1)
for (j in start:end) {
  if (j <= ncol(caps)) {
    column_ranks <- rank(-caps[, j], ties.method = "min")
    results[, j-start+1] <- column_ranks[rank_indices]
  }
}
# Color gradient
blue_gradient <- colorRampPalette(c("darkblue", "lightblue"))(50)
blue_gradient <- adjustcolor(blue_gradient, alpha.f = 0.35)

indices <- which(results[1,] <= y_max) # plot elements before they drop too low


par(mfrow = c(1,1))
plot(dates[date_id[indices]],log10(results[1,indices]),
     type="l", col=blue_gradient[1],
     ylim=c(log10(y_max),log10(1)),xlim=c(dates[start],dates[end]),
     yaxt="n",cex=0.5,ylab="Rank", xlab="Time",
     main="Trajectories of ranks")

for(j in seq(1,end_rank-start_rank+1,1)){
  
  indices <- which(results[j,] <= y_max)
  
  # Plot X where a stock delists and normal lines otherwise
  if(!is.na(d_times[j])){
    
    indices <- indices[indices<d_times[j]]
    
    lines(dates[date_id[indices]],log10(results[j,indices]), col=blue_gradient[j],cex=0.5)
    points(dates[date_id[d_times[j]-1]], log10(results[j,d_times[j]-1]), 
           pch=4, lwd=2, col="black", cex=0.5)
    
  }else{
    
    lines(dates[date_id[indices]],log10(results[j,indices]), col=blue_gradient[j],cex=0.5)
  
  }
}

axis(2, at = log10(c(10000,1000,100,10,1)),labels=c(10000,1000,100,10,1))

### PLOT SECOND SET OF RANKS

# Select Rank trajectories to plot
start_rank<-201
end_rank<-250

# Select the indices of the rows
rank_indices <- order(-caps[, start])[start_rank:end_rank]

#Default Times
d_times<-rep(NA,end_rank-start_rank+1)
for(j in seq(1,end_rank-start_rank+1,1)){
  test<-sum(is.na(caps[rank_indices[j],start:end]))
  if(test>0){
    d_times[j]<-min(which(is.na(caps[rank_indices[j],start:end])))
  }
}

# Rank the next columns and track the rank of the indices
results <- matrix(0, nrow = end_rank-start_rank+1, ncol = end-start+1)
for (j in start:end) {
  if (j <= ncol(caps)) {
    column_ranks <- rank(-caps[, j], ties.method = "min")
    results[, j-start+1] <- column_ranks[rank_indices]
  }
}

# Color gradient
red_gradient <- colorRampPalette(c("darkred", "pink"))(50)
red_gradient <- adjustcolor(red_gradient, alpha.f = 0.35)

for(j in seq(1,end_rank-start_rank+1,1)){
  
  indices <- which(results[j,] <= y_max)
  
  # Plot X where a stock delists and normal lines otherwise
  if(!is.na(d_times[j])){
    
    indices <- indices[indices<d_times[j]]
    
    lines(dates[date_id[indices]],log10(results[j,indices]), col=red_gradient[j],cex=0.5)
    points(dates[date_id[d_times[j]-1]], log10(results[j,d_times[j]-1]), 
           pch=4, lwd=2, col="black", cex=0.5)
    
  }else{
    
    lines(dates[date_id[indices]],log10(results[j,indices]), col=red_gradient[j],cex=0.5)
    
  }
}

### PLOT THIRD SET OF RANKS

# Select Rank trajectories to plot
start_rank<-1001
end_rank<-1050

# Select the indices of the rows
rank_indices <- order(-caps[, start])[start_rank:end_rank]

# Default Times
d_times<-rep(NA,end_rank-start_rank+1)
for(j in seq(1,end_rank-start_rank+1,1)){
  test<-sum(is.na(caps[rank_indices[j],start:end]))
  if(test>0){
    d_times[j]<-min(which(is.na(caps[rank_indices[j],start:end])))
  }
}

# Rank the next columns and track the rank of the indices
results <- matrix(0, nrow = end_rank-start_rank+1, ncol = end-start+1)
for (j in start:end) {
  if (j <= ncol(caps)) {
    column_ranks <- rank(-caps[, j], ties.method = "min")
    results[, j-start+1] <- column_ranks[rank_indices]
  }
}

# Color gradient
green_gradient <- colorRampPalette(c("darkgreen", "lightgreen"))(50)
green_gradient <- adjustcolor(green_gradient, alpha.f = 0.35)

for(j in seq(1,end_rank-start_rank+1,1)){
  
  indices <- which(results[j,] <= y_max)
  
  # Plot X where a stock delists and normal lines otherwise
  if(!is.na(d_times[j])){
    
    indices <- indices[indices<d_times[j]]
    
    lines(dates[date_id[indices]],log10(results[j,indices]), col=green_gradient[j],cex=0.5)
    points(dates[date_id[d_times[j]-1]], log10(results[j,d_times[j]-1]), 
           pch=4, lwd=2, col="black", cex=0.5)
    
  }else{
    
    lines(dates[date_id[indices]],log10(results[j,indices]), col=green_gradient[j],cex=0.5)
    
  }
}
dev.off()
