### R Code file to estimate transition probabilities across ranks
### NOTE: Current specifications take a long time to run

# Load Libraries

library(readr)
library(tidyr)
library(ggplot2)
library(reshape2)

#Reset Environment

rm(list=ls())

# Import caps

caps <- read_csv("caps_common_1962_2023_ffill.csv",
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import dates

dates <-  read_csv("dates_common_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]


topNCaps <- function(mat, j, N) {
  # Get top N caps and their indices
  
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

get_ranks <- function(x) {
  # Obtain ranks
  ranks <- rank(-x, ties.method = "first")
  
  return(ranks)
}

#x <- runif(10)
#get_ranks(x)

get_rank_index <- function(ranks, k){
  # Obtain index of ranks (break ties via min)
  index <- min(which(ranks == k))
  
  return(index)
}

est_trans_prob <- function(mat,K,M){
  # General transition probability estimation that outputs 
  # a transition probability matrix whose rows are the starting
  # rank and columns index the change in rank.
  # Inputs:
  # M is max rank switch
  # K is max baseline rank to consider
  dim<-ncol(mat)
  trans_prob<-matrix(0,nrow=K,ncol=2*M+1)
  for(i in seq(1,dim-1,1)){
    if(i %% 100==1){
      print(i) # Keep track of progress
    }
    init_vals<-mat[,i]
    init_ranks<-get_ranks(init_vals)
    new_vals<-mat[,i+1]
    new_ranks<-get_ranks(new_vals)
    for(k in seq(1,K,1)){
      rank_id<-get_rank_index(init_ranks,k)
      old_rank<-k
      new_rank<-new_ranks[rank_id]  
      movement <- min(max(new_rank-old_rank,-M),M)
      col_id<-movement+M+1
      trans_prob[k,col_id]<-trans_prob[k,col_id]+1
    }
  }
  trans_prob<-trans_prob/(dim-1)
  return(trans_prob)
}

### Estimate transition probabilities

# Number of initial ranks to consider for estimation of transition prob.
N<-1000

# Rank changes to estimate (i.e. up to +/- M)
M<-50 

trans_prob<-est_trans_prob(caps,N,M)
trans_prob

rank_change <- c(0,cumsum(rep(1,2*M)))-M
colnames(trans_prob) <- rank_change
rownames(trans_prob) <- 1:N

heatmap(trans_prob, Rowv = NA, Colv = NA)
help(heatmap)



# Plot density
color_palette <- colorRampPalette(c("blue", "red"),alpha=0.05)(nrow(trans_prob))
plot(rank_change,trans_prob[1,], type="l",ylim=c(0,max(trans_prob)), xlim=c(-20,20),
     col = color_palette[1],xlab="Rank Change",ylab="Probability")
for(i in seq(10,N,10)){
  points(rank_change,trans_prob[i,], col = color_palette[i],type="l")
}

# Plot CDFs
plot(rank_change,cumsum(trans_prob[1,]), type="l",ylim=c(0,1), xlim=c(-50,50),
     col = color_palette[1],xlab="Rank Change",ylab="Cumulative Probability")
for(i in seq(10,N,10)){
  points(rank_change,cumsum(trans_prob[i,]), col = color_palette[i],type="l")
}

### Plot mean change in rank
m<-rep(0,nrow(trans_prob))
for(i in seq(1,N,1)){
  m[i]<-sum(rank_change*trans_prob[i,])
}
plot(m,type="l",col="black",lwd=2,xlab="Rank",ylab="Mean Rank Change")



#pdf(file = "transitions.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
### Plot quantiles
c_tp<-t(apply(trans_prob,1,cumsum))
# Obtain quantiles of transition distributions
q<-seq(0.01,0.99,0.01) #quantiles
quantiles<-matrix(0,nrow=nrow(trans_prob),ncol=length(q))
for(i in seq(1,nrow(trans_prob),1)){
  for(j in seq(1,length(q),1)){
    quantiles[i,j]<-min(which(c_tp[i,]>=q[j]))-(M+1)
  }
}
color_palette <- colorRampPalette(c("red", "blue"),alpha=0.05)(length(q))
ranks<-seq(1,N,1)
plot(quantiles[,1],ranks,
     #xlim = c(-20, 20),
     xlim=c(min(quantiles),max(quantiles)),
     ylim=c(1,N),type="l",
     col=color_palette[1],xlab="Rank change", ylab="Rank",
     main = "Quantiles (1% - 99%)")
for(i in seq(1,length(q),1)){
  lines(quantiles[,i],ranks,col=color_palette[i])
}
lines(m,ranks,col="black",lwd=2) # include mean

### Illustrate simple probabilities of moving up/down in rank or not moving.
down_prob<-rep(0,nrow(trans_prob))
for(i in seq(1,N,1)){
  down_prob[i]<-sum((rank_change<0)*trans_prob[i,])
}
stay_prob<-rep(0,nrow(trans_prob))
for(i in seq(1,N,1)){
  stay_prob[i]<-sum((rank_change==0)*trans_prob[i,])
}
up_prob<-rep(0,nrow(trans_prob))
for(i in seq(1,N,1)){
  up_prob[i]<-sum((rank_change>0)*trans_prob[i,])
}

plot(stay_prob,type="l",ylim=c(0,1),ylab="",
     xlab="Rank", main = "Transition probabilities", lwd=1)
lines(up_prob,type="l",col="blue", lwd=2)
lines(down_prob,type="l",col="red", lwd=2)
legend("topright", legend = c("Increase (smaller)","Same","Decrease (bigger)"), 
       col = c("blue","black","red"),
       lty = c(1,1,1), lwd=c(2,1,2))
#dev.off()

# mean
par(mfrow = c(1, 1))
plot(1:N, m)
points(lowess(1:N, m, 1/3), type = "l")

# up - down
plot(1:N, up_prob - down_prob)
points(lowess(1:N, up_prob - down_prob), type = "l")
