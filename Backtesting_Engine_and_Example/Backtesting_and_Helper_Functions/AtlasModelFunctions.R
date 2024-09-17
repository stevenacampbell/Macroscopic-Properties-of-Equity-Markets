AtlasModel<-function(n,N,T,gamma,gs,sigmas,X0){
  # Simulate from the (generalized) Altas Model
  # n system size
  # N number of time steps
  # T terminal time
  # gamma Baseline drift
  # g Atlas drift parameter
  # sigma volatilities
  # X0 initial values
  
  drifts<-gamma+gs
  vols<-sigmas
  dt<- T/N
  X<-matrix(0, nrow=n, ncol=N+1)
  X[,1]<-X0
  
  for(i in seq(1,N,1)){
    ranks<-rank(-X[,i])
    X[,i+1]<-X[,i]+drifts[ranks]*dt+vols[ranks]*sqrt(dt)*rnorm(n)
  }
  
  return(X)
}

RankedSystem<-function(X){
  # Rank a system of caps (or log caps) where the input matrix has
  # columns indexing time and rows indexing the market securities.
  
  N<-ncol(X)
  n<-nrow(X)
  X_R<-matrix(0,nrow=n,ncol=N)
  for(i in seq(1,N,1)){
    X_R[,i]<- -1*sort(-X[,i])
  }
  return(X_R)
}

Returns<-function(X){
  # Compute the returns from the log caps
  
  R <- t(apply(X, 1, diff))
  
  return(R)
}

ReturnsbyRank<-function(X,R){
  # Assign returns by the capitalization rank
  
  n_cols <- ncol(X)
  ret_by_rank <- matrix(0, nrow=nrow(X), ncol=n_cols-1)
  for(i in seq(1,n_cols-1,1)){
    r_vals <- R[,i]
    ret_by_rank[,i] <- r_vals[order(-X[,i])]
  }
  return(ret_by_rank)
}

CapitalDistribution<-function(X){
  # Return the capital distribution from a matrix of log capitalizations
  
  # Compute capitalizations
  caps <- exp(X)
  
  # Rank the capitalizations
  caps_r <-RankedSystem(caps)
  
  # Compute the capital distribution
  col_sums <- colSums(caps_r)
  cap_dist <- sweep(caps_r,2,col_sums,FUN="/")
  
  return(cap_dist)
}