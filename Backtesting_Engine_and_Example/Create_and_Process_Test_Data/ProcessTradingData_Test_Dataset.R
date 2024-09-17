library(readr)

# Reset Environment
rm(list=ls())

# Load Libraries
source("Backtesting_and_Helper_Functions/PortfolioFunctions.R")

# Import saved CRSP market capitalizations

caps <- read_csv("Test_Data/test_caps.csv", 
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import saved CRSP security returns

rets <- read_csv("Test_Data/test_rets.csv", 
                 col_types =  cols(.default = "d"))
rets <- as.data.frame(rets)

# Import saved CRSP delisting returns

dlrets <- read_csv("Test_Data/test_dlrets.csv", 
                   col_types =  cols(.default = "d"))
dlrets <- as.data.frame(dlrets)

# Import dates

dates <-  read_csv("Test_Data/test_dates.csv", 
                   col_types = cols(dates_test = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

# Import permnos

permnos <-  read_csv("Test_Data/test_permnos.csv",
                     col_types = cols(permnos_test = col_double()))
permnos <- as.data.frame(permnos)[[1]]


# Helper functions
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

# Create flag for the delisting period and add default delisting return
default_dlret<-0 # Default delisting return
delist_flag<- !(is.na(dlrets)) # Initialize delisting flag at non-zero delisting returns
dl_id<-rep(0,nrow(delist_flag)) #delist return id
lo_id<-rep(0,nrow(delist_flag)) #last observation id
max_id<-ncol(delist_flag)
for(i in seq(1,nrow(delist_flag),1)){
  
  id<-min(which(delist_flag[i,]))
  idl<-abs(max(which(caps[i,]>=0))) # if stock is never active = Inf
  
  # If last stock observation is the last period and no delisting return
  # assume that the stock survives
  if(idl==max_id && id==Inf){
    idl=Inf
  }
  dl_id[i]<-id
  lo_id[i]<-idl
  
  # Report progress
  print(paste0(i,": ", id==idl, " DL id: ", id, " LO id: ", idl))
  
  # If no delisting return but stock drops out, flag delisting
  # and add default delisting return.
  if(id==Inf && idl!=Inf){
    delist_flag[i,idl]<-TRUE
    dlrets[i,idl]<-default_dlret
  }
  
}

# Update caps and returns for the period immediately following the delisting
# for use in trading
for(i in seq(1,nrow(delist_flag),1)){
  if(i %% 100 ==1){
    print(i)
  }
  id<-min(which(delist_flag[i,]))
  if(id<ncol(delist_flag)){
    if(is.na(caps[i,id+1])){
      caps[i,id+1]<-caps[i,id]*(1+dlrets[i,id])
      rets[i,id+1]<-dlrets[i,id]
    }
  }
}


## Data reduction
# For testing purposes we restrict the data to those stocks
# that have been in the top 3 at one point in history.
# NOTE: All portfolio processing is done after first sorting for the top N 
# stocks at the trade time. So, this reduction does not introduce any look-ahead
# bias to the current setup.

# Find indices corresponding to the reduced universe
N<-3
unique_indices<-c()
for(i in seq(1,ncol(caps),1)){
  if(i %% 100 == 1){
    print(i)
  }
  res<-topNCaps(caps,i,N)
  indices<-res[[1]]
  unique_indices <- unique(c(unique_indices, indices))
}
unique_indices<-sort(unique_indices) # save (sorted) indices
length(unique_indices) # Number of stocks in reduced universe

# Subset Data
caps_trading<-caps[unique_indices,]
caps_trading[is.na(caps_trading)]<-0
rets_trading<-rets[unique_indices,]
rets_trading[is.na(rets_trading)]<-0
permnos_trading<-permnos[unique_indices]

# Modify delisting flag format to be TRUE after stock delists
delist_flag_trading<-delist_flag[unique_indices,]
for(i in seq(1,nrow(delist_flag_trading),1)){
  if(i %% 100 == 1){
    print(i)
  }
  id<-min(which(delist_flag_trading[i,]))
  if(id<ncol(delist_flag_trading)){
    delist_flag_trading[i,id:ncol(delist_flag_trading)]<-TRUE
  }
}

# Write backtesting data to csv
write_csv(as.data.frame(t(caps_trading)), 
          file = "Test_Data/BACKTESTING_test_caps.csv",
          col_names=TRUE)

write_csv(as.data.frame(t(rets_trading)), 
          file = "Test_Data/BACKTESTING_test_rets.csv",
          col_names=TRUE)

write_csv(as.data.frame(t(delist_flag_trading)), 
          file = "Test_Data/BACKTESTING_test_dlflg.csv",
          col_names=TRUE) 

write_csv(as.data.frame(permnos_trading), 
          file = "Test_Data/BACKTESTING_test_permnos.csv",
          col_names=TRUE) 

write_csv(as.data.frame(dates), 
          file = "Test_Data/BACKTESTING_test_dates.csv",
          col_names=TRUE) 


