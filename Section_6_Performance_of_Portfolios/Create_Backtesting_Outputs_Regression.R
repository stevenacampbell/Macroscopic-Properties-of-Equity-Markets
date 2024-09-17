library(readr)
library(foreach)
library(doParallel)

# Reset Environment
rm(list=ls())

# Load Libraries
source("PortfolioFunctions_CRSP.R")

### IMPORT DATA FOR BACKTESTING

# Import saved CRSP market capitalizations

caps <- read_csv("Data/BACKTESTING_caps_1962_2023.csv", 
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import saved CRSP security returns

rets <- read_csv("Data/BACKTESTING_rets_1962_2023.csv", 
                 col_types =  cols(.default = "d"))
rets <- as.data.frame(rets)

# Import saved CRSP delisting flag

dlflg <- read_csv("Data/BACKTESTING_dlflg_1962_2023.csv", 
                  col_types =  cols(.default = col_logical()))
dlflg <- as.data.frame(dlflg)

# Import dates

dates <-  read_csv("Data/BACKTESTING_dates_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

# Import permnos

permnos <-  read_csv("Data/BACKTESTING_permnos_1962_2023.csv",
                     col_types = cols(.default = col_double()))
permnos <- as.data.frame(permnos)[[1]]

### DEFINE INPUT FUNCTIONS

# Helper Function
topN<-function(mu,N){
  
  # Get the indices of the top N securities
  indices <- order(mu, decreasing = TRUE)[1:N]
  
  # Get the weights for the top N sub-market
  mu_N <- mu[indices]/sum(mu[indices])
  
  # Create a data frame with the results
  result <- data.frame(indices = indices, mu_N = mu_N)
  
  return(result)
}

### PREPROCESSING
# Convert caps and returns to backtesting inputs
mu<-as.matrix(caps_to_weights(caps)) # market weights 
returns<-caps_and_rets_to_rr_rd(caps,rets) # returns (real & dividend)
real_ret<-as.matrix(returns[[1]])
div_returns<-as.matrix(returns[[2]])

# LOOP OVER YEARS AND PVALS
years<-seq(1962,2023,1)
pvals<-c(1,0)

# Trading frequency and transaction costs
f<-10
tc<-0.0025

# Set up parallel for loop
cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

foreach(i=seq(1,length(years),1), .packages=c("readr")) %dopar% {
  for(j in seq(1,length(pvals),1)){
    
    print(paste0("YEAR: ",years[i],", P=",pvals[j]))
    
    # Subset period
    start_id<-min(which(dates>=paste0(years[i],"-01-01")))
    end_id<-max(which(dates<=paste0(years[i],"-12-31")))
    
    mu_sub<-mu[start_id:end_id,]
    r_sub<-real_ret[start_id:end_id,]
    rd_sub<-div_returns[start_id:end_id,]
    dlflg_sub<-dlflg[start_id:end_id,]
    
    ### PORTFOLIO FUNCTIONS
    ### MODIFY THESE FUNCTIONS TO CHANGE INVESTMENT/BENCHMARK STRATEGIES
    
    p<-pvals[j] # select p value
    
    # Define portfolio map (using given p value)
    get_pi<-function(mu,dl_flag){
      
      # Top N stocks to trade
      N<-500
      
      # Initialize id list and portfolio
      ids <- seq(1,length(mu),1)
      pi<-rep(0,length(mu))
      
      # Subset market weights and ids to include stocks that have not defaulted
      mask <- !dl_flag
      admissible_mu<-mu[mask]
      admissible_ids<-ids[mask]
      
      # Get the (sub) indices of the largest N stocks and their relative weights
      res<-topN(admissible_mu,N)
      indices<-res[[1]]
      muN<-res[[2]]
      
      # Get the original indices corresponding to these stocks
      final_ids<-admissible_ids[indices]
      
      # Construct a portfolio on this sub-market
      port<-muN^(p)/sum(muN^(p)) # Diversity portfolio
      
      # Assign the target weights to the stocks
      pi[final_ids]<-port
      
      return(as.vector(pi))
    }
    
    ### RUN BACKTEST
    # Initial Backtest Parameters
    V0<-1000 #Initial Wealth
    pi0<-get_pi(mu_sub[1,],dlflg[1,]) #Initial Portfolio
    tcb<-tc #Buying transaction cost
    tcs<-tc #Selling transaction cost
    freq<-f #Trading frequency
    
    # Get outputs for portfolio
    output<-run_portfolio(V0,pi0,tcb,tcs,freq,mu_sub,rd_sub,r_sub,dlflg_sub,get_pi)
    Term_value<-output[[1]]
    Observables<-output[[2]]
    
    # Log Values
    lV<-log10(Observables[,"V"])
    
    write_csv(as.data.frame(lV), file = paste0("Backtest_Outputs/lV_tc_",tcb*10000,"bps","_freq_",
                                               freq,"_p_",p,"_",years[i],"_",
                                               years[i],".csv")) # write output to csv
  }
}

stopCluster(cl)
