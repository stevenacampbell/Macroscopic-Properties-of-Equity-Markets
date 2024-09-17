library(readr)

# Reset Environment
rm(list=ls())

# Load Libraries
source("PortfolioFunctions_CRSP.R")

### IMPORT DATA FOR BACKTESTING

# Import saved CRSP market capitalizations

caps <- read_csv("Data/BACKTESTING_test_caps.csv", 
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import saved CRSP security returns

rets <- read_csv("Data/BACKTESTING_test_rets.csv", 
                 col_types =  cols(.default = "d"))
rets <- as.data.frame(rets)

# Import saved CRSP delisting flag

dlflg <- read_csv("Data/BACKTESTING_test_dlflg.csv", 
                  col_types =  cols(.default = col_logical()))
dlflg <- as.data.frame(dlflg)

# Import dates

dates <-  read_csv("Data/BACKTESTING_test_dates.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

# Import permnos

permnos <-  read_csv("Data/BACKTESTING_test_permnos.csv",
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

### PORTFOLIO FUNCTIONS
### MODIFY THESE FUNCTIONS TO CHANGE INVESTMENT/BENCHMARK STRATEGIES

# Define portfolio map
get_pi<-function(mu,dl_flag){
  
  # Top N stocks to trade
  N<-3
  
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
  port<-rep(1/N,N) # Equal weight #muN^(1/2)/sum(muN^(1/2)) # Diversity portfolio
  
  # Assign the target weights to the stocks
  pi[final_ids]<-port
  
  return(as.vector(pi))
}

# Define benchmark portfolio (Current: Index Tracking top N stocks)
get_pi_benchmark<-function(mu,dl_flag){
  
  # Top N stocks to trade
  N<-3
  
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
  port<-muN # Index tracking top N stocks
  
  # Assign the target weights to the stocks
  pi[final_ids]<-port
  
  return(as.vector(pi))
}

### PREPROCESSING
# Convert caps and returns to backtesting inputs
mu<-as.matrix(caps_to_weights(caps)) # market weights 
returns<-caps_and_rets_to_rr_rd(caps,rets) # returns (real & dividend)
real_ret<-as.matrix(returns[[1]])
div_returns<-as.matrix(returns[[2]])

### RUN BACKTEST
# Initial Backtest Parameters
V0<-1000 #Initial Wealth
pi0<-get_pi(mu[1,],dlflg[1,]) #Initial Portfolio
pi0_bm<-get_pi_benchmark(mu[1,],dlflg[1,])
tcb<-0.01 #Buying transaction cost
tcs<-0.01 #Selling transaction cost
freq<-3 #Trading frequency
freq_bm<-21

write_csv(as.data.frame(real_ret), 
          file = "Data/test_rr.csv",
          col_names=TRUE) 
write_csv(as.data.frame(div_returns), 
          file = "Data/test_dr.csv",
          col_names=TRUE)
write_csv(as.data.frame(mu), 
          file = "Data/test_mu.csv",
          col_names=TRUE)



# Get outputs for portfolio
output<-run_portfolio(V0,pi0,tcb,tcs,freq,mu,div_returns,real_ret,dlflg,get_pi)
Term_value<-output[[1]]
Observables<-output[[2]]

write_csv(as.data.frame(Observables), 
          file = "Data/test_obs.csv",
          col_names=TRUE)

# Get outputs for benchmark portfolio
output_bm<-run_portfolio(V0,pi0_bm,tcb,tcs,freq_bm,mu,div_returns,real_ret,dlflg,get_pi_benchmark)
Term_value_bm<-output_bm[[1]]
Observables_bm<-output_bm[[2]]

# Log Values
lV<-log10(Observables[,"V"])
lV_bm<-log10(Observables_bm[,"V"])

# Plot Log Portfolio Values
plot(dates, lV, type="l", col="blue",lwd=2,ylab="Log Value",xlab="Dates",ylim=c(min(lV,lV_bm),max(lV,lV_bm)))
lines(dates, lV_bm, type="l", col="red",lwd=2,ylab="Log Value",xlab="Dates")
legend("bottomright",c("Portfolio","Benchmark"),
       col=c("blue","red"),lwd=c(2,2))

# Plot Log Relative Value over time
plot(dates,lV-lV_bm,type="l",lwd=2,ylab="Log Relative Value",xlab="Dates")


View(Observables)

