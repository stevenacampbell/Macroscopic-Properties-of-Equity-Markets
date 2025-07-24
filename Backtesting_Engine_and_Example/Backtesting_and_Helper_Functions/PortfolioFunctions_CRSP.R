#############################################################
########     Portfolio Functions for Backtesting     ########
#############################################################

# A NOTE ON CONVENTIONS:
# Securities are indexed by the columns and days/time by the rows
# We use an End of Day (EoD) convention for data:
# - Returns are EoD returns
# - Caps/Weights are EoD values
# Portfolios are updated at EoD:
# - Portfolio initialized at EoD values at time t
# - First relevant return is at time t+1
# - New trade time is s>=t+1
# - Last relevant return is at time s
# - New portfolio weights determined by observables at time s

#Function Definitions

caps_to_weights<-function(caps){
  # Calculate mu matrix (market weights)
  # input caps
  mu<-caps/(rowSums(caps))
  return(mu)
}

caps_and_rets_to_rr_rd<-function(caps,rets){
  # Computes real and dividend returns from CRSP caps and rets
  # input caps
  # input rets
  cap_ratio<-caps
  cap_ratio[1,]<-rep(1,ncol(caps))
  cap_ratio[2:length(caps[,1]),]<-caps[2:length(caps[,1]),]/caps[1:(length(caps[,1])-1),]
  div_returns<-1+rets-cap_ratio # -Inf when stock first lists, NaN in delisted period
  div_returns[div_returns<0]<-0
  div_returns[is.na(div_returns)]<-0
  real_ret<-rets-div_returns # 0 when stock first lists
  
  # Correct for when both caps and rets are 0 (when a stock is not listed)
  div_returns[caps==0 & rets==0]<-0
  real_ret[caps==0 & rets==0]<-0
  return(list(real_ret,div_returns))
}

trade_decision<-function(days_since_trade,freq){
  # Trade decision function
  # input days since last trade
  # input target trading frequency
  
  return((days_since_trade==freq))
}

calc_psi_minus_and_D_minus<-function(rr,rd,psi){
  #Calculate Psi_ and D_
  #input psi row vector
  #input period returns
  rrp1<-1+rr
  psi_<-rrp1*psi
  D_<-sum(psi*rd)
  return(list(psi_,D_))
}

calc_D_hat<-function(D_,V_,pi_target,psi_,tcs){
  #Calculate D_hat
  psi_sold<-matrix(0,ncol=length(psi_),nrow=1)
  psi_sold[pi_target==0]<-1
  D_hat<-(D_+(1-tcs)*sum(psi_*psi_sold))/V_
  return(D_hat)
}

get_c_vector<-function(pi,pi_target){
  #Get the c vector values
  c<-pi/pi_target
  c[pi_target==0]<-0
  return(c)
}

c_function<-function(c,c_vec,D_hat,pi_target,tcb,tcs){
  #function to solve for c
  c_plus_1<-c-c_vec
  c_plus_1[c_plus_1<=0]<-0
  c_plus_2<-c_vec-c
  c_plus_2[c_plus_2<=0]<-0
  c_out<-(1+tcb)*sum(c_plus_1*pi_target)-(1-tcs)*sum(c_plus_2*pi_target)-D_hat
  return(abs(c_out))
}

run_portfolio<-function(V0,pi0,tcb,tcs,freq,mu,div_returns,real_ret,dl_flag,get_pi){
  #Run portfolio on data
  NS<-ncol(mu) #Number of stocks
  Nper<-nrow(mu) #Number of periods
  
  #Set up Values matrix to record data
  col_names_values<-c(rep("pi_",NS),rep("psi_",NS),"D_", rep("pi_target",NS),"V_","D_hat",rep("c_vec",NS),"c","V",rep("psi",NS),rep("pi",NS))
  size_out<-length(col_names_values)
  Values<-matrix(0, nrow=Nper, ncol=size_out)
  colnames(Values)<-col_names_values
  
  #Initalize Variables
  V<-V0
  V_<-V0
  pi<-pi0
  psi<-pi0*V0
  psi_<-pi0*V0
  D_<-0 # Initial uninvested dividends
  idx<-1
  days_since_trade<-1
  Values[1,]<-c(pi,psi_,D_,pi,V_,NA,rep(NA,NS),NA,V,psi,pi)
  
  print("Backtest Running...")
  
  #Perform the trading until the end of the data set
  while(idx<=(Nper-1)){
    idx<-idx+1
    
    if(idx %% ceiling(Nper/100) == 1){
      print(paste0( round(100 * idx/Nper,0), "% Complete.")) # Track progress
    }
    
    #Get returns
    rr<-real_ret[idx,] 
    rd<-div_returns[idx,]
    
    #Update psi_ and D_
    update_returns<-calc_psi_minus_and_D_minus(rr,rd,psi)
    psi_<-update_returns[[1]]
    D_<-D_+update_returns[[2]]
    
    #Update V_
    V_<-sum(psi_)
    
    #Get current pi
    pi<-psi_/V_
    
    trade<-trade_decision(days_since_trade,freq)
    
    if(trade){
      
      #Get target portfolio
      pi_target<-get_pi(mu[idx,],dl_flag[idx,])
      
      #Get Dhat
      D_hat<-calc_D_hat(D_,V_,pi_target,psi_,tcs)
      
      #Get vector of c_i
      c_vec<-get_c_vector(pi,pi_target)
      
      #Solve for value of C
      res<-optimize(c_function,c_vec=c_vec,D_hat=D_hat,pi_target=pi_target,tcb=tcb,tcs=tcs,interval=c(0,2))
      c<-res$minimum
      
      #Update V, psi and pi
      V<-c*V_
      psi<-pi_target*V
      Values[idx,]<-c(pi,psi_,D_,pi_target,V_,D_hat,c_vec,c,V,psi,pi_target)
      
      # Reset uninvested dividends
      D_<-0
      
      days_since_trade<-1
    }else{
      #Update values
      V<-V_+D_
      psi<-psi_
      pi_target<-rep(NA,NS)
      D_hat<-NA
      c<-NA
      c_vec<-rep(NA,NS)
      Values[idx,]<-c(pi,psi_,D_,pi_target,V_,D_hat,c_vec,c,V,psi,pi)
      days_since_trade<-days_since_trade+1
    }
  }
  
  print("Backtest Complete.")
  return(list(V,Values))
}
