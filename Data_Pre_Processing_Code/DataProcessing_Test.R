# Load Libraries

library(readr)
library(tidyr)

# Reset Environment

rm(list=ls())

## Code to check/test pre-processing output for AAPL

# Import CRSP data for AAPL

AAPL <- read_csv("Data/AAPL_data.csv", 
                 col_types = cols(PERMNO = col_double(), 
                                  date = col_date(format = "%d/%m/%Y"), 
                                  DLRET = col_double(), PRC = col_double(), 
                                  RET = col_double(), SHROUT = col_double()))
AAPL <- as.data.frame(AAPL)

# Import saved CRSP market capitalizations

caps <- read_csv("Data/caps_common_1962_2023.csv", 
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import saved CRSP security returns

rets <- read_csv("Data/rets_common_1962_2023.csv", 
                 col_types =  cols(.default = "d"))
rets <- as.data.frame(rets)

# Import saved CRSP delisting returns

dlrets <- read_csv("Data/dlrets_common_1962_2023.csv", 
                   col_types =  cols(.default = "d"))
dlrets <- as.data.frame(dlrets)

# Import dates

dates <-  read_csv("Data/dates_common_1962_2023.csv", 
                    col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

# Import permnos

permnos <-  read_csv("Data/permnos_common_1962_2023.csv",
                     col_types = cols(permnos = col_double()))
permnos <- as.data.frame(permnos)[[1]]

# Preview

head(AAPL)
caps[1:4,1:4]
rets[1:4,1:4]
dlrets[1:4,1:4]
head(dates)
head(permnos)

# Get identifying information for AAPL in main data set

permno_AAPL<-AAPL[1,1]
col_mask<- dates %in% AAPL$date
row_id<-min(which(permnos==permno_AAPL))

# Get rets and caps directly

cap_AAPL<-abs(AAPL$PRC)*AAPL$SHROUT
ret_AAPL<-AAPL$RET
dlret_AAPL<-AAPL$DLRET

# Get rets and caps from data set

verify_cap_AAPL<-as.numeric(caps[row_id,col_mask])
verify_ret_AAPL<-as.numeric(rets[row_id,col_mask])
verify_dlret_AAPL<-as.numeric(dlrets[row_id,col_mask])

# Check if all values are equal

all.equal(verify_cap_AAPL,cap_AAPL)
all.equal(verify_ret_AAPL,ret_AAPL)
all.equal(verify_dlret_AAPL,dlret_AAPL)

# Plot caps and returns (Note: No delisting returns for AAPL)

plot(AAPL$date,cap_AAPL,type="l")
lines(AAPL$date,verify_cap_AAPL,type="l",col="red")

plot(AAPL$date,ret_AAPL,type="l")
lines(AAPL$date,verify_ret_AAPL,type="l",col="red")

