## Load Libraries

library(readr)
library(tidyr)

## Reset Environment

rm(list=ls())

## Import Raw CRSP data from WRDS (see README document)

data <- read_csv("Data/CRSP_common_1962_2023.csv", 
                 col_types = cols(PERMNO = col_double(), 
                            date = col_date(format = "%d/%m/%Y"), 
                            DLRET = col_double(), PRC = col_double(), 
                            RET = col_double(), SHROUT = col_double()))

data <- as.data.frame(data) # Convert to data frame

head(data) # Preview

## Prices

prices <- pivot_wider(data, id_cols = PERMNO, 
                      names_from = date, 
                      values_from = PRC) # Convert to wide format for prices

prices <-  as.data.frame(prices) # Convert to data frame

permnos<-prices[,1] # Extract permnos

rownames(prices)<-permnos # Set permnos as rownames

prices<-prices[,!(colnames(prices)=="PERMNO")] # Remove permno column

dates<-colnames(prices) # Extract dates

prices<-prices[,order(dates)] # Order prices by date

prices<-abs(prices) # Remove the negative sign from Bid-Ask average prices

prices[1:4,1:4] # Preview

## Shares

shares <- pivot_wider(data, id_cols = PERMNO, 
                      names_from = date, 
                      values_from = SHROUT) # Convert to wide format for shares

shares <-  as.data.frame(shares) # Convert to data frame

rownames(shares)<-permnos # Set permnos as rownames

shares<-shares[,!(colnames(shares)=="PERMNO")] # Remove permno column

shares<-shares[,order(dates)] # Order shares by date

shares[1:4,1:4] # Preview

## Market Caps

caps <- prices * shares # Compute market capitalizations

caps[1:4,1:4] # Preview

write_csv(caps, 
          file = "Data/caps_common_1962_2023.csv",
          col_names=TRUE) # Write caps to csv

## Returns 

returns <- pivot_wider(data, id_cols = PERMNO, 
                      names_from = date, 
                      values_from = RET) # Convert to wide format for returns

returns <-  as.data.frame(returns) # Convert to data frame

rownames(returns)<-permnos # Set permnos as rownames

returns<-returns[,!(colnames(returns)=="PERMNO")] # Remove permno column

returns<-returns[,order(dates)] # Order returns by date

returns[1:4,1:4] # Preview

write_csv(returns, 
          file = "Data/rets_common_1962_2023.csv",
          col_names=TRUE) # Write returns to csv

## Delisting Returns 

dlreturns <- pivot_wider(data, id_cols = PERMNO, 
              names_from = date, 
              values_from = DLRET) # Convert to wide format for delisting returns

dlreturns <-  as.data.frame(dlreturns) # Convert to data frame

rownames(dlreturns)<-permnos # Set permnos as rownames

dlreturns<-dlreturns[,!(colnames(dlreturns)=="PERMNO")] # Remove permno column

dlreturns<-dlreturns[,order(dates)] # Order delisting returns by date

dlreturns[1:4,1:4] # Preview

write_csv(dlreturns, 
          file = "Data/dlrets_common_1962_2023.csv",
          col_names=TRUE) # write delisting returns to csv

## Dates

dates<-dates[order(dates)] # Order the dates chronologically (1962 to 2022)

write_csv(as.data.frame(dates), 
          file = "Data/dates_common_1962_2023.csv",
          col_names=TRUE) # write dates to csv

## Permnos

write_csv(as.data.frame(permnos), file = "Data/permnos_common_1962_2023.csv",
          col_names=TRUE) # write permnos to csv

