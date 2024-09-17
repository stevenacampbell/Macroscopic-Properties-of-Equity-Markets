### Data Processing File #2

## R Code file to eliminate dates where no security is traded and
## create a file where capitalization data is forward filled 
## after a security is first listed and up to its delisting.

# Load Libraries

library(readr)
library(tidyr)

# Reset Environment

rm(list=ls())

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

## Remove dates with no data

# Identify columns with only NA entries
na_columns <- apply(caps, 2, function(x) all(is.na(x)))

# Check how many cases of only NA entries
sum(na_columns)

# Get the indices of columns with only NAs
na_col_indices <- which(na_columns)

# Remove columns from data based on identified indices
caps <- caps[, -na_col_indices, drop = FALSE]
rets <- rets[, -na_col_indices, drop = FALSE]
dlrets <- dlrets[, -na_col_indices, drop = FALSE]
dates<-dates[-na_col_indices]

# Save edited data

write_csv(caps, 
          file = "Data/caps_common_1962_2023.csv",
          col_names=TRUE) # Write updated caps to csv

write_csv(rets, 
          file = "Data/rets_common_1962_2023.csv",
          col_names=TRUE) # Write updated returns to csv

write_csv(dlrets, 
          file = "Data/dlrets_common_1962_2023.csv",
          col_names=TRUE) # Write updated delisting returns to csv

write_csv(as.data.frame(dates), 
          file = "Data/dates_common_1962_2023.csv",
          col_names=TRUE) # Write updated dates to csv


## Forward fill capitalization data between listing/delisting events

# Function to forward fill interior NAs in a vector
forward_fill_interior <- function(x) {
  first_non_na <- which(!is.na(x))[1] # Find the first non-NA value
  last_non_na <- tail(which(!is.na(x)), n=1) # Find the last non-NA value
  
  # Check if there is at least one non-NA value and that the first 
  # and last non-NA values are not the same. Then, forward fill in between. 
  if (!is.na(first_non_na) && first_non_na != last_non_na) {
    for (i in (first_non_na+1):last_non_na) {
      if (is.na(x[i])) {
        x[i] <- x[i-1]
      }
    }
  }
  
  return(x)
}

# Apply the forward fill operation to each row
caps_ffill <- t(apply(caps, 1, forward_fill_interior))

# Convert back to a data frame
caps_ffill <- as.data.frame(caps_ffill)

write_csv(caps_ffill, 
          file = "Data/caps_common_1962_2023_ffill.csv",
          col_names=TRUE) # Write forward filled caps to csv
