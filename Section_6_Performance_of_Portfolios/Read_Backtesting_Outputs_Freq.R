# Required Libraries
library(readr)
library(ggplot2)
library(reshape2)

# Reset Environment
rm(list=ls())

## Code to plot performance differences based on changes in rebalancing frequency.

# Define the sequence of years and frequencies
years <- seq(1962, 2023, 1)
freqs <- c(1,2,5,10,25,50,125,500)

# Define transaction costs and p value
tcb <- 0.0025
p<-0

# Create an empty list to store data
data_list <- list()

# Loop through each year and frequences to read the data
for (i in seq(1,length(years),1)) {
  for (j in seq_along(freqs)) {
    year <- years[i]
    f <- freqs[j]
    
    # Print the current year and p-value
    print(paste0("YEAR: ", year, ", FREQ=", f))
    
    # Construct the file path
    file_path <- paste0("Backtest_Outputs/lV_tc_", tcb * 10000, "bps", "_freq_",
                        f, "_p_", p, "_", year, "_", year, ".csv")
    
    # Read the data from the CSV file
    data <- read_csv(file_path, col_types = cols(.default = "d"))
    data <- as.data.frame(data)[[1]]
    
    # Store the data in the list with a descriptive name
    data_list[[paste0("year_", year, "_freq_", f)]] <- data
  }
}

# Function to query the data by year and p-value
get_data <- function(year, f) {
  data_key <- paste0("year_", year, "_freq_", f)
  if (data_key %in% names(data_list)) {
    return(data_list[[data_key]])
  } else {
    return(NULL)
  }
}

# Collect terminal performance from data
term_perf<-matrix(0,ncol=length(freqs),nrow=length(seq(1,length(years),1)))
for (i in seq(1,length(years),1)) {
  for (j in seq_along(freqs)) {
    lV<-get_data(years[i], freqs[j])
    term_perf[i,j]<-lV[length(lV)]-lV[1]
  }
}
rownames(term_perf)<-years
colnames(term_perf)<-freqs

### Bar Plot for Frequencies (Divisors of 250)

par(mar = c(5, 3, 4, 1))  # Decrease the right and left margins

# Create the color palette
color_palette <- colorRampPalette(c(rgb(0.0,0.0,1,0.8), rgb(1,0.0,0.0,0.8)), alpha = TRUE)(length(freqs))

# Calculate the differences
differences <- sweep(term_perf, 1, term_perf[,ncol(term_perf)], FUN = "-")

# Create a matrix to store bar heights for each year
bar_heights <- t(differences)

# Define the layout
layout(matrix(1:2, ncol = 1))

# Create an empty plot
bp <- barplot(
  rep(0, length(years)),
  names.arg = years,
  ylim = c(-0.025, 0.015),
  main = "Performance differences",
  xlab = "Time",
  ylab = "",
  col = "white",
  border=NA
)
# Plot each bar individually for range of performance
for (j in 1:ncol(bar_heights)) {
  rect(
    xleft = bp[j] - 0.4, 
    ybottom = min(bar_heights[, j]),
    xright = bp[j] + 0.4, 
    ytop = max(bar_heights[, j]),
    col = rgb(0.5,0.5,0.5,0.2), 
    border = NA
  )
}
# Plot each bar individually with gradient colors based on frequency
for (i in 1:length(freqs)) {
  for (j in 1:ncol(bar_heights)) {
    rect(
      xleft = bp[j] - 0.4, 
      ybottom = bar_heights[i, j]-0.0003,
      xright = bp[j] + 0.4, 
      ytop = bar_heights[i, j]+0.0003,
      col = color_palette[i], 
      border = NA
    )
  }
}

# Create an empty plot
bp <- barplot(
  rep(0, length(years)),
  names.arg = years,
  ylim = c(-0.035, 0.035),
  main = "Difference in excess growth rate",
  xlab = "Time",
  ylab = "",
  col = "white",
  border=NA
)

# Read and plot the EGR dataset
ann_egr_d <- read_csv("Annual_EGR.csv", col_types = cols(.default = "d"))
ann_egr_y <- read_csv("Annual_EGR_Yearly.csv", col_types = cols(.default = "d"))
ann_egr_d<-as.data.frame(ann_egr_d)[[1]]
ann_egr_y<-as.data.frame(ann_egr_y)[[1]]

# Plot each bar individually
for (j in 1:length(years)) {
  rect(
    xleft = bp[j] - 0.4, 
    ybottom = min(0,ann_egr_d[j]-ann_egr_y[j]),
    xright = bp[j] + 0.4, 
    ytop = max(0,ann_egr_d[j]-ann_egr_y[j]),
    col = rgb(0.5,0.5,0.5,0.4), 
    border = NA
  )
}

### Bar Plot for Frequencies (All Frequencies)
# Create the color palette
f_v<-freqs
color_palette <- colorRampPalette(c(rgb(0.0,0.0,1,0.2),rgb(1,0.0,0.0,0.2)), alpha = TRUE)(length(f_v))

# Calculate the differences
differences <- sweep(term_perf, 1, term_perf[,ncol(term_perf)], FUN = "-")

# Create a matrix to store bar heights for each year
bar_heights <- t(differences)

# Create an empty plot
bp <- barplot(
  rep(0, length(years)),
  names.arg = years,
  ylim = c(-0.03, 0.07),
  main = "Performance differences",
  xlab = "Time",
  ylab = "Difference",
  col = "white",
  border=NA
)
# Plot each bar individually with gradient colors based on row index
for (j in 1:ncol(bar_heights)) {
  rect(
    xleft = bp[j] - 0.4, 
    ybottom = min(bar_heights[f_v, j]),  
    xright = bp[j] + 0.4, 
    ytop = max(bar_heights[f_v, j]),
    col = rgb(0.5,0.5,0.5,0.2), 
    border = NA
  )
}
# Plot each bar individually with gradient colors based on row index
for (i in 1:length(f_v)) {
  for (j in 1:ncol(bar_heights)) {
    rect(
      xleft = bp[j] - 0.4, 
      ybottom = bar_heights[f_v[i], j]-0.0005, 
      xright = bp[j] + 0.4, 
      ytop = bar_heights[f_v[i], j]+0.0005,
      col = color_palette[i], 
      border = NA
    )
  }
}

# Create an empty plot
bp <- barplot(
  rep(0, length(years)),
  names.arg = years,
  ylim = c(0.0, 0.17),
  main = "Annual excess growth rate",
  xlab = "Years",
  ylab = "Value",
  col = "white",
  border=NA
)

# Read and plot the EGR dataset
ann_egr <- read_csv("Annual_EGR.csv", col_types = cols(.default = "d"))
ann_egr<-as.data.frame(ann_egr)[[1]]

# Plot each bar individually
for (j in 1:ncol(bar_heights)) {
  rect(
    xleft = bp[j] - 0.4, 
    ybottom = 0,
    xright = bp[j] + 0.4, 
    ytop = ann_egr[j],
    col = rgb(0.5,0.5,0.5,0.4), 
    border = NA
  )
}
