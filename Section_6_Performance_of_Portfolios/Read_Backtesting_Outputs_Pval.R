# Required Libraries
library(readr)

# Reset Environment
rm(list=ls())

## Code to plot performance differences based on changes in the portfolio p value.

# Define the sequence of years and p-values
years <- seq(1962, 2023, 1)
pvals <- seq(0.0, 1, 0.01)
freq <- 20
tcb <- 0.0025

# Create an empty list to store data
data_list <- list()

# Loop through each year and p-value to read the data
for (i in seq(1,length(years),1)) {
  for (j in seq_along(pvals)) {
    year <- years[i]
    p <- pvals[j]
    
    # Print the current year and p-value
    print(paste0("YEAR: ", year, ", P=", p))
    
    # Construct the file path
    file_path <- paste0("Backtest_Outputs/lV_tc_", tcb * 10000, "bps", "_freq_",
                        freq, "_p_", p, "_", year, "_", years[i], ".csv")
    
    # Read the data from the CSV file
    data <- read_csv(file_path, col_types = cols(.default = "d"))
    data <- as.data.frame(data)[[1]]
    
    # Store the data in the list with a descriptive name
    data_list[[paste0("year_", year, "_p_", p)]] <- data
  }
}

# Function to query the data by year and p-value
get_data <- function(year, p) {
  data_key <- paste0("year_", year, "_p_", p)
  if (data_key %in% names(data_list)) {
    return(data_list[[data_key]])
  } else {
    return(NULL)
  }
}

# Collect terminal performance
term_perf<-matrix(0,ncol=length(pvals),nrow=length(seq(1,length(years),1)))
for (i in seq(1,length(years),1)) {
  for (j in seq_along(pvals)) {
    lV<-get_data(years[i], pvals[j])
    term_perf[i,j]<-lV[length(lV)]-lV[1]
  }
}
rownames(term_perf)<-years
colnames(term_perf)<-pvals

### Bar Plot of Performance Differences (to market)

# Create the color palette
color_palette <- colorRampPalette(c(rgb(0.2, 0.2, 0.2, 0.2), rgb(0.9, 0.9, 0.9, 0.2)), alpha = TRUE)(length(pvals))

# Calculate the differences
differences <- sweep(term_perf, 1, term_perf[, ncol(term_perf)], FUN = "-")

# Create a matrix to store bar heights for each year
bar_heights <- t(differences)

# Define the layout
layout(matrix(1:2, ncol = 1))

# Plot the first set of bars
barplot(
  bar_heights[1, ],
  names.arg = years,
  col = color_palette[1],
  border = NA,
  ylim = c(-0.045, 0.045),
  main = "Performance differences",
  xlab = "Time",
  ylab = ""
)

# Overlay additional bars
for (i in 2:(ncol(term_perf) - 1)) {
  barplot(
    bar_heights[i, ],
    names.arg = years,
    col = color_palette[i],
    border = NA,
    ylim = c(-0.045, 0.045),
    add = TRUE
  )
}

# Read and prepare the entropy dataset
ann_ddiv <- read_csv("Annual_Change_Entropy.csv", col_types = cols(.default = "d"))
ann_ddiv <- as.data.frame(ann_ddiv)[[1]]

# Plot the annual change in Entropy
barplot(
  ann_ddiv,
  names.arg = years,
  col = rgb(0.2, 0.2, 0.2, 0.2),
  border = NA,
  ylim = c(min(ann_ddiv), max(ann_ddiv)),
  main = "Annual change in entropy",
  xlab = "Time",
  ylab = ""
)


### High resolution plot (explicit gradient - without overlaying bars)

# Refine pvals for color gradient
refined_pvals<-seq(0,1,0.0001)

# Create the color palette
color_palette <- colorRampPalette(c(rgb(0.2, 0.2, 0.2, 0.2), rgb(0.9, 0.9, 0.9, 0.2)), alpha = TRUE)(length(refined_pvals))

# Calculate the differences
differences <- sweep(term_perf, 1, term_perf[, ncol(term_perf)], FUN = "-")

# Create a matrix to store bar heights for each year
bar_heights <- t(differences)

# Define the layout
layout(matrix(1:2, ncol = 1))

# Calculate the differences
differences <- sweep(term_perf, 1, term_perf[,ncol(term_perf)], FUN = "-")

# Create a matrix to store bar heights for each year
bar_heights <- t(differences)

# Create an empty plot
bp <- barplot(
  rep(0, length(years)),
  names.arg = years,
  ylim = c(-0.045, 0.045),
  main = "Performance differences",
  xlab = "Time",
  ylab = "",
  col = "white",
  border=NA
)

# Plot each bar individually with gradient colors based on row index
for (j in 1:ncol(bar_heights)) {
  y<-bar_heights[,j]
  interpolated_values <- approx(pvals, y, xout = refined_pvals)
 for (i in 1:length(refined_pvals)) {
    rect(
      xleft = bp[j] - 0.4, 
      ybottom = interpolated_values$y[i]-0.0001,
      xright = bp[j] + 0.4, 
      ytop = interpolated_values$y[i]+0.0001, 
      col = color_palette[i], 
      border = NA
    )
  }
}

# Read and prepare the entropy dataset
ann_ddiv <- read_csv("Annual_Change_Entropy.csv", col_types = cols(.default = "d"))
ann_ddiv <- as.data.frame(ann_ddiv)[[1]]

# Create an empty plot
bp <- barplot(
  rep(0, length(years)),
  names.arg = years,
  ylim = c(-0.035, 0.025),
  main = "Annual change in entropy",
  xlab = "Time",
  ylab = "",
  col = "white",
  border=NA
)

# Plot each bar individually for range of performance
for (j in 1:ncol(bar_heights)) {
  rect(
    xleft = bp[j] - 0.4, 
    ybottom = min(ann_ddiv[j],0),
    xright = bp[j] + 0.4, 
    ytop = max(ann_ddiv[j],0),
    col = rgb(0.5,0.5,0.5,0.4), 
    border = NA
  )
}

