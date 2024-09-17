
# Required Libraries
library(readr)
library(scatterplot3d)

# Reset Environment
rm(list=ls())

## Code to plot regression figure for explaining portfolio performance.

# Define the sequence of years and p-values
years <- seq(1962, 2023, 1)
freq<-10
tcb <- 0.0025

# Create an empty list to store data
data_list <- list()

# Loop through each year and p-value to read the data
for (i in seq(1,length(years),1)) {
  for(p in c(1,0.5,0)){
    year <- years[i]
    f <- freq
    
    # Print the current year and p-value
    print(paste0("YEAR: ", year, ", p=", p))
    
    # Construct the file path
    file_path <- paste0("Backtest_Outputs/lV_tc_", tcb * 10000, "bps", "_freq_",
                        f, "_p_", p, "_", year, "_", year, ".csv")
    
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

term_perf<-rep(0,length(years))
for (i in seq(1,length(years),1)) {
  lV<-get_data(years[i], 0.0)
  lV_bm<-get_data(years[i],1)
  term_perf[i]<-lV[length(lV)]-lV_bm[length(lV_bm)]
}


ann_egr <- read_csv("Annual_EGR.csv", col_types = cols(.default = "d"))
ann_egr<-as.data.frame(ann_egr)[[1]]

ann_ddiv <- read_csv("Annual_Change_Diversity.csv", col_types = cols(.default = "d"))
ann_ddiv<-as.data.frame(ann_ddiv)[[1]]

summary(lm(term_perf~ann_egr+ann_ddiv))


# Assuming your data frame is named df
# Create the linear model
model <- lm(term_perf ~ ann_ddiv + ann_egr)
summary(model)

# Extract fitted values
fitted.values <- model$fitted.values

# Create 3D scatter plot with regression plane
s3d <- scatterplot3d(ann_ddiv, ann_egr, term_perf, pch = 16, highlight.3d = TRUE,  angle=40,
                     main = "",
                     xlab = expression(Delta *log(bold(H))),
                     ylab = "" ,
                     zlab = "Relative performance",
                     cex.lab = 1.3,
                     cex.axis = 1.1,
                     zlim = c(-0.06, 0.06))
s3d$plane3d(model)

# Add custom y-axis label
coords <- s3d$xyz.convert(max(ann_ddiv)+0.085, min(ann_egr)+0.03,  min(term_perf)-0.03)
text(coords$x, coords$y, labels = expression(Delta * Gamma[mu]) , srt = 0, pos = 1, cex = 1.3)



