## R Code file to plot capital distribution curves 
## (full market and top K stocks)


# Load Libraries

library(readr)
library(tidyr)
library(latex2exp)
library(plotly)

# Reset Environment

rm(list=ls())

# Import caps

caps <- read_csv("caps_common_1962_2023_ffill.csv",
                 col_types =  cols(.default = "d"))
caps <- as.data.frame(caps)

# Import dates

dates <-  read_csv("dates_common_1962_2023.csv", 
                   col_types = cols(dates = col_date(format = "%Y-%m-%d")))
dates <- as.data.frame(dates)[[1]]

##  Illustrate (Full) Capital Distributions in 2D

# Compute (full) market weights
mu<-caps
mu[is.na(mu)]<-0
mu<-sweep(mu,2,colSums(mu),'/')

# Define ranks
ranks<-seq(1,nrow(mu),1)

# Create Plot
#pdf(file = "capdist_full.pdf", width = 6, height = 5)
M<-ncol(mu)
color_palette <- colorRampPalette(c("yellow", "red"),alpha=0.05)(M)
par(mfrow = c(1, 1))
plot(log(ranks,10),log(-sort(-mu[,1]),10),col=color_palette[1],
     ylim=c(-9.5,-0.5),xlim=c(0,4),
     xlab="Rank", ylab="Weight",type='l',xaxt="n",yaxt="n",
     main="Capital distribution curves")
axis(2,at=c(-2,-4,-6,-8),labels=c(latex2exp::TeX("$10^{-2}$"),latex2exp::TeX("$10^{-4}$"),
                                  latex2exp::TeX("$10^{-6}$"),latex2exp::TeX("$10^{-8}$")))
axis(1,at=c(0,1,2,3,4),labels=c(1,10,100,1000,10000))
for(i in seq(1,M,100)){
  lines(log(ranks,10),log(-sort(-mu[,i]),10),col=color_palette[i])
}
#dev.off()

##  Illustrate Top N Capital Distributions in 2D

# Repeat analysis for the market composed of the largest N stocks
N<-1000
mu_N_sorted <- caps
mu_N_sorted[is.na(mu_N_sorted)] <- 0
mu_N_sorted <- -apply(-mu_N_sorted,2,sort)
mu_N_sorted <- mu_N_sorted[1:N,]
mu_N_sorted<-sweep(mu_N_sorted,2,colSums(mu_N_sorted),'/')

# Ranks corresponding to top N
ranks_N<-seq(1,N,1)

# Create Plot
par(mfrow = c(1, 1))
plot(log(ranks_N,10),log(-sort(-mu_N_sorted[,1]),10),col=color_palette[1],
     ylim=c(-4.5,-0.5),xlim=c(0,3),
     xlab="Rank", ylab="Weight",type='l',xaxt="n",yaxt="n")
axis(2,at=c(-1,-2,-3,-4),labels=c(latex2exp::TeX("$10^{-1}$"),latex2exp::TeX("$10^{-2}$"),
                                  latex2exp::TeX("$10^{-3}$"),latex2exp::TeX("$10^{-4}$")))
axis(1,at=c(0,1,2,3),labels=c(1,10,100,1000))
for(i in seq(1,M,100)){
  lines(log(ranks_N,10),log(-sort(-mu_N_sorted[,i]),10),col=color_palette[i])
}




## Illustrate Top N Capital Distributions in 3D
# Extract subset of dates for plotting
date_ids<-seq(1,length(dates),50)
time_vector<-dates[date_ids]

# Save log weights and ranks
lmu_N<-log(mu_N_sorted[,date_ids],10)
lranks_N<-log(ranks_N,10)

# Normalize time for color gradient
time_normalized <- as.numeric(time_vector - min(time_vector)) / as.numeric(max(time_vector) - min(time_vector))

# Create the 3D plot
plot_ly(z = ~lmu_N, y=~lranks_N, x=~time_vector,type = "surface",
        showscale=FALSE,
        colorscale = list(c(0, "yellow"), c(1, "red")),
        cauto = FALSE, cmin = 0, cmax = 1,
        surfacecolor = ~matrix(rep(time_normalized, each = nrow(lmu_N)), 
                               ncol = ncol(lmu_N), byrow = FALSE)) %>%
        layout(scene = list(xaxis = list(title = "Time"),
                      yaxis = list(title = "Rank", tickvals = c(0,1,2,3),
                                   ticktext = c(1,10,100,1000)),
                      zaxis = list(title = "Weight",
                                   tickvals = c(-1, -2, -3, -4),
                                   ticktext = c("10<sup>-1</sup>", "10<sup>-2</sup>", "10<sup>-3</sup>", "10<sup>-4</sup>"))))
