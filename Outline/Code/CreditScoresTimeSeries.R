# ################################################
# ECO 6935 - Capstone in Business Analytics I
#
# Remco A. Scheepmaker, Ph.D. 
# May 28, 2024
#
# Script:   CreditScoresTimeSeries.R
#
# Code to plot a time series of my personal
# credit scores, together with new credit card
# application dates
#
# Dependencies:
# 
# ################################################

##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list = ls(all = TRUE))

# Set working directory, if running interactively.
# wd_path <- '/Users/Remco/Documents/UCF/Summer24/Project/'
# setwd(wd_path)

# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
# tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

library(ggplot2)

##################################################
# Load Data
##################################################

in_file_name <- 'CreditScoresTimeSeries.csv'
in_path <- sprintf('%s/%s', data_dir, in_file_name)
cscores <- read.csv(file = in_path,
                     header = TRUE)
# Transform date
cscores$Date <- as.Date(cscores$Date, "%m/%d/%Y")

# Need to reshape the data into "long" format, for 
# easier plotting of the 4 scores using 4 colors
cscores_long = reshape(cscores, direction = "long", 
                       varying = list(names(cscores)[2:5]), 
                       v.names = "Score",
                       idvar = c("Date"),
                       timevar = "Type", 
                       times = c("FICO-8 TransUnion", "FICO-8 Experian", 
                                 "Vantage-3 TransUnion", "Vantage-3 Experian"))

##################################################
# Make the Plot
##################################################

fig_file_name <- 'CreditScoresTimeSeries.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name, width = 6, height = 3)
options(repr.plot.width = 6, repr.plot.height =2)  
ggplot(data = cscores_long, aes(x = Date, y = Score, color = Type, linetype = Type)) +
  geom_line(size = 1.1, alpha = 1.0) + 
  labs(x="Date", y="Credit Score") +
  ylim(700, 850) +
  geom_vline(xintercept = as.Date(c("2013-06-01", "2019-03-06", "2019-08-27", 
                                    "2019-09-25", "2020-01-14", "2020-04-22", 
                                    "2021-09-03","2022-05-01", "2023-03-02", 
                                    "2023-08-05")), linetype=3, colour="black")
dev.off()