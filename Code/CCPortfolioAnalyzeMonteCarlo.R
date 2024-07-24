# ################################################
# ECO 6936 - Capstone in Business Analytics II
#
# Remco A. Scheepmaker, Ph.D. 
# July 1, 2024
#
# Script:   CCPortfolioAnalyzeMonteCarlo.R
#
# Code to analyze the Monte Carlo Simulation output from CCPortfolioRunMonteCarlo.R.
#
# Dependencies: 
# latex2exp    - for printing greek characters in plot titles
# RColorBrewer - for colored lines
# MASS         - for fitting lognormal distribution
# ggplot       - for barplot
# ################################################

##################################################
# Preparing the Workspace and Reading Data
##################################################

# Clear workspace
rm(list = ls(all = TRUE))

#install.packages('latex2exp')
library(latex2exp)
#install.packages('RColorBrewer')
library(RColorBrewer)
library(MASS)
library(ggplot2)

# Read the income distribution data
# https://data.census.gov/mdat/#/search?ds=ACSPUMS1Y2022&vv=*FINCP(1:9999999)&rv=ucgid&wt=WGTP&g=795P200US1200101,1200102,1200398,1200500,1200798,1200901,1200902,1200903,1200904,1200905,1201101,1201102,1201103,1201104,1201105,1201106,1201107,1201108,1201109,1201110,1201111,1201112,1201113,1201114,1201115,1201116,1201117,1201398,1201500,1201700,1201901,1201902,1202101,1202102,1202103,1202799,1203101,1203102,1203103,1203104,1203105,1203106,1203107,1203301,1203302,1203303,1203500,1204798,1205199,1205300,1205701,1205702,1205703,1205704,1205705,1205706,1205707,1205708,1205709,1205710,1205711,1205998,1206100,1206901,1206902,1206903,1207101,1207102,1207103,1207104,1207105,1207106,1207301,1207302,1208101,1208102,1208103,1208301,1208302,1208303,1208500,1208601,1208602,1208603,1208604,1208605,1208606,1208607,1208608,1208609,1208610,1208611,1208612,1208613,1208614,1208615,1208616,1208617,1208618,1208619,1208620,1208621,1208622,1208623,1208624,1208625,1209101,1209102,1209501,1209502,1209503,1209504,1209505,1209506,1209507,1209508,1209509,1209510,1209701,1209702,1209703,1209901,1209902,1209903,1209904,1209905,1209906,1209907,1209908,1209909,1209910,1209911,1210101,1210102,1210103,1210104,1210301,1210302,1210303,1210304,1210305,1210306,1210307,1210308,1210501,1210502,1210503,1210504,1210505,1210799,1210902,1210903,1211101,1211102,1211103,1211300,1211501,1211502,1211503,1211701,1211702,1211703,1211704,1211900,1212701,1212702,1212703,1212704
family_income <- read.csv(file = "../Data/ACSPUMS1Y2022_2024-06-25T210150.csv", header = TRUE)
family_income <- family_income$FINCP

# Read the Monte Carlo Output
df_MC <- read.csv(file = '../Data/MCOutput_M100k.csv', header = TRUE)
# Select only incomes above $10k to limit spending much more than income
df_sub <- subset(df_MC, income >= 10000)
# Retrieve the number of simulations
M <- nrow(df_MC)
# Read the card popularity output
cards <- read.csv(file = '../Data/MCCardPopularity_M100k.csv', header = TRUE)
colnames(cards) <- c("name", "count")

# #################################################
# Fit all family incomes, but only show incomes below $350,000
# #################################################
pdf('../Figures/IncomeDistribution.pdf', height = 7, width = 7)
f2 <- fitdistr(family_income,"lognormal")
hist(family_income[family_income < 350000], breaks = 15, freq = FALSE, 
     ylim = c(0,8e-6), xlab = "Family Income [$]", main = "")
# main = "2022 Florida Income Distribution"
curve(dlnorm(x, f2$estimate[["meanlog"]], f2$estimate[["sdlog"]]), add = TRUE, col = "blue", lwd = 2)
legend('topright', 'lognormal fit', col = "blue", lwd = 2)
dev.off()
sprintf('Mean observed income: %.2f', mean(family_income))
sprintf('Mean fitted income: %.2f', exp(f2$estimate[[1]] + 0.5*f2$estimate[[2]]^2 ))

# #################################################
# Top 10 most recommended cards
# #################################################
cards <- cards[order(cards$count, decreasing = TRUE) , ]
df_popularity <- data.frame(name = cards[1:10,"name"], fraction = cards[1:10, "count"] / M)
pdf('../Figures/MC_Popularity_M100k.pdf', height = 7, width = 7)
ggplot(df_popularity, aes(x = reorder(name, -fraction), y = fraction)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
#  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
#  labs(title = "Top 10 Cards", x = "", y = "Fraction of portfolios holding the card")
   labs(x = "", y = "Fraction of portfolios holding the card")
dev.off()

# #################################################
# ROS vs Income
# #################################################

# First tried different other plotting styles to deal with the large amount of points:

# ggplot(data = subset(df_MC, income >= 15000), mapping=aes(x = income, y = return_on_spend)) + 
#   geom_point() +
#   geom_smooth() +
#   scale_x_log10(labels=scales::label_dollar())

# d = densCols(df_sub$income, df_sub$return_on_spend, colramp = colorRampPalette(rev(rainbow(10, end = 4/6))))
# ggplot(df_sub, aes(x = income, y = return_on_spend)) +
#   geom_point(aes(income, return_on_spend, col = d), size = 1) +
#   scale_color_identity() +
#   scale_x_log10()

# library(hexbin)
# #install.packages("hexbin")
# with(df_sub, {
#   bin <- hexbin(log10(income), 100*return_on_spend, xbins=50)
#   plot(bin, main="Hexagonal Binning with 10,000 Observations") })

# Finally settled on a "smoothScatter" plot:
pdf('../Figures/MC_ROS_vs_Income.pdf', height = 7, width = 7)
with(df_sub, smoothScatter(log10(income), 100*return_on_spend, 
                   xlab = "Income", xlim = c(log10(10000), log10(2500000)),
                   ylab = "Return on Spend [%]", xaxt = 'n' )) 
axis(1, at = log10(c(10000, 94000, 150000, 200000, 300000, 1000000)), labels = c("$10k", "$94k", "$150k","$200k","$300k", "$1m"))
lw1 <- loess(100*return_on_spend ~ log10(income), data = df_sub)
j <- order(df_sub$income)
lines(log10(df_sub$income[j]), lw1$fitted[j], col="red", lwd=3)
text(log10(c(150000, 300000)), c(1.8,1.8), c("150k","$300k"), cex = 1, pos = 3, col = 'black') 
legend('topright', 'loess curve', col = "red", lwd = 3)
dev.off()

# #################################################
# ROS vs Number of Cards (K)
# #################################################
# with(df_sub, smoothScatter(number_of_cards, 100*return_on_spend, 
#                            xlab = "Number of Cards",
#                            ylab = "Return on Spend [%]")) 

pdf('../Figures/MC_ROS_vs_K.pdf', height = 7, width = 7)
boxplot(100 * return_on_spend ~ number_of_cards, data = df_sub, 
        xlab="Number of Cards", ylab="Return on Spend [%]")
abline(h = mean(100*df_sub$return_on_spend), lwd = 2, lty = 2, col = "red")
text(c(0.6), c(100*mean(df_sub$return_on_spend)), sprintf("%.2f%%",100*mean(df_sub$return_on_spend)), cex = 1, pos = 3, col = 'red') 
dev.off()

# #################################################
# Net Benefit vs Income
# #################################################
pdf('../Figures/MC_NetBenefit_vs_Income.pdf', height = 7, width = 7)
with(df_sub, smoothScatter(log10(income), log10(net_benefit),
                           xlab = "Income", xlim = c(log10(10000), log10(2500000)),
                           ylab = "Net Benefit", xaxt = 'n', yaxt = 'n' ))
axis(1, at = log10(c(10000, 100000, 1000000)), labels = c("$10,000", "$100,000", "$1,000,000"))
axis(2, at = log10(c(100, 1000, 10000)), labels = c("$100", "$1,000", "$10,000"))
lw2 <- loess(log10(net_benefit) ~ log10(income), data = df_sub)
j <- order(df_sub$income)
lines(log10(df_sub$income[j]), lw2$fitted[j], col="red", lwd=3)
dev.off()


# #################################################
# Histogram of ROS
# #################################################

pdf('../Figures/MC_ROS_Histogram.pdf', height = 7, width = 7)
ROS <- 100 * df_sub$return_on_spend
hist(ROS, prob = TRUE, breaks = 20, xlab = "Return on Spend [%]", main = "")
# Overplot a Normal curve
x2 <- seq(min(ROS), max(ROS), length = 40)
fun <- dnorm(x2, mean = mean(ROS), sd = sd(ROS))
lines(x2, fun, col = "red", lwd = 2, lty = 2)
# Overplot a Lognormal curve
f3 <- fitdistr(ROS,"lognormal")
curve(dlnorm(x, f3$estimate[["meanlog"]], f3$estimate[["sdlog"]]), add = TRUE, col = "blue", lwd = 2)
# Overplot locations of mean and mode
abline(v = mean(ROS), lwd = 3, lty = 4, col = "red")
mode <- exp(f3$estimate[["meanlog"]] - f3$estimate[["sdlog"]]^2)
abline(v = mode, lwd = 2, lty = 5, col = "blue")
# Label them
text(c(mean(ROS)), c(0.55), sprintf("%.2f%%",mean(ROS)), cex = 0.8, pos = 4, col = 'red') 
text(c(mode)-0.3, c(0.55), sprintf("%.2f%%",mode), cex = 0.8, pos = 2, col = 'blue') 
legend('topright', c('lognormal', 'mode','normal', 'mean'), col = c("blue","blue", "red", "red"), lty = c(1, 5, 2, 4), lwd = 2)
dev.off()

# Checking some statistics
print(c(mean(ROS), sd(ROS)))
print(c(exp(f3$estimate[["meanlog"]]), exp(f3$estimate[["sdlog"]])))
sprintf('Mean fitted lognormal ROS: %.2f', exp(f3$estimate[[1]] + 0.5*f3$estimate[[2]]^2 ))
print(median(ROS))

# #################################################
# Histogram of Net Benefit
# #################################################

# NEEDS LEGEND!!! BUT I'M NOT SURE WHY I NEED THIS PLOT
# pdf('../Figures/MC_Log10NetBenefit_Histogram.pdf', height = 7, width = 7)
# NB <- df_sub$net_benefit
# hist(log10(NB), prob = TRUE, breaks = 20, xlab = "Log10 Net Benefit [$]", main = "")
# f3 <- fitdistr(log10(NB),"normal")
# #f4 <- fitdistr(NB,"lognormal")
# curve(dnorm(x, f3$estimate[["mean"]], f3$estimate[["sd"]]), add = TRUE, col = "blue", lwd = 2)
# dev.off()
# 
# print(c(mean(NB), sd(NB)))
# print(c(10^(f3$estimate[["mean"]]), 10^(f3$estimate[["sd"]])))
# 
