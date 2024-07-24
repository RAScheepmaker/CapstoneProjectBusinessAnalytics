# ################################################
# ECO 6936 - Capstone in Business Analytics II
#
# Remco A. Scheepmaker, Ph.D. 
# June 27, 2024
#
# Script:   CCPortfolioPrototype.R
#
# Code to test the algorithm (which is located inside
# CCPortfolioFunctions.R) and make some inital figures
# to study the behavior and do a sensitivity analysis
# of various variables.
#
# Dependencies: 
# latex2exp    - for printing greek characters in plot titles
# RColorBrewer - for colored lines
# waterfalls   - for waterfall portfolio plots
# ggplot2      - for improving the waterfall plots
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
#install.packages("waterfalls")
library(waterfalls)
library(ggplot2)

# Read the budget selection and card portfolio selection functions
source("CCPortfolioFunctions.R")

# Read the data
cards_data <- read.csv(file = "../Data/CreditCards.csv", header = TRUE)
budget_data <- read.csv(file = '../Data/BudgetIncome.csv', header = TRUE)

# ################################
# USED FOR TESTING:
# income <- 94000 #'avg'
# K <- 2
# eta <- 0.5
# theta <- 0.8
# # Get the budget corresponding to the income bin
# budget <- get_budget(income, budget_data)
# # # # Create an optimal portfolio
# portfolio <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
# ################################



# ##############################################################################
# 1. Horizontal bar plots with the top card on bottom, and 9 other cards on top
# ##############################################################################
opar <- par(no.readonly=TRUE)
# ---------------
income <- "avg"
K <- 9
eta <- 0
theta <- 0
# ---------------
# Get the budget corresponding to the income bin
budget <- get_budget(income, budget_data)
pdf('../Figures/Portfolio_avg_9_0_0.pdf', height = 7, width = 7)
par(las = 1, mar=c(5,9,2,1), cex.axis=0.8)
portfolio_plot1 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
barplot(portfolio_plot1$marginal_benefit, names.arg = portfolio_plot1$cards, horiz = TRUE,
        xlim = c(0,1600), cex.names=0.8, xlab = "Marginal Benefit [$]", 
#        main = TeX(sprintf(r'(Income = \$%dk   $\eta$ = %.1f   $\theta$ = %.1f)', round(94003/1000,2), eta, theta))) 
        main = TeX(sprintf(r'(Income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', income, eta, theta))) 
abline(v = 50, col = "red", lwd = 2)
text(c(50), c(11), c('$50'), cex = 0.8, pos = 4, col = 'red') 
dev.off()

# ---------------
income <- "avg"
K <- 9
eta <- 0.5
theta <- 0.5
# ---------------
# Get the budget corresponding to the income bin
budget <- get_budget(income, budget_data)
pdf('../Figures/Portfolio_avg_9_05_05.pdf', height = 7, width = 7)
par(las = 1, mar = c(5,9,2,1), cex.axis = 0.8)
portfolio_plot1 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
barplot(portfolio_plot1$marginal_benefit, names.arg = portfolio_plot1$cards, horiz = TRUE,
        xlim = c(0,1600), cex.names = 0.8, xlab = "Marginal Benefit [$]",
#        main = TeX(sprintf(r'(Income = \$%dk   $\eta$ = %.1f   $\theta$ = %.1f)', round(94003/1000,2), eta, theta))) 
        main = TeX(sprintf(r'(Income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', income, eta, theta))) 
abline(v = 50, col = "red", lwd = 2)
text(c(50), c(11), c('$50'), cex = 0.8, pos = 4, col = 'red') 
dev.off()

# ---------------
income <- "avg"
K <- 9
eta <- 1
theta <- 1
# ---------------
# Get the budget corresponding to the income bin
budget <- get_budget(income, budget_data)
pdf('../Figures/Portfolio_avg_9_1_1.pdf', height = 7, width = 7)
par(las = 1, mar = c(5,9,2,1), cex.axis = 0.8)
portfolio_plot1 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
barplot(portfolio_plot1$marginal_benefit, names.arg = portfolio_plot1$cards, horiz = TRUE,
        xlim = c(0,1600), cex.names = 0.8, xlab = "Marginal Benefit [$]",
#        main = TeX(sprintf(r'(Income = \$%dk   $\eta$ = %.1f   $\theta$ = %.1f)', round(94003/1000,2), eta, theta))) 
        main = TeX(sprintf(r'(Income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', income, eta, theta))) 
abline(v = 50, col = "red", lwd = 2)
text(c(50), c(11), c('$50'), cex = 0.8, pos = 4, col = 'red') 
dev.off()
par(opar)
# ##############################################################################


# ##############################################################################
# 2. 3 panels: eta, theta = (0,0), (0.5, 0.5) and (1,1) and then
# marginal benefit vs number of cards for 10 lines of income, the avg income 
# being thicker. $50 overplotted as red line.
# ##############################################################################
opar <- par(no.readonly=TRUE)

# ---------------
income <- list('avg', 22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 9
eta <- 0
theta <- 0
# ---------------
MB <- matrix(0, nrow = length(income), ncol = K)
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot2 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  MB[i,] <- portfolio_plot2$marginal_benefit
}
mycolors <- brewer.pal(length(income)-1, "Dark2")
pdf('../Figures/MBvsKvsIncome_0_0.pdf', height = 7, width = 7)
par(mar=c(4,4,2,1), cex.axis=1)
plot(0,0, type = 'l', xlim = c(1,9), ylim = c(0,800), lwd = 3, xaxt = 'n', 
        xlab = "Card #", ylab = "Marginal Benefit [$]", 
          main = TeX(sprintf(r'($\eta$ = %.1f    $\theta$ = %.1f)', eta, theta))) 
     #main = sprintf('Transfer Partners: %.d%%    Benefits: %.d%%', eta*100, theta*100))
axis(1, at = seq(1, 9))
for (i in 2:length(income)){
  lines(1:K, MB[i, ], type = 'b', col = mycolors[i-1], lwd = 2, pch = i)
}
lines(1:K, MB[1, ], type = 'b', col = 'black', lwd = 3, lty = 2, pch = 1)
abline(h = 50, col = "black", lwd = 2, lty = 5)
text(1, 70, c('$50'), cex = 1, pos = 4, col = 'black') 
income_legend <- rev(c('$22k', '$35k', '$45k', '$59k', '$84k', '$122k', '$171k', '$323k', '$94k'))
colors_legend <- rev(append(mycolors, c("#000000")))
legend('topright', title="Income", income_legend, col = colors_legend, 
       lwd = c(3, 2,2,2,2,2,2,2,2), lty = c(2, 1,1,1,1,1,1,1,1), pch = c(1, 9,8,7,6,5,4,3,2))
dev.off()


# ---------------
income <- list('avg', 22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 9
eta <- 0.5
theta <- 0.5
# ---------------
MB <- matrix(0, nrow = length(income), ncol = K)
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot2 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  MB[i,] <- portfolio_plot2$marginal_benefit
}
mycolors <- brewer.pal(length(income)-1, "Dark2")
pdf('../Figures/MBvsKvsIncome_05_05.pdf', height = 7, width = 7)
par(mar=c(4,4,2,1), cex.axis=1)
plot(0,0, type = 'l', xlim = c(1,9), ylim = c(0,800), lwd = 3, xaxt = 'n', 
     xlab = "Card #", ylab = "Marginal Benefit [$]", 
          main = TeX(sprintf(r'($\eta$ = %.1f    $\theta$ = %.1f)', eta, theta))) 
     #main = sprintf('Transfer Partners: %.d%%    Benefits: %.d%%', eta*100, theta*100))
axis(1, at = seq(1, 9))
for (i in 2:length(income)){
  lines(1:K, MB[i, ], type = 'b', col = mycolors[i-1], lwd = 2, pch = i)
}
lines(1:K, MB[1, ], type = 'b', col = 'black', lwd = 3, lty = 2, pch = 1)
abline(h = 50, col = "black", lwd = 2, lty = 5)
text(1, 70, c('$50'), cex = 1, pos = 4, col = 'black') 
income_legend <- rev(c('$22k', '$35k', '$45k', '$59k', '$84k', '$122k', '$171k', '$323k', '$94k'))
colors_legend <- rev(append(mycolors, c("#000000")))
legend('topright', title="Income", income_legend, col = colors_legend, 
       lwd = c(3, 2,2,2,2,2,2,2,2), lty = c(2, 1,1,1,1,1,1,1,1), pch = c(1, 9,8,7,6,5,4,3,2))
dev.off()


# ---------------
income <- list('avg', 22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 9
eta <- 1
theta <- 1
# ---------------
MB <- matrix(0, nrow = length(income), ncol = K)
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot2 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  MB[i,] <- portfolio_plot2$marginal_benefit
}
mycolors <- brewer.pal(length(income)-1, "Dark2")
pdf('../Figures/MBvsKvsIncome_1_1.pdf', height = 7, width = 7)
par(mar=c(4,4,2,1), cex.axis=1)
plot(0,0, type = 'l', xlim = c(1,9), ylim = c(0,800), lwd = 3, xaxt = 'n', 
     xlab = "Card #", ylab = "Marginal Benefit [$]", 
          main = TeX(sprintf(r'($\eta$ = %.1f    $\theta$ = %.1f)', eta, theta))) 
     #main = sprintf('Transfer Partners: %.d%%    Benefits: %.d%%', eta*100, theta*100))
axis(1, at = seq(1, 9))
for (i in 2:length(income)){
  lines(1:K, MB[i, ], type = 'b', col = mycolors[i-1], lwd = 2, pch = i)
}
lines(1:K, MB[1, ], type = 'b', col = 'black', lwd = 3, lty = 2, pch = 1)
abline(h = 50, col = "black", lwd = 2, lty = 5)
text(1, 70, c('$50'), cex = 1, pos = 4, col = 'black') 
income_legend <- rev(c('$22k', '$35k', '$45k', '$59k', '$84k', '$122k', '$171k', '$323k', '$94k'))
colors_legend <- rev(append(mycolors, c("#000000")))
legend('topright', title="Income", income_legend, col = colors_legend, 
       lwd = c(3, 2,2,2,2,2,2,2,2), lty = c(2, 1,1,1,1,1,1,1,1), pch = c(1, 9,8,7,6,5,4,3,2))
dev.off()

par(opar)
# ##############################################################################



# ##############################################################################
# 3. 3 panels: eta, theta = (0,0), (0.5, 0.5) and (1,1) and then
# marginal ROS (Return-On-Spend) vs number of cards for 10 lines of income, the avg income 
# being thicker. 
# ##############################################################################
opar <- par(no.readonly=TRUE)

# ---------------
income <- list('avg', 22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 9
eta <- 0
theta <- 0
# ---------------
ROS <- matrix(0, nrow = length(income), ncol = K)
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot3 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  ROS[i,] <- portfolio_plot3$return_on_spend * 100
}
mycolors <- brewer.pal(length(income)-1, "Dark2")
pdf('../Figures/ROSvsKvsIncome_0_0.pdf', height = 7, width = 7)
par(mar=c(4,4,2,1), cex.axis=1)
plot(0,0, type = 'l', xlim = c(1,9), ylim = c(2,8), lwd = 3, xaxt = 'n', 
     xlab = "Card #", ylab = "Return on Spend [%]", 
          main = TeX(sprintf(r'($\eta$ = %.1f    $\theta$ = %.1f)', eta, theta))) 
    # main = sprintf('Transfer Partners: %.d%%    Benefits: %.d%%', eta*100, theta*100))
axis(1, at = seq(1, 9))
for (i in 2:length(income)){
  lines(1:K, ROS[i, ], type = 'b', col = mycolors[i-1], lwd = 2, pch = i)
}
lines(1:K, ROS[1, ], type = 'b', col = 'black', lwd = 3, lty = 2, pch = 1)
income_legend <- rev(c('$22k', '$35k', '$45k', '$59k', '$84k', '$122k', '$171k', '$323k', '$94k'))
colors_legend <- rev(append(mycolors, c("#000000")))
legend('topleft', title="Income", income_legend, col = colors_legend, cex = 1,
       lwd = c(3, 2,2,2,2,2,2,2,2), lty = c(2, 1,1,1,1,1,1,1,1), pch = c(1, 9,8,7,6,5,4,3,2))
dev.off()


# ---------------
income <- list('avg', 22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 9
eta <- 0.5
theta <- 0.5
# ---------------
ROS <- matrix(0, nrow = length(income), ncol = K)
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot3 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  ROS[i,] <- portfolio_plot3$return_on_spend * 100
}
mycolors <- brewer.pal(length(income)-1, "Dark2")
pdf('../Figures/ROSvsKvsIncome_05_05.pdf', height = 7, width = 7)
par(mar=c(4,4,2,1), cex.axis=1)
plot(0,0, type = 'l', xlim = c(1,9), ylim = c(2,8), lwd = 3, xaxt = 'n', 
     xlab = "Card #", ylab = "Return on Spend [%]", 
          main = TeX(sprintf(r'($\eta$ = %.1f    $\theta$ = %.1f)', eta, theta))) 
     #main = sprintf('Transfer Partners: %.d%%    Benefits: %.d%%', eta*100, theta*100))
axis(1, at = seq(1, 9))
for (i in 2:length(income)){
  lines(1:K, ROS[i, ], type = 'b', col = mycolors[i-1], lwd = 2, pch = i)
}
lines(1:K, ROS[1, ], type = 'b', col = 'black', lwd = 3, lty = 2, pch = 1)
income_legend <- rev(c('$22k', '$35k', '$45k', '$59k', '$84k', '$122k', '$171k', '$323k', '$94k'))
colors_legend <- rev(append(mycolors, c("#000000")))
legend('topleft', title="Income", income_legend, col = colors_legend, cex = 1,
       lwd = c(3, 2,2,2,2,2,2,2,2), lty = c(2, 1,1,1,1,1,1,1,1), pch = c(1, 9,8,7,6,5,4,3,2))
dev.off()


# ---------------
income <- list('avg', 22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 9
eta <- 1
theta <- 1
# ---------------
ROS <- matrix(0, nrow = length(income), ncol = K)
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot3 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  ROS[i,] <- portfolio_plot3$return_on_spend * 100
}
mycolors <- brewer.pal(length(income)-1, "Dark2")
pdf('../Figures/ROSvsKvsIncome_1_1.pdf', height = 7, width = 7)
par(mar=c(4,4,2,1), cex.axis=1)
plot(0,0, type = 'l', xlim = c(1,9), ylim = c(2,8), lwd = 3, xaxt = 'n', 
     xlab = "Card #", ylab = "Return on Spend [%]", 
     main = TeX(sprintf(r'($\eta$ = %.1f    $\theta$ = %.1f)', eta, theta))) 
#     main = sprintf('Transfer Partners: %.d%%    Benefits: %.d%%', eta*100, theta*100))
axis(1, at = seq(1, 9))
for (i in 2:length(income)){
  lines(1:K, ROS[i, ], type = 'b', col = mycolors[i-1], lwd = 2, pch = i)
}
lines(1:K, ROS[1, ], type = 'b', col = 'black', lwd = 3, lty = 2, pch = 1)
income_legend <- rev(c('$22k', '$35k', '$45k', '$59k', '$84k', '$122k', '$171k', '$323k', '$94k'))
colors_legend <- rev(append(mycolors, c("#000000")))
legend('topleft', title="Income", income_legend, col = colors_legend, cex = 1,
       lwd = c(3, 2,2,2,2,2,2,2,2), lty = c(2, 1,1,1,1,1,1,1,1), pch = c(1, 9,8,7,6,5,4,3,2))
dev.off()

par(opar)
# ##############################################################################



# ##############################################################################
# 4. 3 panels: K = 5. eta, theta = (0,0), (0.5, 0.5) and (1,1) and then
# Net Benefit vs Income.
# ##############################################################################
pdf('../Figures/NBvsIncome_K5.pdf', height = 7, width = 7)
# ---------------
income <- c(22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
K <- 5
eta <- 0
theta <- 0
# ---------------
NB <- c()
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot4 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  NB <- append(NB, tail(portfolio_plot4$net_benefit, n=1))
}
plot(income, NB, type = 'l', xlim = c(0,360000), ylim = c(0,6000), lwd = 2, lty = 3,
     xlab = "Income [$]", ylab = "Net Benefit [$]",
     main = TeX(sprintf(r'(K = %d)', K)))

eta <- 0.5
theta <- 0.5
NB <- c()
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot4 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  NB <- append(NB, tail(portfolio_plot4$net_benefit, n=1))
}
lines(income, NB, type = 'l', col = 'black', lwd = 2, lty = 1)

eta <- 0.5
theta <- 1
NB <- c()
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot4 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  NB <- append(NB, tail(portfolio_plot4$net_benefit, n=1))
}
lines(income, NB, type = 'l', col = 'black', lwd = 2, lty = 4)


eta <- 1
theta <- 1
NB <- c()
for (i in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[i]], budget_data)
  portfolio_plot4 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = FALSE)
  NB <- append(NB, tail(portfolio_plot4$net_benefit, n=1))
}
lines(income, NB, type = 'l', col = 'black', lwd = 2, lty = 2)

legend('topleft', title=TeX(r'(($\eta$, $\theta$))'), c("(1, 1)","(0.5, 1)","(0.5, 0.5)","(0, 0)"), 
       lwd = c(2,2,2,2), lty = c(2,4,1,3))

dev.off()



# ##############################################################################
# 5. 3 pannels: for K = 5 and Income = (45k, avg, 160k): 
# Return-On-Spend filled contour plot for theta (y-axis) vs eta (x-axis)
# ##############################################################################
# ---------------
income <- list(45000, 'avg', 160000)
K <- 4
eta <- seq(0,1,0.1) 
theta <- seq(0,1,0.1) 
ROS <- array(rep(0, length(income)*length(eta)*length(theta)), c(length(income), length(eta), length(theta)))
# ---------------
for (m in 1:length(income)) {
  # Get the budget corresponding to the income bin
  budget <- get_budget(income[[m]], budget_data)
  for (i in 1:length(eta)) {
    for (j in 1:length(theta)){
      portfolio_plot5 <- get_portfolio(K, eta[i], theta[j], cards_data, budget, verbose = FALSE)
      ROS[m,i,j] <- tail(portfolio_plot5$return_on_spend, n=1)
    }
  }
}

pdf('../Figures/ROSvsEtaTheta_K5_Inc45k.pdf', height = 7, width = 7)
filled.contour(eta, theta, ROS[1, ,]*100, levels = seq(2,7, 0.25),
  plot.title = title(main = sprintf('Income = %s    K = %d', income[1], K),
                     xlab =  TeX(r'($\eta$)'), ylab = TeX(r'($\theta$)')),
  key.title = title(main = "ROS [%]"),
  plot.axes = {
  axis(1)
  axis(2)
  contour(eta, theta, ROS[1, ,]*100, levels = seq(2,7, 0.5),labcex = 0.8, add = TRUE, lwd = 1)
  }
)
dev.off()

pdf('../Figures/ROSvsEtaTheta_K5_IncAvgk.pdf', height = 7, width = 7)
filled.contour(eta, theta, ROS[2, ,]*100, levels = seq(2,7, 0.25),
               plot.title = title(main = sprintf('Income = %s    K = %d', income[2], K),
                                  xlab =  TeX(r'($\eta$)'), ylab = TeX(r'($\theta$)')),
               key.title = title(main = "ROS [%]"),
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(eta, theta, ROS[2, ,]*100, levels = seq(2,7, 0.5),labcex = 0.8, add = TRUE, lwd = 1)
               }
)
dev.off()

pdf('../Figures/ROSvsEtaTheta_K5_Inc160k.pdf', height = 7, width = 7)
filled.contour(eta, theta, ROS[3, ,]*100, levels = seq(2,7, 0.25),
               plot.title = title(main = sprintf('Income = %s    K = %d', income[3], K),
                                  xlab =  TeX(r'($\eta$)'), ylab = TeX(r'($\theta$)')),
               key.title = title(main = "ROS [%]"),
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(eta, theta, ROS[3, ,]*100, levels = seq(2,7, 0.5),labcex = 0.8, add = TRUE, lwd = 1)
               }
)
dev.off()


# ##############################################################################
# 6. Waterfalls Portfolio Plots.
# ##############################################################################
# ---------------
income <- "avg" #94000 #
K <- 8
eta <- 0
theta <- 0
# ---------------
# Get the budget corresponding to the income bin
budget <- get_budget(income, budget_data)
pdf('../Figures/Waterfall_avg_8_0_0.pdf', height = 7, width = 7)
portfolio_plot6 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
waterfall(values = portfolio_plot6$marginal_benefit, labels = portfolio_plot6$cards,
          calc_total = TRUE, draw_lines = FALSE, total_rect_color = "orange",
          total_rect_text_color = "black", rect_width = 0.9, rect_text_size = 1.5,
          put_rect_text_outside_when_value_below = 0.2 * (max(cumsum(portfolio_plot6$marginal_benefit)) -
                                                            min(cumsum(portfolio_plot6$marginal_benefit))),
          rect_text_labels = scales::dollar_format(accuracy = 1)(portfolio_plot6$marginal_benefit), 
          total_rect_text  = scales::dollar_format(accuracy = 1)(sum(portfolio_plot6$marginal_benefit))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = TeX(sprintf(r'(K = %d    income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', 
                           K, income, eta, theta)), x = "", y = "Net Benefit") +
  theme(plot.title = element_text(hjust = 0.5, size=14)) +
  theme(axis.title = element_text()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(text=element_text(size=18))
dev.off()

# ---------------
income <- "avg" #94000 #
K <- 8
eta <- 0.5
theta <- 0.5
# ---------------
# Get the budget corresponding to the income bin
budget <- get_budget(income, budget_data)
pdf('../Figures/Waterfall_avg_8_05_05.pdf', height = 7, width = 7)
portfolio_plot6 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
waterfall(values = portfolio_plot6$marginal_benefit, labels = portfolio_plot6$cards,
          calc_total = TRUE, draw_lines = FALSE, total_rect_color = "orange",
          total_rect_text_color = "black", rect_width = 0.9, rect_text_size = 1.5,
          put_rect_text_outside_when_value_below = 0.2 * (max(cumsum(portfolio_plot6$marginal_benefit)) -
                                                            min(cumsum(portfolio_plot6$marginal_benefit))),
          rect_text_labels = scales::dollar_format(accuracy = 1)(portfolio_plot6$marginal_benefit), 
          total_rect_text  = scales::dollar_format(accuracy = 1)(sum(portfolio_plot6$marginal_benefit))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = TeX(sprintf(r'(K = %d    income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', 
                           K, income, eta, theta)), x = "", y = "Net Benefit") +
  theme(plot.title = element_text(hjust = 0.5, size=14)) +
  theme(axis.title = element_text()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(text=element_text(size=18))
dev.off()

# ---------------
income <- "avg"
K <- 8
eta <- 1
theta <- 1
# ---------------
# Get the budget corresponding to the income bin
budget <- get_budget(income, budget_data)
pdf('../Figures/Waterfall_avg_8_1_1.pdf', height = 7, width = 7)
portfolio_plot6 <- get_portfolio(K, eta, theta, cards_data, budget, verbose = TRUE)
waterfall(values = portfolio_plot6$marginal_benefit, labels = portfolio_plot6$cards,
          calc_total = TRUE, draw_lines = FALSE, total_rect_color = "orange",
          total_rect_text_color = "black", rect_width = 0.9, rect_text_size = 1.5,
          put_rect_text_outside_when_value_below = 0.2 * (max(cumsum(portfolio_plot6$marginal_benefit)) -
                                                            min(cumsum(portfolio_plot6$marginal_benefit))),
          rect_text_labels = scales::dollar_format(accuracy = 1)(portfolio_plot6$marginal_benefit), 
          total_rect_text  = scales::dollar_format(accuracy = 1)(sum(portfolio_plot6$marginal_benefit))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = TeX(sprintf(r'(K = %d    income = %s    $\eta$ = %.1f    $\theta$ = %.1f)', 
                           K, income, eta, theta)), x = "", y = "Net Benefit") +
  theme(plot.title = element_text(hjust = 0.5, size=14)) +
  theme(axis.title = element_text()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(text=element_text(size=18))
dev.off()
# ##############################################################################



# ##############################################################################
# 7. Travel and Dining Spend as Fraction of Budget
# ##############################################################################
income <- list(22485, 34988, 44518, 59200, 83894, 121806, 171270, 322568)
travel_frac <- c()
dining_frac <- c()
groceries_frac <- c()
travel_cats <- c("travel_other","hotel_portal", "airline_portal", "car_portal")
for (i in 1:length(income)) {
  budget <- get_budget(income[[i]], budget_data)
  travel_frac <- append(travel_frac, sum(budget[travel_cats]) / sum(budget))
  dining_frac <- append(dining_frac, sum(budget["dining"]) / sum(budget))
  groceries_frac <- append(groceries_frac, sum(budget["groceries"]) / sum(budget))
}
pdf('../Figures/TravelDiningFraction.pdf', height = 7, width = 7)
plot(income, travel_frac, type = 'b', pch = 1, lwd = 2, ylim = c(0,0.2),
     xlab = "Average Income [$]", ylab = "Fraction", main = "")
#main = "Spending as fraction of budget"
lines(income, dining_frac, type = 'b', pch = 0, col = "blue", lwd = 2, lty = 2)
lines(income, groceries_frac, type = 'b', pch = 2, col = "red", lwd = 2, lty = 3)
legend('bottomright', c("Travel", "Dining","Groceries"), col = c("black", "blue", "red"), pch = c(1,0,2), lty = c(1,2,3), lwd = 2)
dev.off()
