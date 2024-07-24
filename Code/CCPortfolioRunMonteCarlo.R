# ################################################
# ECO 6936 - Capstone in Business Analytics II
#
# Remco A. Scheepmaker, Ph.D. 
# July 1, 2024
#
# Script:   CCPortfolioRunMonteCarlo.R
#
# Code to run a Monte Carlo Simulation of many credit card portfolios
# corresponding to random draws of income, number of cards, eta (point valuations),
# and theta (use of benefits). WARNING: code takes around two hours to run for
# M = 100,000 simulations!
#
# Dependencies: 
# ################################################

##################################################
# Preparing the Workspace and Reading Data
##################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Read the income distribution data
# https://data.census.gov/mdat/#/search?ds=ACSPUMS1Y2022&vv=*FINCP(1:9999999)&rv=ucgid&wt=WGTP&g=795P200US1200101,1200102,1200398,1200500,1200798,1200901,1200902,1200903,1200904,1200905,1201101,1201102,1201103,1201104,1201105,1201106,1201107,1201108,1201109,1201110,1201111,1201112,1201113,1201114,1201115,1201116,1201117,1201398,1201500,1201700,1201901,1201902,1202101,1202102,1202103,1202799,1203101,1203102,1203103,1203104,1203105,1203106,1203107,1203301,1203302,1203303,1203500,1204798,1205199,1205300,1205701,1205702,1205703,1205704,1205705,1205706,1205707,1205708,1205709,1205710,1205711,1205998,1206100,1206901,1206902,1206903,1207101,1207102,1207103,1207104,1207105,1207106,1207301,1207302,1208101,1208102,1208103,1208301,1208302,1208303,1208500,1208601,1208602,1208603,1208604,1208605,1208606,1208607,1208608,1208609,1208610,1208611,1208612,1208613,1208614,1208615,1208616,1208617,1208618,1208619,1208620,1208621,1208622,1208623,1208624,1208625,1209101,1209102,1209501,1209502,1209503,1209504,1209505,1209506,1209507,1209508,1209509,1209510,1209701,1209702,1209703,1209901,1209902,1209903,1209904,1209905,1209906,1209907,1209908,1209909,1209910,1209911,1210101,1210102,1210103,1210104,1210301,1210302,1210303,1210304,1210305,1210306,1210307,1210308,1210501,1210502,1210503,1210504,1210505,1210799,1210902,1210903,1211101,1211102,1211103,1211300,1211501,1211502,1211503,1211701,1211702,1211703,1211704,1211900,1212701,1212702,1212703,1212704
family_income <- read.csv(file = "../Data/ACSPUMS1Y2022_2024-06-25T210150.csv", 
                        header = TRUE)
family_income <- family_income$FINCP

# Read the budget selection and card portfolio selection functions
source("CCPortfolioFunctions.R")

# Read the cards and budget data
cards_data <- read.csv(file = "../Data/CreditCards.csv", header = TRUE)
budget_data <- read.csv(file = '../Data/BudgetIncome.csv', header = TRUE)

# ##############################################################################
# Monte Carlo Simulation
# Sample M incomes with replacement. 
# We need to store income, K, eta, theta, Return on Spend, and net_benefit
# ##############################################################################
# Initialize "cards" for counting how many times cards appear in portfolios
cardnames <- unique(cards_data[, "name"])
cards <- c()
cards[cardnames] <- 0

# Initialize Return on Spend (ROS) and Net Benefit (NB)
ROS <- c()
NB <- c()

set.seed(1234)
# ---------------
M = 100000
income <- sample(family_income, M, replace = TRUE)
K <- sample.int(8, size = M, replace = TRUE)
eta <- runif(M)
theta <- runif(M)
# ---------------
for (m in 1:M) {
  budget <- get_budget(income[m], budget_data)
  portfolio <- get_portfolio(K[m], eta[m], theta[m], cards_data, budget, verbose = FALSE)
  ROS <- append(ROS, tail(portfolio$return_on_spend, n=1))
  NB <- append(NB, tail(portfolio$net_benefit, n=1))
  cards[portfolio$cards] <- cards[portfolio$cards] + 1
}

# Store the results of the Monte Carlo in a datafram and export to CSV file
df_MC <- data.frame(income = income, number_of_cards = K, eta = eta, theta = theta, 
                    return_on_spend = ROS, net_benefit = NB)
write.csv(df_MC, file = '../Data/MCOutput_M100k.csv')
write.csv(data.frame(cards), file = '../Data/MCCardPopularity_M100k.csv')
# ##############################################################################