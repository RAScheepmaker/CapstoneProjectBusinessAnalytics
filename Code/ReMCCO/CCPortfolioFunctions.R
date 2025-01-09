# ################################################
# ECO 6936 - Capstone in Business Analytics II
#
# Remco A. Scheepmaker, Ph.D. 
# June 20, 2024
#
# Script:   CCPortfolioFunctions.R
#
# Defines the functions used to select the budget based
# on income, and to return the optimal credit card portfolio,
# based on income, number of cards (K), eta, and theta.
#
# Dependencies: None
# 
# ################################################

get_budget <- function (income = "avg", budget_data) {
  # Given an income (before taxes), this function returns a named
  # vector with the spend per category.
  # "avg" is accepted as a special input argument to return the 
  # average budget from the Bureau of Labor Statistic's 
  # Consumer Expenditure Survey (2022), corresponding to an average 
  # income of $94,003. If a dollar amount is given as input instead, 
  # the function returns the average budget corresponding to the 
  # closest of 9 income bins.
  
  if (income == "avg") { 
    budget <- t(budget_data[2:19, 2])
  } else if (income < 15000) { 
    budget <- t(budget_data[2:19, 5] * income)
  } else if (income >= 15000 & income < 30000) { 
    budget <- t(budget_data[2:19, 7] * income)
  } else if (income >= 30000 & income < 40000) { 
    budget <- t(budget_data[2:19, 9] * income)
  } else if (income >= 40000 & income < 50000) { 
    budget <- t(budget_data[2:19, 11] * income)
  } else if (income >= 50000 & income < 70000) { 
    budget <- t(budget_data[2:19, 13] * income)
  } else if (income >= 70000 & income < 100000) { 
    budget <- t(budget_data[2:19, 15] * income)
  } else if (income >= 100000 & income < 150000) { 
    budget <- t(budget_data[2:19, 17] * income)
  } else if (income >= 150000 & income < 200000) { 
    budget <- t(budget_data[2:19, 19] * income)
  } else if (income >= 200000) { 
    budget <- t(budget_data[2:19, 21] * income)
  }
  
  budget <- as.numeric(gsub('[$,]', '', budget))
  names(budget) <- budget_data[2:19,1]
  return(budget)
}


get_portfolio <- function (K = 4, eta = 0, theta = 0, cards_data, 
                           budget, verbose = TRUE) {
  # Given K (number of cards), eta (fractional use of travel point value), 
  # theta (fractional use of benefits), the cards data (CreditCards.csv
  # in the form of a dataframe), and a budget (from get_budget()), this 
  # function returns a named list, containing:
  # - the optimal credit card portfolio
  # - the total net benefit [$]
  # - the marginal benefit per card
  # - the return on spend after each additional card [%]
  # - the card assignments for each spend category
  # - the total spend [$]
  
  ##################################################
  # Initialization
  ##################################################
  
  total_spend <- sum(budget)
  # Get the category names (are sorted by highest average spend)
  categories <- names(budget)

  # Specify benefits that should only be valued once (do not include
  # 'benefit_credits', which will be valued per card and are assumed 
  # to be stackable)
  single_use_benefits <- c("benefit_globalentry", "benefit_lounge",
                           "benefit_clear")
  # Benefit values
  ge_value <- 20      # Global Entry / TSA pre value in annual dollars
  lounge_value <- 40  # Lounge Access in dollars per year
  clear_value <- 189  # Clear Credit in dollars per year
  single_use_benefits_values <- c(ge_value, lounge_value, clear_value)
  benefit_value <- sweep(cards_data[ , single_use_benefits], 2, 
                         single_use_benefits_values, '*')

    # Initialize the category spend value matrix
  cat_value <- matrix(0, nrow = length(cards_data[,1]), 
                      ncol = length(categories))
  
  # The portfolio that contains the selected cards in order
  cards <- c()
  # The net and marginal benefit after adding each card to the portfolio
  net_benefit <- c()
  marginal_benefit <- c()
  # Named vector that tracks which card to use for every category
  card_assignments <- c()
  
  ##################################################
  # Calculate Value of Spend per Card and Category (matrix)
  ##################################################
  # K is the number of cards in the portfolio, while 
  # N is the number of cards in the dataset.
  
  for (n in 1:length(cards_data[,1])) {
    for (c in 1:length(categories)) {
      cat <- categories[c]
      cap <- paste0(cat, "_cap")
      
      # weighted average of point value, using eta
      point_value <- eta * cards_data[n, "travel_value"] + (1 - eta) * 
                                          cards_data[n, "base_value"]
      
      if (cards_data[n, cap] == 0) {
        # No spending cap, multiply spend * multiplier * value as usual:
        cat_value[n, c] <- budget[[cat]] * cards_data[n, cat] * 
                           point_value
      } else if (budget[[cat]] <= cards_data[n, cap]) {
        # There is a cap, but we stay below it:
        cat_value[n, c] <- budget[[cat]] * cards_data[n, cat] * 
                           point_value
      } else {
        # We spend more than the cap, so fall back to 1x multiplier 
        # beyond the cap
        cat_value[n, c] <- (cards_data[n, cap] * cards_data[n, cat] + 
                              (budget[[cat]] - cards_data[n, cap]) ) * 
                              point_value
      }
    }
  }
  
  ##################################################
  # Select the Optimal Portfolio
  ##################################################
  
  for (k in 1:K) {
    # Net benefit per card 
    net_benefit_per_card <- rowSums(cat_value)  +
      theta * (rowSums(benefit_value) + cards_data[, "benefit_credits"]) -
      cards_data[, "fee"]
    
    # Pick the card with max net_benefit_per_card:
    max_ind <- which.max(net_benefit_per_card)
    # Look up that card's name
    cardname <- cards_data[ max_ind , "name"] 
    # Add the selected card to the portfolio
    cards <- append(cards, cardname )
    # Store the cumulative net benefit and marginal benefit
    if (k == 1) {
      net_benefit <- append(net_benefit, net_benefit_per_card[ max_ind ])
    } else {
      net_benefit <- append(net_benefit, tail(net_benefit, n=1) + 
                              net_benefit_per_card[ max_ind ])
    }
    marginal_benefit <- append(marginal_benefit, 
                               net_benefit_per_card[ max_ind ])
    
    # Assign spending categories with additional value to the card:
    card_assignments[categories[cat_value[ max_ind ,] > 0] ] <- cardname
    
    # Subtract the selected card's values from the value matrix 
    cat_value <- sweep(cat_value, 2, cat_value[max_ind, ])
    # And set negative categories to zero,
    cat_value[cat_value < 0] <- 0
    # Now we're left with positive categories that have remaining value 
    # that we can retrieve with additional cards in the next iteration.
    
    # Multiply all single-use benefits with (1 - single_use_benefits) 
    # of max_ind, to set them to zero after the first time they 
    # have been selected
    benefit_value <- sweep(benefit_value, 2, 
                           unname(unlist(1 - cards_data[max_ind, 
                                         single_use_benefits])), "*")

    # If the card can only be held once (Amex Platinum, Citi Custom 
    # Cash, etc.) we need to make sure it will not be selected again 
    # for just the benefits alone, or additional rewards in other 
    # custom categories. This is accomplished by setting all the values 
    # for the card, including benefits, to zero after selection. 
    # We use ID to find duplicate cards that can only be held once.
    # If cards can be held multiple times (e.g. BoA Customized Cash), 
    # they will have different ID numbers, if not, the duplicates will 
    # have the same ID.
    same_ind <- which(cards_data[,"id"] == cards_data[max_ind ,"id"])
    cards_data[same_ind, "benefit_credits"] <- 0
    cat_value[same_ind, ] <- 0
    benefit_value[same_ind, ] <- 0
  }
  
  if (verbose == TRUE) {
    # Print output to the screen
    print(sprintf("The optimal portfolio with a total benefit of 
                  $%.2f is:", tail(net_benefit, n=1))) 
    print(cards)
    print(sprintf("The marginal benefits are:"))
    print(marginal_benefit)
    print(sprintf("The total spend is: $%.0f", total_spend))
    print(sprintf("The return on spend is: %.2f%%", 
                  100 * tail(net_benefit, n=1) / total_spend))
    print(sprintf("Use the following card assignments:"))
    print(card_assignments)
  }
  
  # Return a named list
  return(list("cards" = cards, "net_benefit" = net_benefit, 
              "marginal_benefit" = marginal_benefit,
              "return_on_spend" = net_benefit / total_spend,
              "card_assignments" = card_assignments,
              "total_spend" = total_spend))
}