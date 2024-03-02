
# Welcome! This script provides functions to convert between odds and probabilities, calculate changes in probability given odds ratios, and demonstrate the relationship between log-odds and odds. 

# In statistical modeling, understanding the relationship between predictor variables and the response variable often involves interpreting coefficients. For logistic regression models, these coefficients represent changes in log-odds. However, it's essential to understand how these changes in log-odds translate to changes in odds and probabilities.

# One common misconception is that you can directly add odds ratios to calculate the combined effect on odds. While this is true for changes in log-odds, it's not the case for changes in odds. 

# This script will illustrate how to properly interpret and apply odds ratios and log-odds changes in statistical modeling. You'll learn how to compare the log-odds from a summary model and how to correctly combine them to achieve the same changes achieved by multiplying odds.




# Odds and Probability calculators====





# Function to convert odds to probability
odds_to_prob <- function(odds) {
  probability <- odds / (1 + odds)
  return(probability)
}

# Function to convert probability to odds
prob_to_odds <- function(probability) {
  odds <- probability / (1 - probability)
  return(odds)
}

# Function to calculate change in probability given odds ratio
change_in_probability <- function(odds_ratio) {
  change_prob <- (odds_ratio - 1) / odds_ratio
  return(change_prob)
}

# Function to calculate change in odds given odds ratio and current odds
new_odds <- function(current_odds, odds_ratio) {
  new_odds <- current_odds * odds_ratio
  return(new_odds)
}
