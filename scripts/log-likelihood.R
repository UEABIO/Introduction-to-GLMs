
#This R code defines a function log_likelihood_visual which generates simulated data from a normal distribution with given mean and standard deviation. 
# It then calculates and plots the log-likelihood surface for different combinations of mean and standard deviation parameters, 
# providing insight into the likelihood of observing the generated data under different parameter settings.


# Simulated data
set.seed(42)

## Run the function below to set up

log_likelihood_visual <- function(mean = 6, sd = 2){
  
  data <- rnorm(100, mean = mean, sd = sd)
  
  # Define a grid of mu and sigma values
  mu_range <- seq(mean-(sd*2), mean+(sd*2), length.out = 100)
  sigma_range <- seq(sd-sd, sd+sd, length.out = 100)
  sigma_range[sigma_range < 1] <- 1
  grid <- expand.grid(mu = mu_range, sigma = sigma_range)
  
  # Function to calculate log-likelihood for normal distribution
  log_likelihood <- function(mu, sigma, data) {
    n <- length(data)
    -n/2 * log(2 * pi) - n * log(sigma) - 1/(2 * sigma^2) * sum((data - mu)^2)
  }
  
  # Calculate log-likelihood for each combination of mu and sigma
  grid$log_likelihood <- mapply(log_likelihood, mu = grid$mu, sigma = grid$sigma, MoreArgs = list(data = data))
  
  # Plot
  ggplot(grid, aes(x = mu, y = sigma, z = log_likelihood)) +
    geom_contour_filled(aes(fill = after_stat(level)), bins = 20) + # Use geom_contour_filled for filled contour plots
    labs(title = "Log-Likelihood Contour Plot",
         x = expression(mu),
         y = expression(sigma),
         fill = "Log-Likelihood") +
    theme_minimal()
}


## Run the function - if parentheses are left blank it will run with a preset mean = 6 and sd = 2

log_likelihood_visual(mean = , sd = )