## -----------------------------------------------------------------------------------------------------------------------------------------
#| include: false
#| message: false
#| warning: false

library(tidyverse)
library(MuMIn)
library(arm)
library(equatiomatic)
library(MASS)
library(pscl)
library(DHARMa)
library(fitdistrplus)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
## covid <- readr::read_csv(
##   "https://raw.githubusercontent.com/nrennie/f4sg-gams/main/data/covid.csv"
##   )


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| layout-ncol: 2
#| message: false

# Define a range of mean and standard deviations (average rate of success)
mean <- rep(c(10,20,30), 3)
sd <- rep(c(5, 7, 10), each = 3)

# Generate the data
norm_data <- map2_df(mean, sd, ~tibble(
  mean = factor(.x),
  sd = factor(.y),
  x = seq(0,40, length.out = 100),
  density = dnorm(x, .x, .y)
))  # For ordered plotting

# Plot
norm_data |> 
  filter(mean == 20) |> 
ggplot(aes(x = x, y = density, color = sd)) +
  geom_line(linewidth = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Change in Normal Distribution \nwith different Standard Deviations, Mean = 20",
       x = "X",
       y = "Probability",
       color = "SD") +
  theme_minimal(base_size = 14)

norm_data |> 
  filter(sd == 7) |> 
ggplot(aes(x = x, y = density, color = mean)) +
  geom_line(linewidth = 1.5) +
  scale_color_brewer(palette = "Accent") +
  labs(title = "Change in Normal Distribution \nwith different Means, SD = 7",
       x = "X",
       y = "Probability",
       color = "Mean") +
  theme_minimal(base_size = 14)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
#| message: false

## car::vif(mod)
## lmtest::bptest(mod)
## 
## sresid <- residuals(janka_gamma, type = "pearson")
## bptest(janka_gamma, ~ sresid)
## 
## 
## shapiro.test(residuals(model))
## 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

janka <- readr::read_csv("../data/janka.csv")



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

janka_ls <- lm(hardness ~ dens, data = janka)

summary(janka_ls)


janka |> 
  ggplot(aes( x = dens, y = hardness))+
  geom_point()+
  geom_smooth(method = "lm")



## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ex-lm-timer
countdown::countdown(
  minutes = 10,
  color_border = "#00AEEF",
  color_text = "#00AEEF",
  color_running_text = "white",
  color_running_background = "#00AEEF",
  color_finished_text = "#00AEEF",
  color_finished_background = "white",
  top = 0,
  margin = "1.2em",
  font_size = "2em"
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

plot(janka_ls, which=2)

plot(janka_ls, which=3)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false

library(lmtest)

# Breusch-Pagan Test of Homoscedasticity
lmtest::bptest(janka_ls)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(janka_ls))





## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ex-transform-lm-timer
countdown::countdown(
  minutes = 15,
  color_border = "#00AEEF",
  color_text = "#00AEEF",
  color_running_text = "white",
  color_running_background = "#00AEEF",
  color_finished_text = "#00AEEF",
  color_finished_background = "white",
  top = 0,
  margin = "1.2em",
  font_size = "2em"
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

janka_sqrt <- lm(sqrt(hardness) ~ dens, data = janka)

plot(janka_sqrt, which=2)

plot(janka_sqrt, which=3)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

ggplot(janka, aes(x = hardness, y = dens)) +
  geom_point() +  # scatter plot of original data points
  geom_smooth(method = "lm", formula = y ~ sqrt(x)) +  # regression line
  labs(title = "Sqrt Linear Regression with ggplot2",
       x = "X", y = "Y")  # axis labels and title



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

janka_log <- lm(log(hardness) ~ dens, data = janka)

plot(janka_log, which=2)

plot(janka_log, which=3)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

janka_poly <- lm(log(hardness) ~ poly(dens, 2), data = janka)

plot(janka_poly, which=2)

plot(janka_poly, which=3)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false


summary(janka_poly)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

ggplot(janka, aes(x = hardness, y = dens)) +
  geom_point() +  # scatter plot of original data points
  geom_smooth(method = "lm", formula = (y ~ log(x))) +  # regression line
  labs(title = "Log Linear Regression",
       x = "X", y = "Y")  # axis labels and title

ggplot(janka, aes(x = hardness, y = dens)) +
  geom_point() +  # scatter plot of original data points
  geom_smooth(method = "lm", formula = (y ~ poly(log(x), 2))) +  # regression line
  labs(title = "Quadratic Log Linear Regression",
       x = "X", y = "Y")  # axis labels and title





## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

janka_wls <- lm(sqrt(hardness) ~ dens, weights = 1/sqrt(hardness), data = janka)

plot(janka_wls, which=2)

plot(janka_wls, which=3)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false

prediction_data <- data.frame(dens = sort(unique(janka$dens)))
predictions <- predict(janka_wls, newdata = prediction_data, interval = "confidence", level = 0.95)

# Adding predictions and confidence intervals to the dataframe
prediction_data$wls_pred = predictions[, "fit"]
prediction_data$wls_pred.lwr = predictions[, "lwr"]
prediction_data$wls_pred.upr = predictions[, "upr"]

ggplot(janka) +
     geom_ribbon(data = prediction_data, aes(x = dens, ymin = wls_pred.lwr, ymax = wls_pred.upr), alpha = 0.8, fill = "lightgray")+
    geom_line(data = prediction_data, aes(x = dens, y = wls_pred), color = "blue")+
  geom_point(aes(x = dens, y = sqrt(hardness)))



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false


janka_glm <- glm(hardness ~ dens, data = janka, family = gaussian(link = "sqrt"))

summary(janka_glm)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false


ggplot(janka, aes(x = dens, y = hardness)) +
  geom_point() +  # scatter plot of original data points
  geom_smooth(method = "glm", method.args = list(gaussian(link = "sqrt"))) +  # regression line
  labs(title = "Linear Regression with ggplot2",
       x = "X", y = "Y")  # axis labels and title




## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| layout-ncol: 2
#| fig-height: 8
#| message: false

janka_glm <- glm(hardness ~ dens, data = janka, family = gaussian(link = "sqrt"))

plot(janka_glm, which=2)

plot(janka_glm, which=3)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

library(fitdistrplus)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
fg <- fitdist(sqrt(janka$hardness), "gamma")
fn <- fitdist(sqrt(janka$hardness), "norm")
plot.legend <- c("normal", "gamma")
denscomp(list(fn, fg), legendtext = plot.legend)
qqcomp(list(fn, fg), legendtext = plot.legend)
cdfcomp(list(fn, fg), legendtext = plot.legend)
ppcomp(list(fn, fg), legendtext = plot.legend)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false


janka_gamma <- glm(hardness ~ dens, data = janka, family = Gamma(link = "sqrt"))

summary(janka_gamma)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false


ggplot(janka, aes(x = dens, y = hardness)) +
  geom_point() +  # scatter plot of original data points
  geom_smooth(method = "glm", method.args = list(Gamma(link = "sqrt"))) +  # regression line
  labs(title = "Linear Regression with ggplot2",
       x = "X", y = "Y")  # axis labels and title




## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| message: false



library(MuMIn)

#r.squaredLR()

# r squared



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false
#| warning: false

# Simulated data
set.seed(42)

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


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false
#| warning: false

load(file = "../data/Mayflies.rda")



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| layout-ncol: 2
#| message: false
#| warning: false

ggplot(Mayflies, aes(x=CCU, y=Occupancy)) + geom_point()+
  geom_smooth(method = "lm")

ggplot(Mayflies, aes(x=CCU, y=Occupancy)) + geom_point()+
  geom_smooth(method = "glm", method.args = list(binomial(link = "logit"))) +  # regression line
  labs(title = "Logit-link Binomial Regression",
       x = "X", y = "Y")  # axis labels and title



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false
#| warning: false

# Define probabilities
p_success <- 0.7
p_failure <- 1 - p_success

# Create data frame
df <- data.frame(
  Outcome = c("Success", "Failure"),
  Probability = c(p_success, p_failure)
)

# Plot
ggplot(df, aes(x = Outcome, y = Probability, group = Outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Probability Distribution of a Bernoulli Random Variable",
       x = "Outcome",
       y = "Probability") +
  theme_minimal()



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| layout-ncol: 3
#| message: false
#| warning: false

# Generate a sequence of independent variable values
independent_variable <- seq(-10, 10, by = 0.5)

# Define a function to calculate probabilities
calculate_probability <- function(x) {
  probability <- 1 / (1 + exp(-x))
  return(probability)
}

# Define a function to calculate odds
calculate_odds <- function(probability) {
  odds <- probability / (1 - probability)
  return(odds)
}

# Define a function to calculate log odds
calculate_log_odds <- function(odds) {
  log_odds <- log(odds)
  return(log_odds)
}

# Calculate probabilities, odds, and log odds
probabilities <- sapply(independent_variable, calculate_probability)
odds <- sapply(probabilities, calculate_odds)
log_odds <- sapply(odds, calculate_log_odds)

# Plot the relationships
par(mfrow = c(1, 3), mar = c(5, 5, 2, 2))
plot(independent_variable, probabilities, type = "l", col = "blue", xlab = "Independent Variable", ylab = "Probability", main = "Change in Probability", ylim = c(0, 1))
plot(independent_variable, odds, type = "l", col = "red", xlab = "Independent Variable", ylab = "Odds", main = "Change in Odds")
plot(independent_variable, log_odds, type = "l", col = "green", xlab = "Independent Variable", ylab = "Log Odds", main = "Change in Log Odds")




## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| message: false
#| warning: false

mayfly_glm <- glm(Occupancy ~ CCU, family = binomial(link = "logit"), data = Mayflies)

summary(mayfly_glm)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ex-odds-timer
countdown::countdown(
  minutes = 10,
  color_border = "#00AEEF",
  color_text = "#00AEEF",
  color_running_text = "white",
  color_running_background = "#00AEEF",
  color_finished_text = "#00AEEF",
  color_finished_background = "white",
  top = 0,
  margin = "1.2em",
  font_size = "2em"
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| layout-ncol: 3
#| message: false

dresid <- resid(mayfly_glm, type = "deviance")
hist(dresid)

plot(mayfly_glm, which = 2)

plot(mayfly_glm, which = 4)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

plot(mayfly_glm, which = 1)



## -----------------------------------------------------------------------------------------------------------------------------------------
library(DHARMa)
plot(simulateResiduals(mayfly_glm))



## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ex-malaria-timer
countdown::countdown(
  minutes = 30,
  color_border = "#00AEEF",
  color_text = "#00AEEF",
  color_running_text = "white",
  color_running_background = "#00AEEF",
  color_finished_text = "#00AEEF",
  color_finished_background = "white",
  top = 0,
  margin = "1.2em",
  font_size = "2em"
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

# Define a range of trials
trial_sizes <- c(5, 10, 20, 50, 100)

# Define the probability of success
p_success <- 0.7

# Generate the binomial distribution data
binom_data <- map_df(trial_sizes, ~tibble(
  trials = .x,
  success = 0:.x,
  probability = dbinom(0:.x, .x, p_success)
)) %>%
  mutate(trials = factor(trials, levels = trial_sizes)) # This ensures the plots are ordered correctly

# Plot
ggplot(binom_data, aes(x = success, y = probability, color = trials)) +
  geom_line() + # Use geom_point() if you prefer dots
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Change in Binomial Distribution with Number of Trials",
       x = "Number of Successes",
       y = "Probability",
       color = "Number of Trials") +
  theme_minimal()



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: true
#| fig-height: 8
#| message: false

# According to the situation:
# n = 5,
# s = 5,
# p = 0.95,
# (1 — p) = 0.05

p = 1 * (0.95)^(5) * (0.05)^(0)

p



## -----------------------------------------------------------------------------------------------------------------------------------------

dbinom(5, 5, 0.95)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

# Define a range of lambda values (average rate of success)
lambda_values <- c(1, 4, 10, 20, 50)

# Generate the Poisson distribution data
poisson_data <- map_df(lambda_values, ~tibble(
  lambda = .x,
  events = 0:75, # Assuming a reasonable range for visualization
  probability = dpois(0:75, .x)
)) |> 
  mutate(lambda = factor(lambda, levels = lambda_values)) # For ordered plotting

# Plot
ggplot(poisson_data, aes(x = events, y = probability, color = lambda)) +
  geom_line(linewidth = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Change in Poisson Distribution with Different Lambda",
       x = "Number of Events",
       y = "Probability",
       color = "Lambda") +
  theme_minimal(base_size = 14)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
#| fig-height: 8
#| message: false

## cuckoo_lm <- lm(Beg ~ Mass + Species + Mass:Species, data = cuckoo)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| include: FALSE

cuckoo <- read_csv("../data/cuckoo.csv")



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 7
#| message: false

cuckoo_lm <- lm(Beg ~ Mass + Species + Mass:Species, data = cuckoo)

broom::augment(cuckoo_lm, type.predict = "response") %>% 
ggplot(aes(x=Mass, y=.fitted, colour=Species)) + 
  geom_point() +
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values=c("green3","turquoise3"))+
  scale_x_continuous(limits = c(0,40)) +
  theme_minimal()





## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| layout-ncol: 2
#| fig-height: 8

plot(cuckoo_lm, which=2)

plot(cuckoo_lm, which=3)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true

cuckoo_glm1 <- glm(Beg ~ Mass + Species + Mass:Species, data=cuckoo, family=poisson(link="log"))

summary(cuckoo_glm1)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ex-pois-timer
countdown::countdown(
  minutes = 10,
  color_border = "#00AEEF",
  color_text = "#00AEEF",
  color_running_text = "white",
  color_running_background = "#00AEEF",
  color_finished_text = "#00AEEF",
  color_finished_background = "white",
  top = 0,
  margin = "1.2em",
  font_size = "2em"
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| layout-ncol: 2
#| fig-height: 8

plot(cuckoo_glm1, which=2)

plot(cuckoo_glm1, which=3)


## -----------------------------------------------------------------------------------------------------------------------------------------

cuckoo_glm2 <- glm(Call_Total ~ Mass + Species + Mass:Species, data=cuckoo, offset = log(Mins), family=poisson(link="log"))



## -----------------------------------------------------------------------------------------------------------------------------------------



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
#|
## install.packages("pscl")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true
#|
## zip1 <- zeroinf(y ~ x1 + x2 | x1 + x2,
##                 dist = "poisson",
##                 link = "logit",
##                 data = dataframe)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: true

## # zeros modelled with a constant
## y ~ x1 + x2
## 
## # zeros modelled with the same variables
## y ~ x1 + x2 | x1 + x2
## 
## # zeros modelled with different variables
## y ~ x1 + x2 | z1 +z2
## 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

##Negative Binomial Distribution (Varying Shape Parameter r)
#In the negative binomial distribution:
  
#  Shape Parameter (r): Represents the number of successes required before the experiment is stopped.
#Probability of Success (p): Represents the probability of success in each Bernoulli trial.
#As r increases:
  
#  The distribution becomes more skewed to the right.
#The average number of failures before success increases.
#The variance of the distribution also increases, indicating greater variability.

# Function to generate negative binomial distribution data
generate_nbinom_data <- function(r, p) {
  tibble(failures = 0:(10*r), 
         probability = dnbinom(0:(10*r), size = r, prob = p))
}

# Define range of shape parameter values (r)
r_values <- rep(c(2, 5, 10), times = 3)  # Number of successes

# Fixed probability of success
p_fixed <- rep(c(0.2, 0.4, 0.6), each = 3)  

# Generate negative binomial distribution data for different r values
nbinom_data <- map2_df(r_values, p_fixed, ~generate_nbinom_data(r = .x, p = .y) 
                      %>% mutate(r = as.factor(.x)) %>% mutate(p = as.factor(.y))) 

# Plot for negative binomial distribution
ggplot(nbinom_data, aes(x = failures, y = probability, color = r, group = interaction(r,p))) +
  geom_line() +
  scale_color_brewer(palette = "Dark2")+
  labs(title = "Negative Binomial Distribution with Varying Shape Parameter (r)",
       x = "Number of Failures",
       y = "Probability",
       color = "Number of Successes (r)") +
  theme_minimal()+
  facet_wrap(~p)+
  gghighlight::gghighlight()



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| echo: false
#| fig-height: 8
#| message: false

# Define a range of shape values
shape_values <- c(1, 2, 5, 10)
scale_value <- 2 # Keep scale fixed for simplicity

# Generate the Gamma distribution data
gamma_data <- map_df(shape_values, ~tibble(
  shape = .x,
  x = seq(0, 20, length.out = 100),
  density = dgamma(seq(0, 20, length.out = 100), shape = .x, scale = scale_value)
)) %>%
  mutate(shape = factor(shape, levels = shape_values)) # For ordered plotting

# Plot
ggplot(gamma_data, aes(x = x, y = density, color = shape)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Change in Gamma Distribution with Different Shape Parameters",
       x = "Value",
       y = "Density",
       color = "Shape Parameter") +
  theme_minimal()



## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| echo: false

## model3 <- survreg((Surv(`Hours`, event) ~ source * supplementation), data = data2, dist = "weibull")
## lin.pred <- predict(model3, type = "lp")[data2$event ==1]
## log.resid <- log(data2$`Hours`[data2$event==1]) - lin.pred
## car::qqPlot(exp(log.resid), dist = "weibull", shape = 1/model3$scale)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ex-4-timer
countdown::countdown(
  minutes = 10,
  color_border = "#b20e10",
  color_text = "#b20e10",
  color_running_text = "white",
  color_running_background = "#b20e10",
  color_finished_text = "#b20e10",
  color_finished_background = "white",
  top = 0,
  margin = "1.2em",
  font_size = "2em"
)

