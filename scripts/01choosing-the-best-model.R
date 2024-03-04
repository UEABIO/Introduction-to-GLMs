# Tutorial on Choosing the Best Linear Model====

## Load necessary libraries====
library(lmtest)
library(tidyverse)
library(fitdistrplus)
library(MuMIn)

## Load the dataset====
janka <- readr::read_csv("data/janka.csv")

## Basic linear regression====

# Linear regression model: hardness ~ dens
janka_ls <- lm(hardness ~ dens, data = janka)

# Summary of the linear regression model
summary(janka_ls)

# Diagnostic plots for the linear regression model
plot(janka_ls, which=c(2,3))

# Breusch-Pagan Test of Homoscedasticity
lmtest::bptest(janka_ls)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(janka_ls))



## Assessing transformations====
# Box-Cox transformation to assess possible transformations
MASS::boxcox(janka_ls)
## indicates a sqrt transformation has best likelihood


## Applying different transformations===
# Linear regression model with square root transformation: sqrt(hardness) ~ dens
janka_sqrt <- lm(sqrt(hardness) ~ dens, data = janka)
plot(janka_ls, which=c(2,3))
# Residual heterogeneity

# Linear regression model with logarithmic transformation: log(hardness) ~ dens
janka_log <- lm(log(hardness) ~ dens, data = janka)
plot(janka_log, which=c(2,3))
# Overcorrection

# Linear regression model with polynomial transformation: log(hardness) ~ poly(dens, 2)
janka_poly <- lm(log(hardness) ~ poly(dens, 2), data = janka)
plot(janka_poly, which=c(2,3))
# A good fit at the cost of a new predictor variable

# Weighted least squares regression model
janka_wls <- lm(sqrt(hardness) ~ dens, weights = 1/sqrt(hardness), data = janka)
plot(janka_wls, which=c(2,3))
# A good fit - relies on getting weighting correct

# All transformations change the relationship of variance and the mean at the same time - comparisons and relationships are difficult to interpret


## Univariate analysis of the dependent variable====


# Perform distribution fitting and comparison for hardness data
descdist(janka$hardness, boot = 500)

# Set up the plotting parameters for comparison plots
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
# Fit gamma distribution to square root transformed hardness data
fg <- fitdist(janka$hardness, "gamma")
# Fit normal distribution to square root transformed hardness data
fn <- fitdist(janka$hardness, "norm")
# Create legend for the distribution comparison plots
plot.legend <- c("normal", "gamma")
# Plot density comparison between normal and gamma distributions
denscomp(list(fn, fg), legendtext = plot.legend)
# Plot quantile-quantile comparison between normal and gamma distributions
qqcomp(list(fn, fg), legendtext = plot.legend)



## Generalised Models====

# Generalized linear model (GLM) with Gaussian family and square root link function
janka_glm <- glm(hardness ~ dens, data = janka, family = gaussian(link = "sqrt"))
plot(janka_glm, which=c(2,3))

# Generalized linear model (GLM) with Gamma family and square root link function
janka_gamma <- glm(hardness ~ dens, data = janka, family = Gamma(link = "sqrt"))
plot(janka_gamma, which=c(2,3))

# AIC comparisons indicate gamma model is a better fit along with analysis of residuals
AIC(janka_glm, janka_gamma)
# Note AIC cannot be used to compare models with dependent variable transformations


# R squared should be used with caution as calculations are calculated with deviance of model rather than variance explained
r.squaredLR(janka_glm)
r.squaredLR(janka_gamma)





## Coefficients of the model====

summary(janka_gamma)
