# Example walkthrough setting up a binomial glm on binary data ====
#
# Suppose we are interested in the abundance of mayflies in a stream. 
# Because mayflies are sensitive to metal pollution, 
# I might be interested in looking at the presence/absence of mayflies in a stream relative to a pollution gradient. 
# Here the pollution gradient is measured in Cumulative Criterion Units 
# (CCU: CCU is defined as the ratio of the measured metal concentration to the hardness adjusted chronic criterion concentration, and then summed across each metal) 
# where larger values imply more metal pollution.
#
#
## Load necessary libraries====

library(tidyverse)
library(broom) # installed with tidyverse but needs separate loading
library(MuMIn)
library(arm)
library(DHARMa)
library(emmeans)

## Load the dataset====
# Data is stored in R compatible .rda binary format
load("data/Mayflies.rda")
data(Mayflies)

## Basic Binary model ====

mayfly_glm <- glm(Occupancy ~ CCU, family = binomial(link = "logit"), data = Mayflies)

##Check model====

# Calculate and plot "deviance" residuals
# Deviance > 2 might indicate poor model fit - There is no overdispersion in a Bernoulli/Binary model 
dresid <- resid(mayfly_glm, type = "deviance")
hist(dresid)

## Two plots showing the same thing - outliers and fit to the model
# Plot leverage
plot(mayfly_glm, which = 5)
# Plot Cook's distance
plot(mayfly_glm, which = 4)


## Binned residuals plot

pred <- predict(mayfly_glm)

arm::binnedplot(pred,dresid)


## Performance package model checks====
# Same as above but will auto-select the appropriate residual plots
# performance::check_model(mayfly_glm)


## Simulated residuals check====

plot(DHARMa::simulateResiduals(mayfly_glm))


## Summary of the  model====
summary(mayfly_glm)

# Pseudo r-squared
MuMIn::r.squaredLR(mayfly_glm)


## Likelihood ratio tests - useful when determining whether to keep or retain interaction terms
## Or for reporting main effects

mayfly_null <- glm(Occupancy ~ 1, family = binomial(link = "logit"), data = Mayflies)


# Changes in 2*deviance for a Binomial error model should follow a chi-square distribution
anova(mayfly_glm, mayfly_null, test = "Chi")

drop1(mayfly_glm, test = "Chi")

# Exponentiated terms with broom helper

broom::tidy(mayfly_glm, 
            exponentiate = T, 
            conf.int = T)


## Predictions====

# Predictions for existing data
predict(mayfly_glm, type = "response")

# Predictions for new data
new_CCU<- data.frame(CCU = c(0,1,2,3,4,5))
new_pred <- predict(mayfly_glm, newdata = new_CCU, type = "response")

new_pred

# Generate a new dataframe if required
new_CCU$new_pred <- new_pred

# Marginal means & "predictions" with emmeans

## Generating confidence intervals for marginal means is not straightforward with standard stats functions,
## emmeans is a very good helper for this process!

emmeans::emmeans(mayfly_glm,  # model
                 specs = ~ CCU, # if a term is not included results will be the "average" response at mean of missing term
                 at=list(CCU=c(0:5)), # if specific predictions/means are required
                 type='response') # specifies probabilities not logit-odds


