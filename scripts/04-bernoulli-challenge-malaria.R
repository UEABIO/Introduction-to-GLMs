# Malaria dataset solutions on binary data ====
#
# Investigating the predictors of malarial infection in the Seychelles Warbler
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

malaria <- read_csv("data/malaria.csv")


## Basic Binary model ====

malaria_glm.1 <- glm(Malaria ~ Tarsus_length * Sex * Ageclass, family = binomial(link = "logit"), data = malaria)

# I do not recommend including interaction models without apriori reasons for thinking there will be an interaction effect
# If you do fit the above model, you will get a warning and that appears to be because there are no Males recorded in the fledgling class
# This is potentially important - but ideally something you would have discovered during exploratory data analysis


# I think it is ok to want to investigate if body size (as tarsus length) changes the probability of malarial infection
# as an interaction with age:
malaria_glm.2 <- glm(Malaria ~ Tarsus_length + Sex + Ageclass + Tarsus_length:Ageclass, family = binomial(link = "logit"), data = malaria)


##Check model====

# Calculate and plot "deviance" residuals
# Deviance > 2 might indicate poor model fit - There is no overdispersion in a Bernoulli/Binary model 
dresid <- resid(malaria_glm.2, type = "deviance")
hist(dresid)

## Two plots showing the same thing - outliers and fit to the model
# Plot leverage
plot(malaria_glm.2, which = 5)
# Plot Cook's distance
plot(malaria_glm.2, which = 4)

# Indicates one potential outlier - probably worth investigating

## Binned residuals plot

pred <- predict(malaria_glm.2)

arm::binnedplot(pred,dresid)


## Performance package model checks====
# Same as above but will auto-select the appropriate residual plots
# performance::check_model(malaria_glm.2)


## Simulated residuals check====

plot(DHARMa::simulateResiduals(malaria_glm.2))



## Removing the interaction term====

# I know my model has a good fit - but I want to see if removing the interaction term
# significantly affects the deviance of the model

drop1(malaria_glm.2, test = "Chi")

# It doesnt appear to - so I can drop this interaction term

malaria_glm.3 <- glm(Malaria ~ Tarsus_length + Sex + Ageclass, family = binomial(link = "logit"), data = malaria)

## Summary of the  model====
summary(malaria_glm.3)

# Pseudo r-squared
MuMIn::r.squaredLR(malaria_glm.3)

# Testing deviance of each predictor
drop1(malaria_glm.3, test = "Chi")
# I would conclude that only Age class appears to significantly change deviance

# Exponentiated terms with broom helper

broom::tidy(malaria_glm.3, 
            exponentiate = T, 
            conf.int = T)


## Predictions====


# Marginal means & "predictions" with emmeans

## Generating confidence intervals for marginal means is not straightforward with standard stats functions,
## emmeans is a very good helper for this process!

emmeans::emmeans(malaria_glm.3,  # model
                 specs = pairwise ~ Ageclass, # if a term is not included results will be the "average" response at mean of missing term
                 type='response') # specifies probabilities not logit-odds

# Using the estimates and the marginal means we can produce predictive probabilites of malarial infection
# At each age class for the average size and across both sexes
# We can see that Juveniles have the highest probability of malarial infection, followed by Fledglings
# Chicks have the lowest probability
# Posthoc pairiwise analysis lets up compare between each level of the factor if we wish to.


means <- emmeans::emmeans(malaria_glm.3,  # model
                 specs = ~ Ageclass, # if a term is not included results will be the "average" response at mean of missing term
                 type='response') |> 
  as_tibble()

malaria |> 
  mutate(Ageclass = factor(Ageclass, levels = c("Chick","Fledgling", "Juvenile", "Adult"))) |> 
  ggplot(aes(x = Ageclass, y = Malaria))+
#  geom_jitter()+
  geom_line(data = means, aes(x = Ageclass, y = prob, group =1))+
  geom_line(data = means, aes(x = Ageclass, y = asymp.UCL, group =1),
            linetype = "dashed")+
  geom_line(data = means, aes(x = Ageclass, y = asymp.LCL, group =1),
            linetype = "dashed") +
  theme_classic()
