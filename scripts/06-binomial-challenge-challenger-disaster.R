# Challenger Disaster====
#
# 
# Can you fit a binomial model of o-ring failure with temperature?
# Can you predict the probability of total failure at 36 degrees Farhenheit
# This was the temperature on launch day
#
## Load necessary libraries====

library(tidyverse)
library(broom) # installed with tidyverse but needs separate loading
library(MuMIn)
library(arm)
library(DHARMa)
library(emmeans)
library(sjPlot)

## Read the CSV file====
challenger <- read_csv("data/challenger.csv")

## Preprocess the data: 

glimpse(challenger)

#oring_tot = total number of orings
#oring_dt = orings destroyed
# temp = temp in farenheit at launch
# flight = flight number

challenger <- challenger |> 
  mutate(oring_int = oring_tot - oring_dt) # create variable containing orings left intact

## Fit a binomial GLM model to the data====
challenger_glm <- glm(cbind(oring_dt, oring_int) ~ temp, family = binomial(link = "logit"), data = challenger)

# Summary of the binomial GLM model
summary(challenger_glm)


## Diagnostics with stats functions====

# Calculate and plot "deviance" residuals

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

pred <- predict(challenger_glm) 
dresid <- resid(challenger_glm, type = "deviance")

# Check for overdispersion
hist(dresid)

# Can also check the residual deviance/residual df from summary() >1.5 = overdispersed
# If overdispersion detected overdispersion parameter can be estimated with quasilikelihood

# No evidence of overdispersion

# Binned residuals
arm::binnedplot(pred, dresid)

# Plot leverage
plot(challenger_glm, which = 5)

plot(challenger_glm, which = 2)

## Diagnostics with performance package====

performance::check_model(challenger_glm)

## Diagnostics using DHARMa====
plot(simulateResiduals(challenger_glm))

# See the help page for DHARMa - minor deviations in quantiles from predicted,
# but a formal test finds this to be n.s.
# testQuantiles(challenger_glm)


## Results====

# Generate confidence intervals using broom
broom::tidy(challenger_glm, conf.int = T)

# Display table with exponentiated coefficients

sjPlot::tab_model(challenger_glm)


## Predictions ===

new_temp<- data.frame(temp = 36)
predict(challenger_glm, newdata = new_temp, type = "response")

# Predictions using emmeans
emmeans::emmeans(challenger_glm, 
        specs = ~ temp, 
        at=list(temp=c(36:90)), 
        type='response') 

# Predictions plot
means <- emmeans::emmeans(challenger_glm, 
                          specs = ~ temp, 
                          at=list(temp=c(36:90)), 
                          type='response')   |> 
  as_tibble()

ggplot(means, aes(x=temp, y=prob))+
  geom_ribbon(aes(ymin = asymp.LCL,
                  ymax = asymp.UCL),
              alpha = .2)+
  geom_line() +  # regression line
  labs(title = "Logit-link Binomial Regression",
       x = "Temp ", y = "Probability of total o-ring failure")+
  theme_classic(base_size = 14)
