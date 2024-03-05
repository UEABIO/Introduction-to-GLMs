# Example walkthrough setting up a binomial glm on weighted proportions ====
#
# 
# This small dataset is from an experiment looking at mortality of Tribolium beetles in batches of flour 
# exposed to different doses of pesticides.
# How does pesticide dose affect mortality?
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
beetles <- read_csv("../data/beetles.csv")

## Preprocess the data: rename columns and calculate 'alive' column====
beetles <- beetles |> 
  rename("dead" = Number_killed,
         "trials" = Number_tested) |> 
  mutate("alive" = trials-dead)

## Fit a binomial GLM model to the data====
beetle_glm <- glm(cbind(dead, alive) ~ Dose, family = binomial(link = "logit"), data = beetles)

# Summary of the binomial GLM model
summary(beetle_glm)

# Fit a binomial GLM model with weights
beetle_glm_weights <- glm(Mortality_rate ~ Dose, weights = trials, family = binomial(link = "logit"), data = beetles)

# Summary of the binomial GLM model with weights
summary(beetle_glm_weights)

## Diagnostics with stats functions====

# Calculate and plot "deviance" residuals

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

pred <- predict(beetle_glm_weights) 
dresid <- resid(beetle_glm_weights, type = "deviance")

# Check for overdispersion
hist(dresid)

# Can also check the residual deviance/residual df from summary() >1.5 = overdispersed
# If overdispersion detected overdispersion parameter can be estimated with quasilikelihood
# beetle_glm <- glm(cbind(dead, alive) ~ Dose, family = quasibinomial(link = "logit"), data = beetles)

# Binned residuals
arm::binnedplot(dresid, pred)

# Plot leverage
plot(beetle_glm_weights, which = 5)

plot(beetle_glm_weights, which = 2)

## Diagnostics with performance package====

performance::check_model(beetle_glm_weights)

## Diagnostics using DHARMa====
plot(simulateResiduals(beetle_glm_weights))




## Results====

# Generate confidence intervals using broom
broom::tidy(beetle_glm, conf.int = T)

# Display table with exponentiated coefficients

sjPlot::tab_model(beetle_glm)

# Predictions using emmeans
emmeans::emmeans(beetle_glm, 
        specs = ~ Dose, 
        at=list(Dose=c(40, 50, 60, 70, 80)), 
        type='response') 

# Predictions plot
means <- emmeans(beetle_glm, 
                 specs = ~ Dose, 
                 at=list(Dose=c(40:80)), 
                 type='response')  |> 
  as_tibble()

ggplot(beetles, aes(x=Dose, y=Mortality_rate)) + geom_point()+
  geom_ribbon(data = means,
              aes(x = Dose,
                  y = prob,
                  ymin = asymp.LCL,
                  ymax = asymp.UCL),
              alpha = .2)+
  geom_line(data = means,
            aes(x = Dose,
                y = prob)) +  # regression line
  labs(title = "Logit-link Binomial Regression",
       x = "Dose", y = "Mortality")+
  theme_classic(base_size = 14)
