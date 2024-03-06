# Tutorial on Choosing the Best Linear Model====

## Load necessary libraries====
library(lmtest)
library(tidyverse)
library(fitdistrplus)
library(MuMIn)
library(gtsummary)
library(emmeans)

## Load the dataset====
fruitfly <- readr::read_csv("data/fruitfly.csv")


## Data exploration====

# I will assume basic data exploration through visualisation has been carried out first


## Basic linear regression====

# Linear regression model: 
fly_lm <- lm(longevity ~ type + thorax + sleep, data = fruitfly)

# Diagnostic plots for the linear regression model
plot(fly_lm)
## Residuals appear normally distributed, no evidence of outliers, there is mild heterogeneity

# Breusch-Pagan Test of Homoscedasticity
lmtest::bptest(fly_lm)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(fly_lm))

## Both of the formal tests indicate heterogeneity is mild and residuals are normally distributed


## Assessing transformations====
# Box-Cox transformation to assess possible transformations
MASS::boxcox(fly_lm)
## indicates a sqrt transformation has best likelihood


## Univariate analysis of the dependent variable====


# Perform distribution fitting and comparison for hardness data
descdist(fruitfly$longevity, boot = 500)

# Set up the plotting parameters for comparison plots
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
# Fit gamma distribution to square root transformed hardness data
fg <- fitdist(fruitfly$longevity, "gamma")
# Fit normal distribution to square root transformed hardness data
fn <- fitdist(fruitfly$longevity, "norm")
# Create legend for the distribution comparison plots
plot.legend <- c("normal", "gamma")
# Plot density comparison between normal and gamma distributions
denscomp(list(fn, fg), legendtext = plot.legend)
# Plot quantile-quantile comparison between normal and gamma distributions
qqcomp(list(fn, fg), legendtext = plot.legend)

## Generalised Linear Model

fly_glm <- glm(longevity ~ type + thorax + sleep, family = gaussian(link = "identity"), data = fruitfly)

fly_sqrtglm <- glm(longevity ~ type + thorax + sleep, family = gaussian(link = "sqrt"), data = fruitfly)

# Indicates very little benefit to likelihood of the square root transformation (slight detriment)
AIC(fly_glm, fly_sqrtglm)


## Gamma GLM

# There is mild heterogeneity - though univariate analysis indicates gamma may not be needed

fly_gamma <- glm(longevity ~ type + thorax + sleep, family = Gamma(link = "identity"), data = fruitfly)

fly_sqrtgamma <- glm(longevity ~ type + thorax + sleep, family = Gamma(link = "sqrt"), data = fruitfly)

AIC(fly_gamma, fly_sqrtgamma)

# The gamma model actually has a slightly better AIC and has definitely reduced any heterogeneity

# R squared should be used with caution as calculations are calculated with deviance of model rather than variance explained
r.squaredLR(fly_glm)
r.squaredLR(fly_gamma)


# Conclusion - in my opinion either of the Gamma or Gaussian GLM with an identity link could be used here.
#Changes to estimates, SE and any hypothesis testing is negligible

## Coefficients of the model====

summary(fly_glm)
summary(fly_gamma)

## Write up for the Gamma model====

# Full model summary table
tbl_regression(fly_gamma)

# Hypothesis testing of main effects
drop1(fly_gamma, test = "F")

# Mean estimates and posthoc comparisons

emmeans::emmeans(fly_gamma, 
                 specs = pairwise ~ type + thorax + sleep,
                 type = "response")

# Capture max min range of thorax
fruitfly |> summarise(max = max(thorax), min = min(thorax))

# Co-erce emmeans to a tibble  - provide range of required predictions for thorax 
means <- emmeans::emmeans(fly_gamma, 
                 specs = ~ type + thorax,
                 at = list(thorax=c(0.64,0.94)),
                 type = "response") |> 
  as_tibble()

# set figure colours
colours <- c("cyan", "darkorange", "purple")

# Overly model predictions with observed values===
fruitfly |> 
  ggplot(aes(x=thorax, y = longevity, colour = type, fill = type))+
  geom_ribbon(data = means,
              aes(x = thorax, 
                  y = emmean,
                  ymin=lower.CL, 
                  ymax=upper.CL), alpha = 0.2)+
  geom_line(data = means,
            aes(linetype = "dashed",
                x = thorax,
                y = emmean),
                show.legend = FALSE)+
  geom_point(data = fruitfly, aes(x = thorax, y = longevity),
             show.legend = FALSE)+
  scale_colour_manual(values = colours)+
  scale_fill_manual(values = colours)+
  labs(y = "Lifespan in days",
       x = "Thorax length (mm)",
       fill = "Type of female exposure")+
  guides(colour = "none")+
  theme_classic()
  