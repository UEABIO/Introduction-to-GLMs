
# In a study by Kilner et al. (1999), the authors studied the begging rate of nestlings 
# in relation to total mass of the brood of reed warbler chicks and cuckoo chicks.
# The question of interest is: How does nestling mass affect begging rates between the different species?

## Packages====
library(tidyverse)
library(broom)
library(fitdistrplus)

## Data====

# Read the cuckoo data from CSV file
monarchs <- read_csv("data/monarchs.csv")


## Fit a linear regression model====
monarchs_lm <- lm(count ~ garden, data = monarchs)

# Plot the regressions
broom::augment(monarchs_lm, type.predict = "response") %>% 
  ggplot(aes(x=garden, y=.fitted)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()

# Plot model diagnostics
plot(monarchs_lm, which=2)
plot(monarchs_lm, which=3)

## The logic and model diagnostics show this in not appropriate
# We have discrete(non-continous count data)

## Univariate checks====
# Plot the distribution of count variable
descdist(monarchs$count, boot = 500, discrete = T)

# Fit different distributions and compare them
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
fp <- fitdist(monarchs$count, "pois")
fn <- fitdist(monarchs$count, "norm")
fnb<- fitdist(monarchs$count, "nbinom")
plot.legend <- c("normal", "poisson", "negative binomial")
denscomp(list(fn, fp, fnb), legendtext = plot.legend)
qqcomp(list(fn, fp, fnb), legendtext = plot.legend)

## Poisson looks ok, but so does the normal distribution
# but we have to remember that we KNOW this is count data
# we also know that this is the full dependent variable and this
# could change a lot once we have fitted a linear model and are looking at the 
# residuals

## Model fitting====

# Fit a Poisson regression model
monarchs_glm1 <- glm(count ~ garden, data=monarchs, family=poisson(link="log"))


#AIC

AIC(monarchs_lm, monarchs_glm1)
# Poisson is clearly a better fit

# Summarize the Poisson regression model
summary(monarchs_glm1)

# Plot model diagnostics for Poisson model
plot(monarchs_glm1, which=2)
plot(monarchs_glm1, which=3)

## Scaled residuals are fine, slight skew in qqplot

# DHARMa check - looks good
plot(DHARMa::simulateResiduals(monarchs_glm1))

# No evidence of over-dispersion

drop1(monarchs_glm1, test = "Chisq")

# Figures

means <- emmeans(monarchs_glm1, 
                 specs =pairwise ~ garden,
                 type='response') 
means

ggplot(means, aes(x=garden, y=rate))+
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL))+
    geom_jitter(data = monarchs,
                aes(x = garden,
                    y = count),
                width = .2,
                alpha = .2)+
  theme_classic(base_size = 14)

