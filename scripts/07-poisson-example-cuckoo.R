
# In a study by Kilner et al. (1999), the authors studied the begging rate of nestlings 
# in relation to total mass of the brood of reed warbler chicks and cuckoo chicks.
# The question of interest is: How does nestling mass affect begging rates between the different species?

## Packages====
library(tidyverse)
library(broom)
library(fitdistrplus)

## Data====

# Read the cuckoo data from CSV file
cuckoo <- read_csv("data/cuckoo.csv")

## Fit a linear regression model====
cuckoo_lm <- lm(Beg ~ Mass + Species + Mass:Species, data = cuckoo)

# Plot the regression line
broom::augment(cuckoo_lm, type.predict = "response") %>% 
  ggplot(aes(x=Mass, y=.fitted, colour=Species)) + 
  geom_point() +
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_colour_manual(values=c("green3","turquoise3"))+
  scale_x_continuous(limits = c(0,40)) +
  theme_minimal()

# Plot model diagnostics
plot(cuckoo_lm, which=2)
plot(cuckoo_lm, which=3)

## The logic and model diagnostics show this in not appropriate
# We have discrete(non-continous count data)

## Univariate checks====
# Plot the distribution of Beg variable
descdist(cuckoo$Beg, boot = 500, discrete = T)

# Fit different distributions and compare them
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
fp <- fitdist(cuckoo$Beg, "pois")
fn <- fitdist(cuckoo$Beg, "norm")
fnb<- fitdist(cuckoo$Beg, "nbinom")
plot.legend <- c("normal", "poisson", "negative binomial")
denscomp(list(fn, fp, fnb), legendtext = plot.legend)
qqcomp(list(fn, fp, fnb), legendtext = plot.legend)

## The Negative Binomial looks better than Poisson - but for now let's see what we get with a Poisson model

## Model fitting====

# Fit a Poisson regression model
cuckoo_glm1 <- glm(Beg ~ Mass + Species + Mass:Species, data=cuckoo, family=poisson(link="log"))

# Summarize the Poisson regression model
summary(cuckoo_glm1)

# Plot model diagnostics for Poisson model
plot(cuckoo_glm1, which=2)
plot(cuckoo_glm1, which=3)

## QQplot is ok - is there funneling in the scaled-residuals plot?

# DHARMa check - also indicates the error distribution is not quite right
plot(DHARMa::simulateResiduals(cuckoo_glm1))

# We will visit this later

## We haven't checked for multicollinearity yet!
car::vif(cuckoo_glm1)
## Multicollinearity can make hypothesis testing difficult - it is common when we include interaction terms
## but we can remove this by mean centering our continuous predictor variables

cuckoo$mass.c <- cuckoo$Mass - mean(cuckoo$Mass, na.rm =T)

cuckoo_glm2 <- glm(Beg ~ mass.c + Species + mass.c:Species, data=cuckoo, family=poisson(link="log"))

car::vif(cuckoo_glm2)
## This is now gone! Should be good standard practise with interactions in order to be able to detect 
## TRUE multicollinearity

## LRT or interaction term

drop1(cuckoo_glm2, test = "Chisq")


# Offsetting====

# In Poisson GLMs, 
# using an offset allows us to model rates rather than counts, 
# by including the logarithm of the exposure variable as an offset, 
# ensuring that the model accounts for the differing exposure levels across observations.

cuckoo_glm3 <- glm(Call_Total ~ mass.c + Species + mass.c:Species, data=cuckoo, offset = log(Mins), family=poisson(link="log"))

summary(cuckoo_glm3)

# Overdispersion====
## Going back to our count model
# We suspect there is more going on in our residuals than can be accounted for by a Poisson model,
# but for now we can definitely tell we have overdispersion

summary(cuckoo_glm2)
#Residual deviance / Residual df
#436.05/47 = 9.28

## This would count as minor overdispersion and could be dealt with
# using a quasilikelihood overdispersion term

cuckoo_glm_quasi <- glm(Beg ~ mass.c + Species + mass.c:Species, data=cuckoo, family=quasipoisson(link="log"))

# We should use an F statistic for the LR to account for the extra variance
drop1(cuckoo_glm2, test = "F")
# We would now conclude the interaction term is NOT significant

## See next exercises for dealing with this data through zero-inflation
