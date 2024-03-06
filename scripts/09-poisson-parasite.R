library(performance)
library(pscl)
library(tidyverse)
library(broom)
library(fitdistrplus)
library(DHARMa)

## Data====
# Read the data from CSV file
parasite <- read_csv("data/parasite_exp.csv")

## Fit a linear regression model====
diplo_lm <- lm(Diplo_intensity ~ Treatment, data = parasite)


# Plot model diagnostics
plot(diplo_lm, which=2)
plot(diplo_lm, which=3)

## Univariate checks====
# Plot the distribution of Soleolifera variable
descdist(parasite$Diplo_intensity, boot = 500, discrete = T)

# Fit different distributions and compare them
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
fp <- fitdist(parasite$Diplo_intensity, "pois")
fn <- fitdist(parasite$Diplo_intensity, "norm")
fnb <- fitdist(parasite$Diplo_intensity, "nbinom")
plot.legend <- c("normal", "poisson", "negative binomial")
denscomp(list(fn, fp, fnb), legendtext = plot.legend)
qqcomp(list(fn, fp, fnb), legendtext = plot.legend)

## Model fitting====

# Fit a Poisson regression model
diplo_glm <- glm(Diplo_intensity ~ Treatment, data=parasite, family=poisson(link="log"))

# AIC comparison
AIC(diplo_lm, diplo_glm)

# Summarize the Poisson regression model
summary(diplo_glm)

#697/217 = mild overdispersion = 3

# Plot model diagnostics for Poisson model
plot(diplo_glm, which=2)
plot(diplo_glm, which=3)

## Scaled residuals are fine, slight skew in qqplot

# DHARMa check - looks ok - detects some overdispersion
plot(DHARMa::simulateResiduals(diplo_glm))


## Check for zeroinflation

100*sum(parasite$Diplo_intensity == 0)/nrow(parasite)
# Only 2% of our data has zeros

performance::check_zeroinflation(diplo_glm)
# We are "underpredicting zeros" but on a dataset with only 2% zeros,
# doesn't seem worth worrying about! 


## A quasi-likelihood model should be fine

diplo_quasiglm <- glm(Diplo_intensity ~ Treatment, data=parasite, family=quasipoisson(link="log"))


## Fitting a negative binomial - we probably dont need to do this but lets take a look!

stick_nb <- glm.nb(Diplo_intensity ~ Treatment, link = "log", data = parasite)


## Validate model


plot(simulateResiduals(stick_nb))



par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

plot(stick_nb)


## A comparison of models:

colors <- c("Quasi" = "cyan", "NegBin" = "darkorange")

means_quasi <- emmeans::emmeans(stick_quasi, 
                                specs = ~ Treatment, 
                                type='response') |> 
  as_tibble()

means_nb <- emmeans::emmeans(stick_nb, 
                             specs = ~ Treatment, 
                             type='response') |> 
  as_tibble()



ggplot(parasite, aes(x=Treatment, y=Diplo_intensity)) + 
  geom_jitter(width = .2,
              alpha = .4)+
  geom_pointrange(data = means_quasi,
                  aes(x = Treatment,
                      y = rate,
                      ymin = asymp.LCL, ymax = asymp.UCL,
                      colour = "Quasi"),
                  position = position_nudge(x= -.2))+
  geom_pointrange(data = means_nb,
                  aes(x = Treatment,
                      y = response,
                      ymin = asymp.LCL, ymax = asymp.UCL,
                      color = "NegBin"),
                  position = position_nudge(x= .2))+
 
  theme_classic(base_size = 14)+
  scale_color_manual(values = colors)
