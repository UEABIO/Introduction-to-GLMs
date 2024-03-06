library(performance)
library(pscl)
library(tidyverse)
library(broom)
library(fitdistrplus)

## Data====
# Read the data from CSV file
reveg <- read_csv("data/Revegetation.csv")

## Fit a linear regression model====
reveg_lm <- lm(Soleolifera ~ Treatment, data = reveg)


# Plot model diagnostics
plot(reveg_lm , which=2)
plot(reveg_lm , which=3)

## Univariate checks====
# Plot the distribution of Soleolifera variable
descdist(reveg$Soleolifera, boot = 500, discrete = T)

# Fit different distributions and compare them
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
fp <- fitdist(reveg$Soleolifera, "pois")
fn <- fitdist(reveg$Soleolifera, "norm")
fnb <- fitdist(reveg$Soleolifera, "nbinom")
plot.legend <- c("normal", "poisson", "negative binomial")
denscomp(list(fn, fp, fnb), legendtext = plot.legend)
qqcomp(list(fn, fp, fnb), legendtext = plot.legend)

## Model fitting====

# Fit a Poisson regression model
reveg_glm <- glm(Soleolifera ~ Treatment, data=reveg, family=poisson(link="log"))


# Summarize the Poisson regression model
summary(reveg_glm)

#367/96 = mild overdispersion = 3.8

# Plot model diagnostics for Poisson model
plot(reveg_glm, which=2)
plot(reveg_glm, which=3)

## Residuals do not look good!

# DHARMa check - looks ok - detects some overdispersion
plot(DHARMa::simulateResiduals(reveg_glm))


## Check for zeroinflation

100*sum(reveg$Soleolifera== 0)/nrow(reveg)
# 36% zeros quite high!

performance::check_zeroinflation(reveg_glm)
# We are "underpredicting zeros" on a dataset with 36% zeros



reveg_zip <- zeroinfl(Soleolifera ~ Treatment| 
                         Treatment,
                       dist = "poisson",
                       link = "logit",
                       data = reveg)



sresid <- residuals(cuckoo_zip, type = "pearson")

pred <- predict(cuckoo_zip)




summary(cuckoo_zip)


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
