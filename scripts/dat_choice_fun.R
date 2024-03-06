###################################
#                             
#             Testing General(ized) Linear Mixed Models of preference
#
#
#                                 Adapted from: 
#       Ariel Muldoon's "Simulate! Simulate! - Part 2: A linear mixed model"
#               See link below for equations and mathematical notation: 
#           https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/
#
#
#                     Simpler linear model example here:
#         https://aosmith.rbind.io/2018/01/09/simulate-simulate-part1/
#
#
#                             Other useful ones:
#             https://aosmith.rbind.io/2019/03/06/lots-of-zeros/
#     https://aosmith.rbind.io/2019/07/22/automate-model-fitting-with-loops/
#
#
#                     Other similar resources compiled here:
#             https://threadreaderapp.com/thread/1378030883043745792.html
# 
###################################



###################################
#
#             Simulating the data
# 
#             Two fixed effects: 
#           "treatment" (discrete) 
#       
#   
###################################


# Packages needed for this script:
library(tidyverse)
library(car)
library(MuMIn)
library(lme4)
#
#

# The "truth"
# I've tried to design this with the real biology in mind.
# You would want to tailor this to reflect the real/true biology that you 
# anticipate giving rise to your measured response variable. 

# # parameter guide 
# # (you can modify these as arguments in subsequent runs of the function "dat_choice_fun()")

# # function defaults
# ngroup = 2 # treatment; two groups
# nplate = 20 # 20 levels in this random effect
# totflies = 10 # total number of flies in each plate
# obs = 10 # 10 observations in each level of [whatever; each plate in this case]
# prob = 0.6 # a 10% greater probability of being on food "A" (in "group1") relative to the null of 0.5 in group2
# sigma = 0.1 # magnitude of random noise in "prob" among plates; could build off this if you want this to differ between treatments
# #
# #


# Make a function
dat_choice_fun = function(ngroup = 2, nplate = 20, totflies = 10, obs = 10, prob = 0.6, sigma = 0.1) {
  group = as.factor(rep( c("group1", "group2"), each = (obs*nplate)/ngroup)) # group variable
  plate = as.factor(rep(LETTERS[1:nplate], each = obs)) # plate variable
  plateeff = rep( rnorm(nplate, 0, sigma), each = obs) # plate effects drawn from a normal dist with mean=0 and sigma=sigma
  observation = as.factor(rep(rep(1:10), obs*ngroup)) # variable for each time you look the plates
  dummyobs = as.factor(rep(1:length(group))) # dummy variable of 1-n where n is your total sample size
  nflies = sample(0:totflies, 10) # number of flies that made a choice out of total number of flies in each plate
  A = c(rbinom(n = length(group[group == "group1"]), size = nflies, prob = prob + plateeff), # give nflies a "prob" of going on food "A" with "plateeff" noise)
        rbinom(n = length(group[group == "group2"]), size = nflies, prob = 0.5 + plateeff)) # make that preference for food "A" absent in "group2"; modify if needed, think about the biology here
  B = (nflies - A) # put the remainder of nflies on food B
  simdat = data.frame(group, plate, observation, dummyobs, A, B) # put that simulated data in to a dataframe
  
  # Choose model by commenting in/out (only keep one "in" at a time)
  m1 = glmer(cbind(A,B) ~ group + (1|plate), family = "binomial", data = simdat)
  m1
  # m2 = glmer(cbind(A,B) ~ group + (1|plate) + (1|dummyobs), family = "binomial", data = simdat) # overdispersion correction sensu Harrison (2015), DOI: 10.7717/peerj.1114) 
  # m2
  # m3 = glmer(cbind(A,B) ~ group + (1|plate/observation), family = "binomial", data = simdat) # "nested random effect"
  # m3
  # m4 = glmer(cbind(A,B) ~ group + (1|plate) + (1|observation), family = "binomial", data = simdat) # "crossed randome effect"
  # m4
  # m5 = glmer(A ~ group + (1|plate), family = "poisson", data = simdat) # poisson model of the counts on food A
  # m5
  # m6 = glmer(A ~ group + (1|plate) + (1|observation), family = "poisson", data = simdat) # poisson "crossed random" effects
  # m6
  # m7 = lmer(A ~ group + (1|plate) + (1|observation), data = simdat) # "crossed random effects" ANOVA of counts on food A
  # m7
  
  # lots of different models one could think of, try one at a time, simulate, looks at performance, repeat.
  
}


# Don't need to run this, it's just to ...
# test the function to see that it works 
set.seed(12) 
dat_choice_fun() # default parameter settings, and just printing "m1" at the end


# Ok, but that's just ONE simulated data set... We want 1000! :-)
# R is really fast with vectorized processes, not so fast with loops
# so, if you can, find a non-loop way (often I can't, and I resort to loops)






# Using replicate() but there are other ways...
# ~1% of these will generate singular fits with default settings, it's ok, ...
# ...and I'm not sure if that's an issue with the way I've designed the data, 
# or a mismatch between experimental design and analysis (i.e. modeling it as 
# a binary process when it's not really one).
sims = replicate(n = 200, # number of simulations
                 dat_choice_fun(), # again, change defaults here, e.g. dat_choice_fun(sigma = 0.01, totflies = 15)
                 simplify = FALSE ) # "simplify = FALSE" makes the output an object of class list
sims[[200]] # e.g. the 1000th data set/model






# Plot distribution of estimates 
# would be good to work out what estimate corresponds to 
# the loss of the 10% preference for food "A" in "group2"
# and put a vertical line at that spot to see how the model performs
# at recovering "the truth."
sims %>%
  map_df(broom.mixed::tidy) %>%
  filter(term == "groupgroup2") %>%
  ggplot( aes(x = estimate) ) +
  geom_density(fill = "blue", alpha = .5) # +
  # geom_vline( xintercept = ?) # that vertical line would go here with "+" at end of previous line
  
# you can equivalently get this from the summary like so
sims %>%
  map_dbl(~summary(.x)$coefficients[2]) %>%
  data.frame(coefficients = .) %>%
  ggplot( aes(x = coefficients) ) +
  geom_density(fill = "blue", alpha = .5) 

# you can therefore get WHATEVER you want from the summary (see "str(summary(sims[[1000]]))" for options)
# e.g. AIC
sims %>%
  map_dbl(~summary(.x)$AICtab[1]) %>% # BIC in AICtab[2] 
  data.frame(AICs = .) %>%
  ggplot( aes(x = AICs) ) +
  geom_density(fill = "blue", alpha = .5) 

# the "AICtab" line also holds the deviance and residual degrees of freedom...
# in AICtab[4] and AIC[5], respsectively, the ratio of which = overdispersion...
# ...but this shows an identical distribution to AIC, so, I either did something
# ...wrong or learned something new about AIC.
sims %>%
  map_dbl(~summary(.x)$AICtab[4] / summary(.x)$AICtab[5]) %>%
  data.frame(overdispersion = .) %>%
  ggplot( aes(x = overdispersion) ) +
  geom_density(fill = "blue", alpha = .5) 

# test statistics (from car's Anova() function)
sims %>%
  map_dbl(~Anova(.x, test.statistic="Chisq")$`Chisq`[1]) %>%
  data.frame(chisquare = .) %>%
  ggplot( aes(x = chisquare) ) +
  geom_density(fill = "blue", alpha = .5) 

# plot pvalues (also from car)
sims %>%
  map_dbl(~Anova(.x, test.statistic="Chisq")$`Pr(>Chisq)`[1]) %>% 
  data.frame(pvalue = .) %>%
  ggplot( aes(x = pvalue) ) +
  geom_density(fill = "blue", alpha = .5) 

# mean p value (not from car(), possibly matters, not sure, should be close)
sims %>%
  map_df(tidy) %>%
  filter(term == "groupgroup2") %>%
  pull(p.value) %>%
  mean()

# Statistical power to detect significant effect of that magnitude (e.g. sigma = 0.1)
sims %>%
  map_df(tidy) %>%
  filter(term == "groupgroup2") %>%
  pull(p.value) %>%
  {. <  0.05} %>% # proportion of simulations that found a pvalue >0.05
  mean()






# Under defaults for dat_choice_fun(), using m1, 
# ~1% are singular,
# models look overdispersed
# without correcting for that, the average pvalue is 0.09,
# and statistical power is 0.52 to detect significance of magnitude "sigma"

# Lot's of room to improve this function, especially if someone can find
# a more meaningful way to simulate the data so that it can be mathematically
# linked to the estimates that come from the models, and test how often the 
# models recover the "truth." But there's still some utility here as is to 
# compare modeling approaches withing a given set of parameter settings.








# e.g. for loops (recall that lists are indexed like "[[]]"):

# Create empty vectors to store p values of fixed effects
teststat <- 1:1000
pval <- 1:1000
AICs <- 1:1000
#
#

# Run function 1000 times and store the p values for fixed effects
for (i in 1:200) {
  teststat[i] <- Anova(sims[[i]], test.statistic="Chisq")$`Chisq`[1]
  pval[i] <- Anova(sims[[i]], test.statistic="Chisq")$`Pr(>Chisq)`[1]
  AICs[i] <- AIC(sims[[i]])
}
#
#


# plots and means
hist(teststat)
hist(pval)
hist(AICs)
mean(teststat)
mean(pval)
mean(AICs)









##########################
# end
##########################
