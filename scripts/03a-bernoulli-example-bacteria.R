# Load the required library
library(MASS)

# Load the bacteria dataset
data(bacteria)

# View the structure of the bacteria dataset
glimpse(bacteria)

# Fit a logistic regression model with interaction between treatment and week
model.bact1 <- glm(y ~ trt * week, data = bacteria, family = binomial)

# Perform a likelihood ratio test for dropping each term sequentially from the model
drop1(model.bact1, test = "Chi")

# Fit a logistic regression model with main effects of treatment and week
model.bact2 <- glm(y ~ trt + week, data = bacteria, family = binomial)

# Perform a likelihood ratio test for dropping each term sequentially from the model
drop1(model.bact2, test = "Chi")

# Summarize the results of the logistic regression model
summary(model.bact2)
