# Load libraries necessary for analyses.
library(tidyverse)
library(lme4)
library(lmerTest)

# Data downloaded from https://sites.stat.columbia.edu/gelman/arm/ as zip file to 
# Downloads directory, then opened into directory by double clicking, prior to running.
cd4dat <- read_csv('~/Downloads/ARM_Data/cd4/allvar.csv')

# Use tidyverse dplyr mutate to create time variable as current age minus baseline age,
# dependent variable y as the square root of CD4 percentage, and treat variable with 1
# as the treatment group and 0 the control group.
cd4dat <- cd4dat %>% 
  mutate(time = visage - baseage,
         y = sqrt(CD4PCT),
         treat = treatmnt - 1)

# Fit model predicting square root of CD4 percentage as linear function of baseline age,
# current time past baseline, and treatment, using OLS linear regression. In this model,
# treatment has a significant positive effect.
lrmod <- lm(y ~ baseage + time + treat, data = cd4dat)
summary(lrmod)

# Fit the same model adding a random intercept term to model a common non-negative 
# correlation among observations for the same subject. Accounting for correlations
# results in treatment effect no longer being significant. Also look at likelihood
# ratio test of random intercept variance, which clearly indicates that it is
# warranted.
rimod <- lmer(y ~ baseage + time + treat + (1 | newpid), data = cd4dat)
summary(rimod)
ranova(rimod)

# Add a random slope to the fixed time slope, allowing the variance components for
# the random intercept and slope to correlate. Treatment remains nonsignificant, the
# model clearly fits better than the others (AIC is 3123.0, vs. 3149.2 for the one
# with just the random intercept). Note that the correlation between the random
# slope and intercept terms is very small (-.04).
rsimod <- lmer(y ~ baseage + time + treatmnt + (1 + time | newpid), data = cd4dat)
summary(rsimod)
ranova(rsimod)

# Fit a slightly simpler model still including the random slope and intercept terms, 
# but constrain these to be uncorrelated. The ranova output doesn't show a likelihood
# ratio test for removal of just that correlation, but the model has an AIC of 3121.1,
# which is better than the 3123.0 value for the model where the two correlate, so this
# model would be preferred. Treatment remains nonsignificant.
simpmod <- lmer(y ~ baseage + time + treatmnt + (1 + time || newpid), data = cd4dat)
summary(simpmod)
ranova(simpmod)

# Compare AIC values for the four models.
AIC(lrmod,rimod,rsimod,simpmod)
