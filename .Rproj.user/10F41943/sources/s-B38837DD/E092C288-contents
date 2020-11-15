#
#
# ASSUMPTIONS
# Testing assumptions for linear model created

# Run the script where the linear model was created first
source("linear.regression.R")




# ====== CHECK ASSUMPTIONS ======================================================

# Is the mean of residuals approximately zero
mean(linearMod$residuals)
# [1] 8.65974e-17
# Yes

# is there homoscedasticity
# change margins to see
par(mfrow =c(2,2))
plot(linearMod)
# the top  and bottom left are both relatively flat

# ==== AUTO CORRELATION ========
# set margins back to normal
par(mfrow = c(1,1))
library(ggplot2)
acf(linearMod$residuals)
# the residuals are not autocorrelated from the diagram

# In order to learn how to rectify autocorrelation, I loaded a new dataset
data("economics")
lmMod <- lm(pce ~ pop, data = economics)
acf(lmMod$residuals)
# highly autocorrelated data

# Use Runs Test
runs.test(lmMod$residuals)
# R does not recognize the runs test function

# Use Durbin-Watson Test
lmtest::dwtest(lmMod)
# p-value < 2.2e-16
# Hence, we reject the null that it is random
# This means that there is a definite pattern in residuals

# Rectify autocorrelation
# load libraries
library(DataCombine)
# combining residuals to the origina dataframe
econ_data <- data.frame(economics, resid_mod1 = lmMod$residuals)
# lagging residual models by 1 unit
econ_data_1 <- slide(econ_data, Var = "resid_mod1", NewVar = "lag1",
                     slideBy = -1)
# removing all NAs from dataset
econ_data_2 <- na.omit(econ_data_1)

# making a new linear model
lmMod2 <- lm(pce ~ pop+ lag1, data = econ_data_2)


# check if autocorrelation is solved
acf(lmMod2$residuals) 
# autocorrelation resolved

# performing a Durbin-Watson test on the new Linear Model
lmtest::dwtest(lmMod2)
# DW = 2.0309, p-value = 0.6126
# Therefore, we cannot reject the null hypothesis that it is random
# hence, we can safely assume that the residuals are not autocorrelated


# ASSUMPTION 5
# Are the Xvariables and residuals uncorrelated
# Using the cars data frame
cor.test(cars$speed, linearMod$residuals)
# p- value = 1 
# The p-value is so high
# Hence, we can't reject the null hypothesis that true correlation is 0

# ASSUMPTION 6
# the number of observation must be greater than number of Xs
# directly observed by looking at data

# ASSUMPTION 7
# The variability in x is positive
var(cars$speed)
# 27.95918
# True

# ASSUMPTION 8
# Is the regression model correctly specified?

# ASSUMPTION 9
# No perfect multicollinearity
# There is no perfect linear relationship between explanatory variables
head(mtcars)
mod2 <- lm(mpg ~ ., data = mtcars)
vif(mod2)
?vif
# a vif should be less than 4 
# This is in order to be accepted as not causing multicollinearity

#     cyl    disp      hp    drat      wt    qsec      vs 
# 15.3740 21.6200  9.8320  3.3746 15.1650  7.5280  4.9659 
# am    gear    carb 
# 4.6485  5.3575  7.9087 

# Almost all vifs are higher than 4
# How do we rectify this?

# Way 1
# Iteratively remove the X var with the highest VIF

# Way 2
# See the correlation between all variables
# Keep only one of all highly correlated pairs
# load library for that process
library(corrplot)
corrplot(cor(mtcars[,-1]))
# from the plot
# The correlated pairs are:
# => disp, cyl, hp, wt
# => gear, am
# => - hp, carb

# ASSUMPTION 10
# Normality of residuals
par(mfrow =c(2,2))
# Tested using the qqplot on top right corner
plot(linearMod)
# the plot appears normally distributed


# CHECK ASSUMPTIONS AUTOMATICALLY
# use necessary library
library(gvlma)

# check assumptions
gvlma(linearMod)
# Value  p-value                   Decision
# Global Stat        15.801 0.003298 Assumptions NOT satisfied!
#   Skewness            6.528 0.010621 Assumptions NOT satisfied!
#   Kurtosis            1.661 0.197449    Assumptions acceptable.
# Link Function       2.329 0.126998    Assumptions acceptable.
# Heteroscedasticity  5.283 0.021530 Assumptions NOT satisfied!

# Three assumptions are not satisfied
# this could be because we have only 50 data points
# This means, even 2 or 3 outliers can impact the quality of the data


# Using the diagnostic plots, certain plots are marked as outliers
# let's rebuild the model without them
new_mod <- lm(dist~speed, data =cars[-c(23,35,49), ])

# check assumptions for new model
gvlma(new_mod)

#                    Value  p-value       Decision
# Global Stat        7.5910 0.10776 Assumptions acceptable.
# Skewness           0.8129 0.36725 Assumptions acceptable.
# Kurtosis           0.2210 0.63831 Assumptions acceptable.
# Link Function      3.2239 0.07257 Assumptions acceptable.
# Heteroscedasticity 3.3332 0.06789 Assumptions acceptable.


plot(new_mod)
#  the changes look minor
# However, they allow conformation to the assumptions

# UNDERSTANDING THE COOK'S DISTANCE MODEL
# A point far from the certroid can severely distort the regression

# we can check influence measures
influence.measures(linearMod)
# but what happens next


