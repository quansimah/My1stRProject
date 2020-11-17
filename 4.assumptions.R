#
#
# ASSUMPTIONS FOR A LINEAR REGRESSION
# What are the assumptions that our linear model has to fulfill?


# ==== LOAD LINEAR MODEL =======================================================
# To test the assumptions on the previously generated linear model,
# we need to read the save linear Model 
linearMod <-readRDS(paste(path.results,"linearMod.RDS",
                          sep = ""))
# ==============================================================================





# ==== ASSUMPTION 1&2 ==========================================================

# The mean of the residuals of the linear model is approximately zero
mean(linearMod$residuals)
# -1.064079e-16
# Yes, the mean of the residuals is very low

# The residuals are homoscedastic

# we check this by plotting the linear model
# change margins to see all 4 diagnostics  at once
par(mfrow =c(2,2))
# plot model
plot(linearMod)

# the top and bottom left plots show  relatively flat lines
# hence, the residuals of the model are homoscedastic
# ==============================================================================

# ==== ASSUMPTION 4 ============================================================
# the residuals are not autocorrelated
# we check this by using the acf function from ggplot2

# set margins back to normal to see plot on the full window
par(mfrow = c(1,1))

# make the acf plot
acf(linearMod$residuals)
# from the plot the residuals, are not autocorrelated


# we can also check for autocorrelation using the  Durbin-Watson Test
lmtest::dwtest(linearMod)
# DW = 2.0973, p-value = 0.6326
# We failed to reject the null hypothesis that the model's residuals are random
# This means that there is no pattern in the model's residuals

# how to rectify if the residuals were autocorrelated
# we will need the Rpackage "DataCombine" here 
# first, we combine the residuals to our dataframe
# then  we lag the residual column by 1 using the slide function
# then we remove all NAs from the dataset
# and then, we make a new linear model
# the new model uses both the orignal regressor and the lagged residuals as its regressors 
# you can then check the new model for autocorrelation

# ==============================================================================

# ==== ASSUMPTION 5 ============================================================
# The regressor and the residuals of the linear model are uncorrelated
# We can test this using the cor.test function
cor.test(sim.data$X1, linearMod$residuals)
# p- value = 1 
# The p-value is so high
# Hence, we failed to reject the null hypothesis that true correlation is 0
# this means that the regressor and the residuals of our model are uncorrelated

# ==============================================================================

# ========= ASSUMPTION 6 -8 ===================================================

# Assumption 6
# the number of observations must be greater than number of Xs
# directly observed by looking at data

# ASSUMPTION 7
# The variability in x is positive
var(sim.data$X1)
# 3.975507
# True

# ASSUMPTION 10
# Normality of residuals
par(mfrow =c(2,2))
# Tested using the qqplot on top right corner of the linear model plots
plot(linearMod)
# the plot appears normally distributed

# ==============================================================================

# ===== CHECK ASSUMPTIONS AUTOMATICALLY ========================================
# we use the Rpackage gvlma

# check assumptions for our model
gvlma(linearMod)
#                     Value p-value         Decision
# Global Stat        2.3217  0.6768 Assumptions acceptable.
# Skewness           0.4636  0.4960 Assumptions acceptable.
# Kurtosis           1.4787  0.2240 Assumptions acceptable.
# Link Function      0.2577  0.6117 Assumptions acceptable.
# Heteroscedasticity 0.1217  0.7271 Assumptions acceptable.

# All assumptions are satisfied
# ==============================================================================


# ----- IMPORTANT NOTE ---------------------------------------------------------
# With a small dataset even 2 or 3 outliers can distort our data
# In such instances, you can choose to take out outliers 
# From the Cook's distance model, certain points are indicated as outliers
# one can  rebuild their model without these outliers
# The changes to the actual model might be minor
# However, it allows conformation to the assumptions
# If you are not sure how much an outlier influences your model, you can check.
influence.measures(linearMod)
#The outlier row might have an "*" beside its influence measure
# If it does, it is most likely distorting your model
# Note however, that removing outliers is not necessarily the best practice 

# ------------------------------------------------------------------------------


