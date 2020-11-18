#
#
# MULTIPLE REGRESSION ANALYSIS
#  Performing a  multiple regression analysis on simulated data
#

# ====== READ CSV ==============================================================
# read the simulated data csv and  assign to object sim.data
sim.data <- data.t <- read.csv(paste(path.data.clean,"simulated.data.csv",
                                     sep = ""), stringsAsFactors = FALSE,
                               strip.white = TRUE)
# ==============================================================================

# ==== LOAD LINEAR MODEL =======================================================
# for this section, we need the Rpackages AER and MASS, which are already loaded
# in main
# we also need  to read the saved linear model since we will be comparing
linearMod <-readRDS(paste(path.results,"linearMod.RDS",
                          sep = ""))
# ==============================================================================


# ===== OMITTED VARIABLE BIAS ==================================================
# What happens when your regressor is correlated with another determinant of y?

# if we remember,there was another determinant of Y that was correlated with X1
# check "simulation.R"
# in our orignal model, we put it aside
# let us take it into account in a new model
mult.mod <- lm(Y ~ X1 + X2 , data = sim.data)

# How do the results from the first model and this one differ
linearMod
# Call:
# lm(formula = Y ~ X1, data = sim.data)

# Coefficients:
#  (Intercept)           X1  
#       105.308        4.436 

mult.mod
# Call:
#   lm(formula = Y ~ X1 + X2, data = sim.data)

# Coefficients:
#   (Intercept)           X1           X2  
#       -20.424        3.451        3.009 


# So, what do we observe?
# the coefficient of X1 lowers once X2 is included
# This is what we call an omitted variable bias
# We omitted a variable that both explains Y and is correlated with X1 in linearMod
# Hence we created a model that overestimated the influence of X1 on Y
# That is the omitted variable bias
# It is why in most cases, a multiple linear model suits a dataset better

# ==============================================================================

# ==== MULTIPLE REGRESSION MODEL ===============================================

# obtain information on the mult.mod coefficients using summary
summary(mult.mod)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -20.424381 14.8175761 -1.378389 1.746096e-01
# X1            3.451497  0.3143780 10.978812 1.443931e-14
# X2            3.009099  0.3042933  9.888811 4.588627e-13
# => Y = -20.42 + 3.45X1 + 3.009X2

# ==============================================================================

# ==== MEASURES OF FIT IN MULTIPLE REGRESSION ==================================

# What is the R^2 and Adjusted R^2 of our multiple regression model
# find out with summary
summary(mult.mod)
# Residual standard error: 4.162 on 47 degrees of freedom
# Multiple R-squared:  0.8716,	Adjusted R-squared:  0.8661 
# F-statistic: 159.5 on 2 and 47 DF,  p-value: < 2.2e-16

# Let us compare this effect size with that of the single regression model
summary(linearMod)
# Residual standard error: 7.229 on 48 degrees of freedom
# Multiple R-squared:  0.6044,	Adjusted R-squared:  0.5962 
# F-statistic: 73.35 on 1 and 48 DF,  p-value: 3.133e-11


# 0.8661 > 0.5962
# The multiple regression model  has a much higher adjusted R-squared

# ==============================================================================

# ==== OLS ASSUMPTIONS IN MULTIPLE REGRESSION ==================================
# The assumptions  of a multiple regression model are similar to that for a 
# linear regression, except that of multicollinearity
# Multiple regressions require that no two regressors are perfectly correlated
# In such cases,R does not compute coefficients for the perfectly correlated regressor


# The higher the correlation between regressors, the higher the variance of ..
# ... model coefficient estimates. 
# to check the variance inflation, we use the vif function from the DAAG package
vif(mult.mod)
#     X1     X2 
# 1.1114 1.1114 
# a vif of more than 4 is cause for concern 
# for our model, we can see that the regressors are not multicorrelated

# to visualize this relationship, we can use the corrplot function
# this is obtained from the Rpackpage "corrplot"
# we plot the regressors without the response variable
corrplot(cor(sim.data[,-3]))

# from the plot, we see that X1 and X2 are not very correlated

# ==============================================================================

#=== COMPARING AIC AND BIC =====================================================
# Calculate the AIC of the simple linear Model
AIC(linearMod)
# 343.659

# Calculate the AIC of the multiple linear model
AIC(mult.mod)
# 289.4026

# The AIC of the multiple linear model is lesser than than of the simple linear
# model. 

# ==============================================================================

# ==== DISTRIBUTION OF OLS ESTIMATES ===========================================
# we need to load the R package "MASS"

# initialize vector of coefficients
coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))

# set seed for reproducibility
set.seed(1)

# loop sampling and estimation of our model
# create a loop that randomly generates numbers
# this will be done in the same way we simulated data
for (i in 1:10000) { 
A<- rmvnorm(50, c(25, 50), sigma = cbind(c(5, 1.25), c(1.25, 10)))
e <- rnorm(50, sd = 5)
B <- 5 + 2.5 * A[, 1] + 3 * A[, 2] + e
# store coefficients in vector for coefficients
  coefs[i,] <- lm(B ~ A[,1] + A[,2])$coefficients[- 1]
  
}


# compute density estimates
kde <- kde2d(coefs[,1], coefs[,2])


# plot density estimate
persp(kde, 
      theta = 30, 
      phi = 30, 
      xlab = "beta_1", 
      ylab = "beta_2", 
      zlab = "Est. Density",
      main = "Distribution of Model Estimates")

# the plot shows a distribution of coefficients estimated by R
# ==============================================================================

 

