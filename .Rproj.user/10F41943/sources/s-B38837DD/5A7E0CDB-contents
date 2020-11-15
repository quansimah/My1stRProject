#
#
# MULTIPLE REGRESSION ANALYSIS
# Learning how to do a multiple regression analysis
#
# 
# load the libraries needed
library(AER)
library(MASS)

# ===== OMITTED VARIABLE BIAS ============================================
# What happens when your regressor is correlated with another determinant of y

# let us investigate using R

# load the data set
data("CASchools")

# define variables
CASchools$STR <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math)/2


# compute correlations
# is the proportion of teachers to students correlated with the student's score?
cor(CASchools$STR, CASchools$score)
# -0.2263627

# is the proportion of teachers to students correlated, 
# with the proportion of native English speakers in that school
cor(CASchools$STR, CASchools$english)
# 0.1876424

# what happens when we take the second part into account
# do smaller classes actually cause higher test scores
# or does including another variable explain everything better

# estimate both regression models
# a simple linear regression

mod <- lm(score~STR, data = CASchools)

# a multiple linear regression taking into account english speakers as well
mult.mod <- lm(score ~ STR + english , data = CASchools)

# What do the results show
mod
# Call:
#   lm(formula = score ~ STR, data = CASchools)

# Coefficients:
#   (Intercept)          STR  
# 698.93        -2.28  

mult.mod
# Call:
#   lm(formula = score ~ STR + english, data = CASchools)

# Coefficients:
#   (Intercept)          STR      english  
# 686.0322      -1.1013      -0.6498  

# So, what does this tell us?

# the coefficient of class structure lowers once english speakers are included
# this supports the claim that there was an omitted variable bias
# hence the multiple regression model does a better job of estimating scores

# ==============================================================================

# ==== MULTIPLE REGRESSION MODEL ===============================================
# Explaining mult.mod in previous section
# the coefficient on student-teacher ratio is the effect on test scores of one
# unit change of STR if the percentage of English learners is kept constant

# obtain information on the mult.mod coefficient using summary
summary(mult.mod)$coef
# Estimate Std. Error    t value      Pr(>|t|)
# (Intercept) 686.0322445 7.41131160  92.565565 3.871327e-280
# STR          -1.1012956 0.38027827  -2.896026  3.978059e-03
# english      -0.6497768 0.03934254 -16.515882  1.657448e-47

# on this note, the multiple regression model is :
# score = 686.03 -1.1STR - 0.65ENG 

# ==============================================================================

# ==== MEASURES OF FIT IN MULTIPLE REGRESSION ==================================

# What is the R^2 and Adjusted R^2 of our multiple regression model
# find out with summary
summary(mult.mod)
# Residual standard error: 14.46 on 417 degrees of freedom
# Multiple R-squared:  0.4264,	Adjusted R-squared:  0.4237 
# F-statistic:   155 on 2 and 417 DF,  p-value: < 2.2e-16

# Let us compare this effect size with that of the single regression model
summary(mod)
# Residual standard error: 18.58 on 418 degrees of freedom
# Multiple R-squared:  0.05124,	Adjusted R-squared:  0.04897 
# F-statistic: 22.58 on 1 and 418 DF,  p-value: 2.783e-06

# The multiple regression model shows a much better effect size

# ==============================================================================

# ==== OLS ASSUMPTIONS IN MULTIPLE REGRESSION ==================================

# PERFECT MULTICOLLINEARITY
# How does R react if we try to estimate a model with perfectly correlated regressors

# let's add a variable that is simply a fraction of the column english
CASchools$FracEL <- CASchools$english/100

# estimate the model
mult.mod2 <- lm(score ~ STR + english + FracEL, data = CASchools)

# obtain a summary of the new model 
summary(mult.mod2)

# the row FracEl in coefficients was excluded from the model automatically
# if we were to compute it by hand however, there will be no one to help us out

# ==============================================================================

# ==== DISTRIBUTION OF OLS ESTIMATES ===========================================
# load packages
library(MASS)
library(mvtnorm)

# set sample size
n <- 50

# initialize vector of coefficients
coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))

# set seed for reproducibility
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs[i,] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# compute density estimate
kde <- kde2d(coefs[, 1], coefs[, 2])

# plot density estimate
persp(kde, 
      theta = 310, 
      phi = 30, 
      xlab = "beta_1", 
      ylab = "beta_2", 
      zlab = "Est. Density")
# ==============================================================================

