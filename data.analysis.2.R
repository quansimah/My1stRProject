#
#
#
# DATA ANALYSIS
# Performing Regression Analyses of clean data
#
# installing packages
# load packages
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)

# first, estimating simple causal effects
mod <- lm(debt.stock ~ log.financial.integration, data = debt.data )
mod
# Coefficients:
# (Intercept)  
# 278.65  
# log.financial.integration  
# -24.56  

# plot debt.stock by year
this.plot <- plot(factor(debt.data$years), debt.data$debt.stock)
that.plot <- plot(factor(debt.data$years), debt.data$log.financial.integration)
the.plot <- plot(factor(debt.data$years),debt.data$debt.service.cap)
