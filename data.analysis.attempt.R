#
#
#
# DATA ANALYSIS
# Performing Regression Analyses of clean data
#
#  packages installed
# visreg,lmerTest, emmeans

# load libraries
library(visreg)
library(lmerTest)
library(ggplot2)
library(tidyverse)
library(emmeans)

# read csv
debt.data <- read.csv(paste(path.data.clean,"debt.clean.csv",
                           sep = ""),
                     stringsAsFactors = FALSE, 
                     strip.white = TRUE)
head(debt.data)
# ======= DEBT STOCK REGRESSION ================================================
# do a simple linear regression
 raw.dsf.plot <- stripchart(debt.stock ~ round(log.financial.integration,2),
                            data = debt.data,
           vertical = TRUE,
           pch = 16, 
          cex = 1.5, las = 1, col = "firebrick",
          xlab = "Financial Integration", ylab = "Debt Stock")
# insert a line of best fit
y<- lm(debt.stock ~ log.financial.integration, data = debt.data)
abline(y)
# absolutely horrible


# creating a plot that parses data by year
dsf.int.plot <- interaction.plot(response = debt.data$debt.stock,
                 x.factor = round(debt.data$log.financial.integration,2), 
                 trace.factor = debt.data$years,
                 legend = FALSE, lty = 1, xlab = "Financial Integration", 
                 ylab = "Debt stock", type = "b", pch = 16, las = 1, 
                 cex = 1.5)

# making a linear mixed effect model
z <- lmer( debt.stock ~ log.financial.integration + (1|years), data = debt.data) 
summary(z)

model.z <- interaction.plot(round(debt.data$log.financial.integration,2),debt.data$years, 
                 debt.data$debt.stock)
debt.by.year<- visreg(z, xvar = "log.financial.integration",
       by = "years", scales=list(rot = 90))
# 
plot(z)


# results
# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: debt.stock ~ log.financial.integration + (1 | years)
#    Data: debt.data

# REML criterion at convergence: 800.6

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3845 -0.5081 -0.1954  0.3771  3.5282 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  years    (Intercept)  807.8   28.42   
# Residual             1095.3   33.10   
# Number of obs: 82, groups:  years, 3

# Fixed effects:
#                          Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)                151.467     62.948  70.662   2.406   0.0187 *
# log.financial.integration  -10.493      6.563  79.308  -1.599   0.1138  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Correlation of Fixed Effects:
#             (Intr)
# lg.fnncl.nt -0.964

# ==============================================================================

# ===== DEBT SERVICE CAPABILITY REGRESSION =====================================
# do a simple linear regression
raw.dscf.plot <- stripchart(debt.service.cap ~ round(log.financial.integration,2),
                           data = debt.data,
                           vertical = TRUE,
                           pch = 16, 
                           cex = 1.5, las = 1, col = "firebrick",
                           xlab = "Financial Integration",
                           ylab = "Debt Service Capabilities ")
# insert a line of best fit
x<- lm(debt.service.cap ~ log.financial.integration, data = debt.data)
abline(x)
# absolutely horrible


# creating a plot that parses data by year
dscf.int.plot <- interaction.plot(response = debt.data$debt.service.cap,
                                 x.factor = round(debt.data$log.financial.integration,2), 
                                 trace.factor = debt.data$years,
                                 legend = FALSE, lty = 1,
                                 xlab = "Financial Integration", 
                                 ylab = "Debt Service Capabilities ",
                                 type = "b", pch = 16, las = 1,cex = 1.5)

# making a linear mixed effect model
a <- lmer( debt.service.cap ~ 
             log.financial.integration + (1|years), data = debt.data) 
summary(a)
plot(a)

# Results
# Linear mixed model fit by REML. t-tests use Satterthwaite's
#   method [lmerModLmerTest]
# Formula: 
# debt.service.cap ~ log.financial.integration + (1 | years)
#    Data: debt.data

# REML criterion at convergence: 538.3

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.9720 -0.4944 -0.1894  0.4696  4.7104 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  years    (Intercept) 24.95    4.995   
#  Residual             38.15    6.177   
# Number of obs: 83, groups:  years, 3

# Fixed effects:
#                           Estimate Std. Error     df t value
# (Intercept)                 -5.674     11.692 73.150  -0.485
# log.financial.integration    1.609      1.224 80.433   1.315
#                           Pr(>|t|)
# (Intercept)                  0.629
# log.financial.integration    0.192

# Correlation of Fixed Effects:
#             (Intr)
# lg.fnncl.nt -0.967
# shows no statistically significant correlation
# however, there appears to be one very big outlier affecting the results 
# i would need to remove that outlier and try again 

model.a <- interaction.plot(round(debt.data$log.financial.integration,2),
                            debt.data$years, 
                            debt.data$debt.stock)
debt.cap.by.year<- visreg(z, xvar = "log.financial.integration",
                      by = "years", scales=list(rot = 90))
# ==============================================================================

# there is an extreme outlier that appears to be disturbing the data

max(debt.data$debt.stock, na.rm = T)

