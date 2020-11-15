#
#
#
# DATA ANALYSIS OF DEBT DATA
# Performing Regression Analyses of clean debt data
#
#


# ==== READ CSV ================================================================
debt.data <- read.csv(paste(path.data.clean,"debt.clean.csv",
                            sep = ""),
                      stringsAsFactors = FALSE, 
                      strip.white = TRUE, na.strings = "NA")
head(debt.data)
# ==============================================================================

# ====== COUNTRY DATA SUBSETS ================================================
# Create subsets of data according to years
# subset 2009 data
data.2009 <- subset(debt.data,years == "2009")

# subset 2014 data
data.2014 <- subset(debt.data,years == "2014")

#  subset 2019 data
data.2019 <- subset(debt.data,years == "2019")
# ==============================================================================

# ======= DEBT STOCK REGRESSION ================================================
# do a simple linear regression
# plotting speed against distance
scatter.smooth(x= debt.data$financial.integration,
               y= debt.data$debt.stock, main ="Debt stock~ financial integration")

# checking for outliers

par(mfrow = c(1,2))  # diving graph area in 2 columns

# make a box plot for financial integration
boxplot(debt.data$financial.integration, main = "Financial Integration")

# find outliers
boxplot.stats(debt.data$financial.integration)[]

# make vector of outliers
fi.out <- c(17700686746, 32132536118, 36607977032,
             56197765491, 18027049171, 10689168577,
            33128223077, 53612271731)


# make a box plot for debt stock
boxplot(debt.data$debt.stock, main = "Debt Stock")
        
# create vector of outliers        
ds.out <- boxplot.stats(debt.data$debt.stock)$out

# is the response variable close to normality
# Using density plots
# divide graph area in 2 columns
par(mfrow=c(1,2))
# making a density plot for debt.stock
plot(density(debt.data$debt.stock,na.rm = T), main = "Density Plot: Debt.stock", 
     ylab = "Frequency",
     sub = paste("Skewness:",round(e1071::skewness(debt.data$debt.stock, na.rm = T),2)))
# making the polygon red
polygon(density(debt.data$debt.stock, na.rm =T), col = 'red')
# Skewness =  2.69
# This means that the data is positively skewed

# making a density plot for financial integration
plot(density(debt.data$financial.integration,na.rm = T), 
     main = "Density Plot: Financial.Integration", 
     ylab = "Frequency",
     sub = paste("Skewness:",round(e1071::skewness(debt.data$financial.integration,
                                                   na.rm = T),2)))
# making the polygon red
polygon(density(debt.data$financial.integration, na.rm =T), col = 'red')
# Skewness =  3.69
# This means that the data is positively skewed


# run a correlation test
cor.test(debt.data$debt.stock,debt.data$financial.integration)
#   cor 
# -0.1806223

# The results show no statistically significant correlation
# This correlation is a weak negative correlation


# what happens when we make a linear model of this poorly structured data
debt.mod<- lm(debt.stock ~ financial.integration, data = debt.data,
       na.action = na.exclude)
debt.mod
# Coefficients:
# (Intercept)  financial.integration  
# 5.462e+01             -7.218e-10  

# see if a polynomial fit will be better
debt.mod_2 <- lm(debt.stock ~ financial.integration + I(financial.integration^2),
          data = debt.data )
# Coefficients:
# (Intercept)       financial.integration  
# 6.166e+01                  -3.668e-09  
# I(financial.integration^2)  
# 6.294e-20 

# let's visualize how well these lines fit the data
# set margins back to normal
par(mfrow = c(1,1))
# let's plot our data
debt.plot<- stripchart(debt.stock ~ financial.integration,
                        data =debt.data,
                        vertical = TRUE,
                        pch = 16, 
                        cex = 1.5, las = 1, col = "firebrick",
                        xlab = "Financial Integration", ylab = "Debt Stock",
                        main = "Debt stock ~ Financial Integration")
# overlay both models on the data
abline(debt.mod)
abline(debt.mod_2)
# Warning message:
# In abline(debt.mod_2) :
#         only using the first two of 3 regression coefficients

# what happens when I take into account the years
mult.debt.mod<- lm(debt.stock ~ factor(years) + financial.integration, data =
                    debt.data)
# Coefficients:
#         (Intercept)      factor(years)2014      factor(years)2019  
# 9.234e+01             -5.922e+01             -4.988e+01  
# financial.integration  
# -2.608e-10 


# since my data did not show correlation, I decided against using it
# there was no real claims I could make given the shortcomings of the dataset
# the sample size was small, and there was a lot of missing data and outliers
# This could also be because the factors I look at,
# have a very low association in real life
# As a result, I decided to move on to a simulated dataset
# This project was to learn how to perform a regression analyses in R
# See subsequent scripts for guidance

# ==============================================================================

# ==== COMPARE MODELS ==========================================================
# All 3 models did not tell us much
# But, let us compare their adjusted R-squared values

# for the simple linear regression model
summary(debt.mod)
# Residual standard error: 41.08 on 80 degrees of freedom
# (62 observations deleted due to missingness)
# Multiple R-squared:  0.03262,	Adjusted R-squared:  0.02053 
# F-statistic: 2.698 on 1 and 80 DF,  p-value: 0.1044

# for the polynomial regression model
summary(debt.mod_2)
# Residual standard error: 40.17 on 79 degrees of freedom
# (62 observations deleted due to missingness)
# Multiple R-squared:  0.08686,	Adjusted R-squared:  0.06374 
# F-statistic: 3.757 on 2 and 79 DF,  p-value: 0.02762

# for the multiple regression model
summary(mult.debt.mod)
# Residual standard error: 33.42 on 78 degrees of freedom
# (62 observations deleted due to missingness)
# Multiple R-squared:  0.3758,	Adjusted R-squared:  0.3517 
# F-statistic: 15.65 on 3 and 78 DF,  p-value: 4.648e-08

# for all 3, the multiple regression model explained the values best
# however, it showed extremely low coefficients 
# Thence,there can hardly be any claims on the association of variables
# ==============================================================================