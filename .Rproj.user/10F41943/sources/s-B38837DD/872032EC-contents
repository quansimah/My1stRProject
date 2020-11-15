#
#
#
# LINEAR REGRESSION USING R
# A linear regression workshop online

# loading the cars dataset
cars
head(cars)

# ===== GRAPHICAL ANALYSIS ===============================================
# plotting speed against distance
scatter.smooth(x= cars$speed, y= cars$dist, main ="Dist~ Speed")

# checking for outliers

par(mfrow = c(1,2))  # diving graph area in 2 columns

# make a box plot for speed
boxplot(cars$speed, main = "Speed",
        sub = paste("Outlier rows:",
                    boxplot.stats(cars$speed)$out))
# No outliers for speed

# make a box plot for distance
boxplot(cars$dist, main = "Distance",
        sub = paste("Outlier rows:",
                    boxplot.stats(cars$dist)$out))
# Outlier row for distance : 120

# is the response variable close to normality
# Using density plots
# load necessary library
library(e1071)
# divide graph area in 2 columns
par(mfrow=c(1,2))
# making a density plot for speed
plot(density(cars$speed), main = "Density Plot:Speed", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(cars$speed),2)))
# making the polygon red
polygon(density(cars$speed), col = 'red')
# Skewness = -0.11
# This means it is relatively normally skewed

# making a density plot for distance
plot(density(cars$dist), main = "Density Plot:Distance", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(cars$dist),2)))
# making the polygon red
polygon(density(cars$dist), col = 'red')
# Skewness = 0.76
# It is quite positively skewed

# ==============================================================================

# ======= CORRELATION ==========================================================
# test for correlation
cor(cars$speed, cars$dist)
# 0.8, suggest a strong correlation
# ==============================================================================

# ======= LINEAR MODEL =========================================================
# build a linear model
linearMod <- lm(dist~speed, data = cars)

# inspect the model
print(linearMod)
# Call:
#   lm(formula = dist ~ speed, data = cars)

# Coefficients:
#   (Intercept)        speed  
# -17.579        3.932 
# what this means is
# dist = -17.579 + 3.932*speed

# ==============================================================================

# === LINEAR REGRESSION DIAGNOSTICS ============================================
# get model summary 
summary(linearMod)
# Call:
#   lm(formula = dist ~ speed, data = cars)

# Residuals:
#   Min      1Q  Median      3Q 
# -29.069  -9.525  -2.272   9.215 
# Max 
# 43.201 

# Coefficients:
#   Estimate Std. Error
# (Intercept) -17.5791     6.7584
# speed         3.9324     0.4155
# t value Pr(>|t|)    
# (Intercept)  -2.601   0.0123 *  
#  speed         9.464 1.49e-12 ***
#  ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’
# 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

# ==============================================================================

# ==== T-STATISTIC AND P-VALUE =================================================
# capture model summary as an object
modelSummary <- summary(linearMod)

# captured model coefficients as an object
modelCoeffs <- modelSummary$coefficients

# get estimates for speed
beta.estimate <- modelCoeffs["speed", "Estimate"]

# get standard error for speed
std.error <- modelCoeffs["speed", "Std. Error"]

# calculating t-value
t_value <- beta.estimate/std.error
# 9.46399

# calculating the p-value
p_value <- 2*pt(-abs(t_value), df = nrow(cars)-ncol(cars))
# 1.489836e-12

# f- statistic
f_statistic <- linearMod$fstatistic[1]
# calculating the parameters for model p-value
f<- summary(linearMod)$fstatistic
#   value    numdf    dendf 
# 89.56711  1.00000 48.00000 
model_p <-  pf(f[1],f[2],f[3], lower =FALSE)
# model p-value
# 1.489836e-12 

# ==============================================================================

# Find AIC and BIC
AIC(linearMod)
# 419.1569
BIC(linearMod)
# 424.8929
# The lower the better
# ==============================================================================

# ===== PREDICTING LINEAR MODELS ===============================================

# Create Training Test data
#  set seed to reproduce results of random sampling
set.seed(100)
# row indices for training data
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))

# make training Data
trainingData <- cars[trainingRowIndex, ]

# make test Data 
testData <- cars[-trainingRowIndex, ]

# Build the model on training data
lmMod <- lm(dist~speed, data =trainingData)

# use the model to predict distance
distPred <- predict(lmMod, testData)

# Review diagnostic measures
summary(lmMod)
# Call:
#   lm(formula = dist ~ speed, data = trainingData)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -24.726 -11.242  -2.564  10.436  40.565 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -20.1796     7.8254  -2.579   0.0139 *  
#   speed         4.2582     0.4947   8.608 1.85e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 15.49 on 38 degrees of freedom
# Multiple R-squared:  0.661,	Adjusted R-squared:  0.6521 
# F-statistic: 74.11 on 1 and 38 DF,  p-value: 1.848e-10

AIC(lmMod)
# 336.6933

# What does all this tell us?

# both the predictor's and model's p value are less than 0.05
# So we have a statistically significant model


# ==== PREDICTION ACCURACY =====================================================
# make a dataframe with the actual and predicted data
actual_preds <- data.frame(cbind(actuals = testData$dist,
                                 predicteds =distPred))
# what is the correlation accuracy
correlation_accuracy <- cor(actual_preds)
# 90.3 %

# inspect data
head(actual_preds)

# calculate min_max accuracy
min_max_accuracy <- mean(apply(actual_preds,1,min)/
  apply(actual_preds,1,max))
# 73.11% (the higher the better)

# calculate mean absolute percentage error
mape <- mean(abs((actual_preds$predicteds - actual_preds$actuals))/
               actual_preds$ actuals)
# 49.59% , mean absolute percentage deviation (the lower the better)

# ==============================================================================

# ===== K-FOLD CROSS VALIDATION ===============================================

# load the necessary library
library(lattice)
library(DAAG)

# performing the cross validation with 5 folds

# change margins first
par(mfrow = c(1,1))
# make plot
cvResults <- suppressWarnings( CVlm (cars, form.lm = dist ~ speed,
                                   m=5, dots=FALSE, seed=29,
                                   legend.pos="topleft",  printit=FALSE,
                                   main="5-Fold Cross Validation"))

# supress warning supresses the warnings
# smaller symbols are predicted values while bigger ones are actuals
# lines are very close, however, they are not exactly parallel
# fold 2 crosses the rest slightly

# find mean squared error of k-fold cross validation
attr(cvResults, 'ms')
# 254.2661 , the lower the better 
# ==============================================================================



