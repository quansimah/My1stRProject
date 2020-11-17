#
#
# SIMPLE LINEAR REGRESSION
# Carry out a simple linear regression on the simulated data

# ==== BACKGROUND =============================================================
# For the simple linear regression, we only take into account two variables
# These variables will be obtained from a previously R simulated data
# See how the simulation was carried out in the file "simulation.R"
# The variables to be considered from the dataset will be X1 and Y
# Let us consider X1 as the explanatory variable and Y as the response variable
# ==============================================================================

# ====== READ CSV ==============================================================
# read the simulated data csv and  assign to object sim.data
sim.data <- data.t <- read.csv(paste(path.data.clean,"simulated.data.csv",
                                     sep = ""), stringsAsFactors = FALSE,
                               strip.white = TRUE)
# ==============================================================================

# ===== GRAPHICAL ANALYSIS =====================================================

# First, let us carry out a graphical analysis of our variables


# let us plot X1 againnst Y1 to visualize their relationship


slr.plot <- plot(x= sim.data$X1, y= sim.data$Y, main ="X1~ Y",
                           pch =16, col = 'red', xlab = "X1", ylab = "Y")

# do outliers affect the line in the scatter plot?
# let us check for outliers in both variables

# We want to visualize the plots of both variables side by side
# so we divide the graph area into 2 columns using par
par(mfrow = c(1,2))

# make a box plot for X1
boxplot(sim.data$X1, main = "X1",
        sub = paste("Outlier rows:",
                    boxplot.stats(sim.data$X1)$out))
# There are no outliers for X1

# make a box plot for Y
boxplot(sim.data$Y, main = "Y",
        sub = paste("Outlier rows:",
                    boxplot.stats(sim.data$Y)$out))
# There are no outliers for Y
# We are good to go 


# Now let's find out if both variables are close to normality using density plots
# this is  done with the Rpackage e1071

# making a density plot for X1 
plot(density(sim.data$X1), main = "Density Plot:X1", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(sim.data$X1),2)))
# making the polygon red
polygon(density(sim.data$X1), col = 'red')
# Skewness = 0.01
# This is a very low level of skewness

# making a density plot for Y
plot(density(sim.data$Y), main = "Density Plot:Y", 
     ylab = "Frequency", sub = paste("Skewness:",
                                     round(e1071::skewness(sim.data$Y),2)))
# making the polygon red
polygon(density(sim.data$Y), col = 'red')
# Skewness = 0.18
# This is also a low level of skewness

# ==============================================================================

# ======= CORRELATION ==========================================================

# let us test for correlation between both variables
cor(sim.data$Y, sim.data$X1)
# 0.774579;  this suggests a strong positive correlation between both variables

# ==============================================================================

# ======= LINEAR MODEL =========================================================
# build a linear model
linearMod <- lm(Y ~ X1, data = sim.data)

# inspect the model
print(linearMod)
#Call:
# lm(formula = Y ~ X1, data = sim.data)

# Coefficients:
#   (Intercept)           X1  
#       105.308        4.436 
# This translates to Yestimate = 105.308 + 4.436*X1

# since the linear model will be used in other scripts, save the linear model
saveRDS(linearMod,paste(path.results,"linearMod.rds", sep = ""))

# ==============================================================================

# === LINEAR REGRESSION DIAGNOSTICS ============================================
# get model summary 
summary(linearMod)
#> Call:
#> lm(formula = Y ~ X1, data = sim.data)

#> Residuals:
#>   Min       1Q   Median       3Q      Max 
#> -13.4182  -6.1344  -0.5484   5.7165  15.5783 

#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 105.3075    13.2156   7.968 2.46e-10 ***
#>  X1            4.4357     0.5179   8.564 3.13e-11 ***
#>  ---
#>   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#> Residual standard error: 7.229 on 48 degrees of freedom
#> Multiple R-squared:  0.6044,	Adjusted R-squared:  0.5962 
#>  F-statistic: 73.35 on 1 and 48 DF,  p-value: 3.133e-11

# The Coefficients estimates and model p-values are lower than 0.05
# This means, the model is statistically significant
# The Adjusted R-Squared shows that the model acoounts for 59.62% of the data

# ==============================================================================


# ==== T-STATISTIC AND P-VALUE =================================================
# Assessing the t-statistic, F-statistic, and p-values manually


# capture model summary as an object
modelSummary <- summary(linearMod)

# captured model coefficients as an object
modelCoeffs <- modelSummary$coefficients

# get estimates for regressor
beta.estimate <- modelCoeffs["X1", "Estimate"]

# get standard error for regressor
std.error <- modelCoeffs["X1", "Std. Error"]

# calculating t-statistic of the coefficient estimates
t_value <- beta.estimate/std.error
# 8.564298


# calculating the p-value of the t-statistics
p_value <- 2*pt(-abs(t_value), df = nrow(sim.data)- 2)
# 3.132848e-11

# f- statistic
f_statistic <- linearMod$fstatistic[1]

# calculating the parameters for model p-value
f<- summary(linearMod)$fstatistic
#   value    numdf    dendf 
# 73.3472 1.00000 48.00000

model_p <-  pf(f[1],f[2],f[3], lower =FALSE)
# model p-value
# 3.132848e-11 

# Find AIC and BIC
AIC(linearMod)
# 343.659
BIC(linearMod)
# 349.395

# ==============================================================================


# ===== PREDICTING LINEAR MODELS ===============================================
# In this section, we generate a linear model of 80% of our data 
# This is then used to predict the other 20% of our data


# First, we create Training Test data

#  set seed to reproduce results of random sampling
set.seed(100)

# row indices for training data
trainingRowIndex <- sample(1:nrow(sim.data), 0.8*nrow(sim.data))

# make training Data
trainingData <- sim.data[trainingRowIndex, ]

# make test Data 
testData <- sim.data[-trainingRowIndex, ]

# Build the model on training data
lmMod <- lm(Y~X1, data =trainingData)

# Review diagnostic measures
summary(lmMod)
#> Coefficients:
#> Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  107.395     16.452   6.528 1.08e-07 ***
#>  X1             4.373      0.641   6.823 4.27e-08 ***

#> Residual standard error: 7.813 on 38 degrees of freedom
#> Multiple R-squared:  0.5506,	Adjusted R-squared:  0.5387 
#> F-statistic: 46.55 on 1 and 38 DF,  p-value: 4.275e-08


# What does all this tell us?
# both the predictor's and model's p value are less than 0.05
# So we have a statistically significant model

# ==============================================================================


# ==== PREDICTION ACCURACY =====================================================

# use the model of the training data to predict the testData
YPred <- predict(lmMod, testData)

# make a dataframe with the actual and predicted data
actual_preds <- data.frame(cbind(actuals = testData$Y,
                                 predicteds =YPred))
# write a csv with the actual and predicted values
write.csv(actual_preds, paste(path.results,"predictions.csv", sep = ""), 
          row.names = FALSE)

# inspect the actual_preds dataframe
head(actual_preds)
# actuals predicteds
# 3  209.0133   218.9149
# 11 225.9502   226.6082
# 18 204.2498   202.8588
# 19 217.0146   212.8311
# 21 212.0542   214.8210
# 23 206.8188   209.1757

# what is the correlation accuracy of the actual and predicted values
correlation_accuracy <- cor(actual_preds)
# 92.001 %


# calculate min_max accuracy of the model
min_max_accuracy <- mean(apply(actual_preds,1,min)/
                           apply(actual_preds,1,max))
# 98.31% (the higher the better)

# calculate mean absolute percentage error
mape <- mean(abs((actual_preds$predicteds - actual_preds$actuals))/
               actual_preds$ actuals)
# 1.73% , mean absolute percentage deviation (the lower the better)

# ==============================================================================

# ===== K-FOLD CROSS VALIDATION ===============================================

# the R packages needed for this section are lattice and DAAG

# Our previous model performed satisfactorily
# but how can we know it will all the time 
# we can rigorously test the model's performance as many times as we please
# This is done using a k-fold cross validation
# for our purposes, we use a 10 fold 

# change margins back to normal so the plot fits one window
par(mfrow = c(1,1))

# make plot
cvResults <- suppressWarnings( CVlm (sim.data, form.lm = Y ~ X1,
                                     m=10, dots=FALSE, seed=29,
                                     legend.pos="topleft",  printit=FALSE,
                                     main="10-Fold Cross Validation"))

# supress warning supresses the warnings
# Lines are very close and  parallel 
# This confirms the linear model's performance

# Write a csv with the predicted results from the 10 fold cross validation
write.csv(cvResults, paste(path.results,"cross.validataion.csv", sep = ""), 
          row.names = FALSE)


# find mean squared error of k-fold cross validation
attr(cvResults, 'ms')
# 53.15313 , the lower the better 
# ==============================================================================


