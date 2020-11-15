# 
#
# SIMULATION DATA
#
# 
# Creating a simulated data frame with which to carry out analyses

# ===== LOAD LIBRARY ===========================================================
library(mvtnorm)
# this package allows me to randomly derive multivariate vectors

# ====== SIMULATE DATA =========================================================
# set sample size to 50
n<- 50


# before simulating the data, set seed for reproducibilty
set.seed(1)

# Now let us randomly generate a bivariate matrix 
# let us assign the random number generation to the object X

# first, let us set the means for each column
mean_x = c(25,50)

# then, let us create the covariance matrix
# this tells us the covariance between each pair of elements of our random vectors
cov_x = cbind(c(5,1.25), c(1.25,5))

# Random Variable generation
# let us create a random multivariate object 
X <- rmvnorm(n, mean_x , sigma = cov_x)
# let us inspect X
head(X)
# The X columns will count as regressors for future analyses

# randomly generate error terms in u
u <- rnorm(n, sd = 5)

# set the dependent variable function of both randomly generated regressors
# this preset equation counts as the true population model
Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u

# ==============================================================================

# ===== NEW DATAFRAME ==========================================================

# Now, let's create a dataframe that contains our regressors and dependent
df <- cbind.data.frame(X[,1],X[,2],Y)

# let's give our new data frame column names
colnames(df) <- c("X1","X2","Y")

# let us inspect our new dataframe
head(df)

# to stop us from having to regenerate numbers each time this code is run;
# let us write a new csv of the simulated data in the data clean folder 
write.csv(df, paste(path.data.clean,"simulated.data.csv", sep = ""), 
          row.names = FALSE)

# now, let us move on to carry out our regression analyses

# ==============================================================================

