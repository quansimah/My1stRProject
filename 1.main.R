# =============================================================================
#
# MAIN
# 
R.version.string
#   "R version 4.0.3 (2020-10-10)"
#
# =============================================================================
# NOTES
# open the scripts in the orders in which they are numbered.


# - "2.simulation.R" shows how the data used for the analyses was simulated
# - "3.simple.linear.regression.R" shows the processes involved in conducting 
# a simple linear regression
# - "4.assumptions.R" explores the assumptions of a linear regression
# - "5.multiple.regression.R" shows the processes for a multiple linear 
# regresssion
# - "6.data.manipulation.R" shows how the debt data obtained from the World Bank
# was cleaned
# - "7.data.analysis.R" shows the attempts at analysing the clean debt data
# - "8.saveplots.R" is a script where plots were saved into pdf's

# =============================================================================


# Set the working dir to retrace where files should be 
# in case the code fails
wk.dir <- getwd() #


# =============================================================================
# ---- libraries ----

# install libraries needed for the project
install.packages("mvtnorm")
install.packages("e1071")
install.packages("lattice")
install.packages("DAAG")
install.packages("ggplot2")
install.packages("gvlma")
install.packages("AER")
install.packages("MASS")
install.packages("corrplot")


# load the libraries needed for the project to run 
library(mvtnorm)
library(e1071)
library(lattice)
library(DAAG)
library(ggplot2)
library(gvlma)
library(AER)
library(MASS)
library(corrplot)




# =============================================================================
# --- folder management ---

# names of project folders ("figures", "data.raw","data.clean","results")
# store names of the folders in an object
folder.names <- c("1.data.raw","2.data.clean", "3.results","4.figures")
# make the folders if they don't exist yet. 
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

#Store in an object the file path to these folders so we can 
# read from them and write to them.
path.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
path.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
path.results <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
path.fig <- paste(wk.dir, "/",folder.names[4], "/", sep = "")
# =============================================================================



# ==== end =================================================================







