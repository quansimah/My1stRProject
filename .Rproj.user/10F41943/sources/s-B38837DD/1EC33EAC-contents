# =============================================================================
#
# main file where I put global variables, libraries needed loading and source 
# typically start with the version of R it works on
# 
R.version.string
#   "R version 4.0.3 (2020-10-10)"
#
# =============================================================================
# NOTES
# • 
# •
#
# You can also add the different R scripts in the project here as an overview
#
#
#
# =============================================================================
# --- global variables ---

# I like to set the working dir so I can always retrace where files should be 
# in case the code fails
wk.dir <- getwd() #


# =============================================================================
# ---- libraries ----

# install libraries needed for the project

# load the libraries needed for the project to run 
library(mvtnorm)
library(e1071)
library(lattice)
library(DAAG)
library(ggplot2)
library(DataCombine)
library(gvlma)
library(AER)
library(MASS)
library(corrplot)




# =============================================================================
# --- folder management ---

# names of project folders ("figures", "data.raw","data.clean","results")
# store names of the folders in an object
folder.names <- c("1.data.raw","2.data.clean", "3.results","4.figures")
# and make the folders if they don't exit yet. No need to understand this now
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# you need to store in an object the file path to these folders so we can 
# read from them and write to them.
# again no need to understand the code
path.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
path.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
path.results <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
path.fig <- paste(wk.dir, "/",folder.names[4], "/", sep = "")


# =============================================================================
# --- run scripts ---


# ==== end =================================================================




# structure your R scripts
# data.manip.R  for the the data manipulation and selection ->
#     -> save the clean file into the data.clean folder
# analysis.R  put data analysis here
# figures.R   Make figures here if this is more tricky, otherwise in 
#               analysis.R
# functions.R the functions your made for this project.





