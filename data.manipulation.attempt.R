#
#
# DATA MANIPULATION
# Retrieving the data I need
#
# ======== RAW DATA ============================================================
# let's read the raw data from data.raw
debt.raw <- read.csv(paste(path.data.raw,"debt.data.csv",
                           sep = ""),
                     stringsAsFactors = FALSE, 
                     strip.white = TRUE)
# inspect the structure of data.raw
str(debt.raw)
head(debt.raw)
# inspect series names
unique(debt.raw$ï..Series.Name)

length(export$X1990..YR1990.)
export[2,8] +import[2,8]
str(export)
# make columns numeric
for (i in 5:ncol(debt.raw)){
  debt.raw[,i] <- as.numeric(as.character(debt.raw[,i]))
  
}
?subset

# make debt.new. 1 it's own csv
write.csv(debt.new.1, paste(path.data.clean,"debt.new.1.csv", sep = ""), 
          row.names = FALSE)

# making subsets of data with variables to be used
# use a function
# x represents the unique Series Name
getvar <- function(x){
  chosen.var <- subset(debt.raw,ï..Series.Name == x )
  return(chosen.var)
}

# creating data frames of the variables to be used

debt.stock <- getvar("External debt stocks (% of GNI)")
gni <- getvar("GNI (current US$)" )
debt.service.cap <- getvar("Total debt service (% of exports of goods, services and primary income)")
fdi <- getvar("Foreign direct investment, net (BoP, current US$)")
portfolio <- getvar("Portfolio investment, net (BoP, current US$)")
export <- getvar("Exports of goods and services (current US$)")
import <- getvar("Imports of goods and services (current US$)")
str(export)

# find average of data from 2009 to 20





# making calculations
# developing a function to calculate export + import /gni
# creating a vector of yearly data
# colums 3: 17 represent the colums with yearly data
# a loop might do better in this instance
for (i in 5:17){
  .val<- (export[,i]+ import[,i])/gni[,i]
  
}

# maybe I should create a function that find fi. values for each year
fi <- function(i){
  ei.val<- (export[,i]+ import[,i])/gni[,i]
  plusfdi <- ei.val + (fdi[,i]/gni[,i])
  pluspi <- plusfdi + (portfolio[,i])
  return(pluspi) 
}

# financial integration values for the year 2000
debt.raw$
  fi.2009 <- fi(6)
fi.2014 <- fi(11)
fi.2019 <- fi(16)


print(fi.val)
warnings()
length(fi.val)
is.matrix(fi.val)
debt.raw[,1:4]
?aggregate
# now how do I make these values remain attached to their countries and years
# see what happens if you record it as a 


# make dataframe with country names and others only
debt.clean <- data.frame(rep(export$Country.Name, 11))
?rep

# make a vector of years 
years <- c(rep(2009, 48), rep(2010,48), rep(2011,48), 
           rep(2012,48), rep(2013,48), rep(2014,48), rep(2015,48), rep(2016,48), 
           rep(2017,48), rep(2018,48), rep(2019,48))
# add this vector as a column in debt. clean
debt.clean$years <- years
adding financial integration data
fi <- r.bind()

debt.clean <- data.frame(rep(export$Country.Name,3))
debt.clean$financial.integration <- c(fi.2009,fi.2014, fi.2019)
# make a vector of years 
years <- c(rep(2009, 48),rep(2014,48), rep(2019,48))
is.vector(fi.data)
str(fi.data)
