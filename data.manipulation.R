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
debt.raw$ï..Series.Name
unique(debt.raw$ï..Series.Name)

# make columns with yearly data numeric
for (i in 5:ncol(debt.raw)){
  debt.raw[,i] <- as.numeric(as.character(debt.raw[,i]))
  
}
# ==============================================================================

# ======== DATA  MANIPULATION ==================================================

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
fdi <- getvar("Foreign direct investment, net (BoP, current US$)" )
portfolio <- getvar("Portfolio investment, net (BoP, current US$)")
export <- getvar("Exports of goods and services (current US$)")
import <- getvar("Imports of goods and services (current US$)")
str(export)





# maybe I should create a function that find fi. values for each year
fi <- function(i){
 e.val<- export[,i]* 0.25
 i.val <- import[,i] * 0.25
 fdi.val <- fdi[,i] * 0.3
 p.val <- portfolio[,i] * 0.2
 fi.val <- e.val + i.val + fdi.val + p.val
 return(fi.val) 
}

# financial integration values for the years to be compared

fi.2009 <- fi(6)
fi.2014 <- fi(11)
fi.2019 <- fi(16)


# make dataframe with country names and others only
debt.clean <- data.frame(rep(export$Country.Name,3))

# make a vector of years 
years <- c(rep(2009, 48),rep(2014,48), rep(2019,48))

# add this vector as a column in debt. clean
debt.clean$years <- years

# add financial integration data
debt.clean$financial.integration <- c(fi.2009,fi.2014, fi.2019)

str(debt.stock)
# retrieve debt stock figures for 2009, 2014, and 2019
ds.2009 <- debt.stock[,6]
ds.2014 <- debt.stock[,11]
ds.2019 <- debt.stock[,16]

# add debt stock data to debt.clean
debt.clean$debt.stock <- c(ds.2009, ds.2014, ds.2019)

# retrieve debt service capabilities figures
dsc.2009 <- debt.service.cap[,6]
dsc.2014 <- debt.service.cap [,11]
dsc.2019 <- debt.service.cap [,16]

# add debt service data to debt.clean
debt.clean$debt.service.cap <- c(dsc.2009,dsc.2014,dsc.2019)


# set column names
colnames(debt.clean) <- c("country.name", "years", "financial.integration",
                          "debt.stock", "debt.service.cap")

# write debt.clean as a csv in path.data.clean
write.csv(debt.clean, paste(path.data.clean,"debt.clean.csv", sep = ""), 
          row.names = FALSE)

# make data set that drops data with NA financial integration
debt.new <- debt.clean[complete.cases(debt.clean),]

# write a new csv with complete data
write.csv(debt.new, paste(path.data.clean,"debt.complete.csv", sep = ""), 
          row.names = FALSE)

# ==============================================================================