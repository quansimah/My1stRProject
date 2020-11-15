#
#
#
# COMPARING MAPS
#  making maps of the data
# Create an African map
# I would have to clean up my country names once again
# and create subsets of data for each country

# ========= COUNTRY NAME CLEANING =============================================

debt.data$rep.export.Country.Name..3.[debt.data$rep.export.Country.Name..3.
                                      == "Cote d'Ivoire"] <- "Ivory Coast"
debt.data$rep.export.Country.Name..3.[debt.data$rep.export.Country.Name..3.== "Congo, Rep."] <- "Republic of Congo"
debt.data$rep.export.Country.Name..3.[debt.data$rep.export.Country.Name..3. == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"

# ==============================================================================

# ====== COUNTRY DATA SUBSETS ================================================

# subset 2009 data
data.2009 <- subset(debt.data,years == "2009")

# subset 2014 data
data.2014 <- subset(debt.data,years == "2014")

#  subset 2019 data
data.2019 <- subset(debt.data,years == "2019")
# ==============================================================================

# finding the range to set as limit
range(debt.data$log.financial.integration, na.rm = T)
# 7.658744 10.749719

# ==== MAKING MAPS =============================================================
# make a world map
world.map <- map_data(map = "world")

# create a  blank base map
p<- ggplot(world.map, aes (x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", colour = "white")



# join location of world map to country names

data.map <- left_join(data.2009, world.map,
                      by = c( "rep.export.Country.Name..3." = "region"))
# ==============================================================================


# ======= 2009 PLOTS ===========================================================
# drawing the plot for 2009 data
data.plot <- p + geom_polygon(data = data.map,
                              aes(x = long, y = lat, group = group,
                                  fill = log.financial.integration)) + 
  xlim(-20,60) + ylim(-35,40) +
  # Setting the color scale
  scale_fill_continuous( low = 'white',high = 'red4' )  + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle(" Estimated Financial Integration in SSA", subtitle = "2009")



debt.stock.plot <- p + geom_polygon(data = data.map,
                              aes(x = long, y = lat, group = group,
                                  fill = debt.stock)) + 
  xlim(-20,60) + ylim(-35,40) +
  # Setting the color scale
  scale_fill_continuous( low = 'white',high = 'red4' )  + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Debt Stock (% GNI)", subtitle = "2009")

debt.service.plot <- p + geom_polygon(data = data.map,
                                    aes(x = long, y = lat, group = group,
                                        fill = debt.service.cap)) + 
  xlim(-20,60) + ylim(-35,40) +
  # Setting the color scale
  scale_fill_continuous( low = 'white',high = 'red4' )  + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Debt Service Capabilities", subtitle = "2009")
# ==============================================================================


