library(tidyverse)
library(ggplot2)
library(gganimate)
library(plyr)
library(data.table)

#Read the csv
carsData <- read_csv("D:/carsData/big_epa_cars.csv")

#Converted consumption from barrels to gallons to litres
barrelGallon <- 42 #1 barrel = 42 gallons
gallonLitre <- 3.78541 #1 gallon = 3.78541 litres according to Google
carsData$consLitres <- carsData$barrels08*barrelGallon*gallonLitre

#Filter data
#unique(carsData$VClass) #take a look at vehicle types

#Data frames for fuel types, additional filter based on vehicle class
carsDataPremium <- filter(carsData, fuelType == 'Premium' & VClass == 'Midsize Cars')
carsDataRegular <- filter(carsData, fuelType == 'Regular' & VClass == 'Midsize Cars')
carsDataDiesel <- filter(carsData, fuelType == 'Diesel' & VClass == 'Midsize Cars')

#calculate quantiles for fuel consumption by year, as per instructions here: https://stackoverflow.com/questions/22403450/ddply-multiple-quantiles-by-group
premiumAll <- ddply(carsDataPremium, .(year), function(x) quantile (x$consLitres))
regularAll <- ddply(carsDataRegular, .(year), function(x) quantile (x$consLitres))
dieselAll <- ddply(carsDataDiesel, .(year), function(x) quantile (x$consLitres))

#add new columns to data frames before concatenation
premiumAll$fuelType <- "premium"
regularAll$fuelType <- "regular"
dieselAll$fuelType <- "diesel"

#concatenate tables
ConsumptFuelType <- rbind(premiumAll,regularAll,dieselAll)

#rename columns
ConsumptFuelType <- setnames(ConsumptFuelType, old=c("0%","25%","50%","75%","100%"), new=c("p0","p25","p50","p75","p100"))

#Make the plot, used columns p25, p50 and p75 to visualise variations in consumption
plot_anim <- ggplot(ConsumptFuelType, aes(x=year, y=p50, ymin=p25, ymax=p75, fill=fuelType, linetype=fuelType)) + 
  geom_point(aes(colour=fuelType), size = 2) +
  geom_ribbon(alpha=0.3) +
  geom_line(aes(colour=fuelType), size = 1) +
  scale_colour_manual(values=c(diesel="#7898FB",premium="#5CE5D5",regular="#B8FB3C"))+
  scale_fill_manual(values = c("#7898FB","#5CE5D5","#B8FB3C"))+#, labels = c("Diesel","Premium","Regular"))+
  scale_y_continuous(breaks = c(500,1000,1500, 2000,2500,3000,3500,4000), limits = c(500,4250))+
  scale_x_continuous(breaks = c(1975,1980,1985,1990,1995,2000,2005,2010,2015,2020))+
  theme(
    plot.background = element_rect(fill="#001437"),
    panel.background = element_rect(fill="#001437"),
    plot.caption = element_text(size=10,colour="#FE6B35"),
    axis.text = element_text(colour="#FE6B35"),
    plot.title = element_text(size=12,colour="#FE6B35"),
    plot.subtitle = element_text(size=10,colour="#FE6B35"),
    axis.title = element_text(size=11,colour="#FE6B35"),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12,colour="#FE6B35", ),
    legend.key = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#143061"),
  ) +
  ggtitle("Annual petroleum consumption in litres per fuel type (midsized cars)") +
  labs(caption = "Source: fueleconomy.gov",
       subtitle = "Quantiles 25, 50 (line), 75",
       y = "Consumption, litres",
       x = "Year"
  ) +
  transition_reveal(year)

#animate the plot
animate(plot_anim, renderer = gifski_renderer('D:/carsData/midsizeCarsFuelConsmpFinal.gif'),width = 500, height = 500)

        