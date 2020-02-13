library(mosaic)
library(tidyverse)
library("ggplot2")
#Reading the data
gb = read.csv('../data/greenbuildings.csv')
summary(gb)
str(gb)
quantile(gb$leasing_rate, probs=c(0.01, 0.05, 0.1, 0.15, 0.2)) # to get an idea of quantiles

#Occupancy levels by green or non green

##Plotting the data to assess the occupancy levels
par(mfrow = c(6, 2)) # Create a 2 x 2 plotting matrix
hist(gb$leasing_rate, xlab="Occupancy level", main="Occupancy level histogram", col="blue") #plot histogram of occupancy level in percentage

#boxplot(gb$leasing_rate,col = "lightgray", main = "Occupancy levels box plot", outline=TRUE)

##Now plotting only green buildings occupancy level

gb_green = gb %>% filter(gb$green_rating > 0.5)
summary(gb_green)

hist(gb_green$leasing_rate, xlab="Green buildings occupancy level", main="Green buildings occupancy level histogram", col="green") #plot histogram of occupancy level in percentage

#boxplot(gb_green$leasing_rate,col = "lightgray", main = "Green buildings occupancy levels box plot", outline=TRUE)

##Now plotting only non green buildings occupancy level

gb_nongreen = gb %>% filter(gb$green_rating < 0.5)
summary(gb_nongreen)

hist(gb_nongreen$leasing_rate, xlab="Non-green buildings occupancy level", main="Non-green buildings occupancy level histogram", col="red") #plot histogram of occupancy level in percentage

boxplot(gb$leasing_rate, gb_green$leasing_rate, gb_nongreen$leasing_rate, names=c("All", "green", "non-green"), col = c("blue", "green", "red"), main = "Occupancy levels box plot", outline=TRUE)




#Occupancy levels by cluster id 


plot(gb$cluster, gb$leasing_rate,  main = "Occupancy levels by cluster", xlab="cluster id", ylab="occupancy level", col="blue")

#occupancy levels by storeys, note the buildind proposed is 15 storey, so it is better to check meadian occupancy for around 15 storey building
plot(gb$stories, gb$leasing_rate,  main = "Occupancy levels by stories", xlab="stories", ylab="occupancy level", col="blue")
plot(gb_green$stories, gb_green$leasing_rate,  main = "Occupancy levels by stories", xlab="stories", ylab="occupancy level", col="green")

plot(gb_nongreen$stories, gb_nongreen$leasing_rate,  main = "Occupancy levels by stories", xlab="stories", ylab="occupancy level", col="red")

ggplot(data = gb) +
  geom_point(mapping = aes(y = leasing_rate, x= stories), col="blue") +
  stat_summary(aes(y = leasing_rate,x=stories), fun.y=median, colour="red", geom="line",group=1) +
  labs(title="Scatter plot for occupancy level by stories")

boxplot(gb$Rent, gb_green$Rent, gb_nongreen$Rent, names=c("All", "green", "non-green"), col = c("blue", "green", "red"), main = "Rent box plot", outline=TRUE)  

# ggplot(data = gb) +
#   geom_point(mapping = aes(y = Rent, x= stories), col="blue") +
#   stat_summary(aes(y = Rent,x=stories), fun.y=median, colour="red", geom="line",group=1) +
#   labs(title="Scatter plot for rent by stories")



