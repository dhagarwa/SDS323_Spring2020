library(ggplot2)
library(dplyr)
library(knitr)
ABIA <- read.csv("ABIA.csv")
ABIA <- na.omit(ABIA)
attach(ABIA)
#show(ABIA)
#ABIA.Delay.data <- data.frame(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay)
#str(ABIA.Delay.data)


ggplot(data=ABIA) + geom_violin(aes(x=ArrDelay, y=Distance, fill=ArrDelay)) +   theme_bw(base_size=20) 

ggplot(data=ABIA) + geom_violin(aes(x=DepDelay, y=Distance, fill=DepDelay)) +   theme_bw(base_size=20) 
ggplot(data=ABIA) + geom_violin(aes(x=CarrierDelay, y=Distance, fill=CarrierDelay)) +   theme_bw(base_size=20) 
ggplot(data=ABIA) + geom_violin(aes(x=WeatherDelay, y=Distance, fill=WeatherDelay)) +   theme_bw(base_size=20) 
ggplot(data=ABIA) + geom_violin(aes(x=NASDelay, y=Distance, fill=NASDelay)) +   theme_bw(base_size=20) 
ggplot(data=ABIA) + geom_violin(aes(x=SecurityDelay, y=Distance, fill=SecurityDelay)) +   theme_bw(base_size=20) 
ggplot(data=ABIA) + geom_violin(aes(x=LateAircraftDelay, y=Distance, fill=LateAircraftDelay)) +   theme_bw(base_size=20) 


ggplot(ABIA, aes(x=Distance, y=ArrDelay)) + geom_point(color="red")
#ggplot(ABIA, aes(x=Distance, y=DepDelay)) + geom_point(color="orange")
#ggplot(ABIA, aes(x=Distance, y=CarrierDelay)) + geom_point(color="yellow") 
#ggplot(ABIA, aes(x=Distance, y=WeatherDelay)) + geom_point(color="green")
#ggplot(ABIA, aes(x=Distance, y=NASDelay)) + geom_point(color="blue")
#ggplot(ABIA, aes(x=Distance, y=SecurityDelay)) + geom_point(color="indigo")
#ggplot(ABIA, aes(x=Distance, y=LateAircraftDelay)) + geom_point(color="violet")