library(mosaic)
library(tidyverse)

milk <- read.csv("milk.csv")

plot(sales~price, milk)
plot(log(sales)~log(price), milk)
ped <- lm(log(sales) ~ log(price), milk)
ped
# log(Q)=4.7-1.62log(p)
##Q = (e^4.7)p^-1.62

curve((x-1)*110*x^(-1.62), from=2, to=5)
curve((x-1)*110*x^(-1.62), from=2.5, to=2.7)
