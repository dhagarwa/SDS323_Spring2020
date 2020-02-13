library(tidyverse)
library(mosaic)

milk <- read.csv("~/GitHub/SDS323_Spring2020/ex4/milk.csv")

plot(sales ~ price, data=milk)
plot(log(sales) ~ log(price), data=milk)
lm_ped =lm(log(sales) ~log(price), data=milk)
coef(lm_ped)


curve((x-1)*110*x^(-1.62), from=2.5, to=2.7)
