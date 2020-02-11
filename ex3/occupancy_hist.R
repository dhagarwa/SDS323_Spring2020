library(mosaic)
library(tidyverse)
gb = read.csv('../data/greenbuildings.csv')
summary(gb)
str(gb)
quantile(gb$leasing_rate, probs=c(0.01, 0.05, 0.1, 0.15, 0.2))
hist(gb$leasing_rate)

