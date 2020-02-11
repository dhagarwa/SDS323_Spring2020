library(mosaic)
library(tidyverse)
creatinine = read.csv('../data/creatinine.csv')

lm1 = lm(creatclear ~ age, data = creatinine)
new_data = data.frame(age = c(55, 40, 60))
predict(lm1, new_data)
coef(lm1)
summary(lm1)
135-123.0203
112-110.6240

ggplot(data = creatinine) + 
  geom_point(mapping = aes(x = age, y = creatclear)) + 
  geom_abline(intercept = coef(lm1)[1] , slope = coef(lm1)[2], color="red")+
  labs(title = "Creatinine Clearing Rate Decreasing with Age") +
  labs(
    y = "Creatinine Clearing Rate (ml/min)",
    x = "Age (yrs)")


