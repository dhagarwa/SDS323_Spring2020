library(foreach)
library(tidyverse)
library(class)
library(mosaic)
library(FNN)
<<<<<<< HEAD:hw2/Question 1/question 1.R


=======
sclass = read.csv('../Question1/sclass.csv')
>>>>>>> 9ed238f47e4aba9713684bdf3368718139a60ffe:hw2/Question1/question1.R

sclass350 = subset(sclass, trim == '350')
sclass65AMG = subset(sclass, trim == '65 AMG')

n350 = nrow(sclass350)
n_train350 = round(0.8*n350) 
n_test350 = n350 - n_train350
train_cases350 = sample.int(n350, n_train350, replace=FALSE)
test_cases350 = setdiff(1:n350, train_cases350)
sclass350_train = sclass350[train_cases350,]
sclass350_test = sclass350[test_cases350,]

n65AMG = nrow(sclass65AMG)
n_train65AMG = round(0.8*n65AMG) 
n_test65AMG = n65AMG - n_train65AMG
train_cases65AMG = sample.int(n65AMG, n_train65AMG, replace=FALSE)
test_cases65AMG = setdiff(1:n65AMG, train_cases65AMG)
sclass65AMG_train = sclass65AMG[train_cases65AMG,]
sclass65AMG_test = sclass65AMG[test_cases65AMG,]

Xtrain350 = model.matrix(~ mileage, data=sclass350_train)
Xtest350 = model.matrix(~ mileage, data=sclass350_test)

ytrain350 = sclass350_train$price
ytest350 = sclass350_test$price

Xtrain65AMG = model.matrix(~ mileage, data=sclass65AMG_train)
Xtest65AMG = model.matrix(~ mileage, data=sclass65AMG_test)

ytrain65AMG = sclass65AMG_train$price
ytest65AMG = sclass65AMG_test$price

K = 2
rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

knn_model350 = knn.reg(Xtrain350, Xtest350, ytrain350, k=K)

rmse(ytest350, knn_model350$pred)

k_grid350 = exp(seq(log(1), log(300), length=100)) %>% round %>% unique
rmse_grid350 = foreach(K = k_grid350, .combine='c') %do% {
  knn_model350 = knn.reg(Xtrain350, Xtest350, ytrain350, k=K)
  rmse(ytest350, knn_model350$pred)
}

plot(k_grid350, rmse_grid350, log='x')
k_grid350[which.min(rmse_grid350)]
finalknn350 = knn.reg(train = Xtrain350, test = Xtest350, y = ytrain350, k=k_grid350[which.min(rmse_grid350)]
)
ypred_finalknn350 = finalknn350$pred

sclass350_test$ypred_finalknn350 = ypred_finalknn350
p_test350 = ggplot(data = sclass350_test) + 
  geom_point(mapping = aes(x = mileage, y = price))
p_test350 + geom_point(aes(x = mileage, y = ypred_finalknn350), color='red')

K =2
knn_model65AMG = knn.reg(Xtrain65AMG, Xtest65AMG, ytrain65AMG, k=K)

rmse(ytest65AMG, knn_model65AMG$pred)

k_grid65AMG = exp(seq(log(1), log(200), length=100)) %>% round %>% unique
rmse_grid65AMG = foreach(K = k_grid65AMG, .combine='c') %do% {
  knn_model65AMG = knn.reg(Xtrain65AMG, Xtest65AMG, ytrain65AMG, k=K)
  rmse(ytest65AMG, knn_model65AMG$pred)
}

plot(k_grid65AMG, rmse_grid65AMG, log='x')
k_grid65AMG[which.min(rmse_grid65AMG)]

finalknn65AMG = knn.reg(train = Xtrain65AMG, test = Xtest65AMG, y = ytrain65AMG, k=k_grid65AMG[which.min(rmse_grid65AMG)]
)
ypred_finalknn65AMG = finalknn65AMG$pred

sclass65AMG_test$ypred_finalknn65AMG = ypred_finalknn65AMG
p_test65AMG = ggplot(data = sclass65AMG_test) + 
  geom_point(mapping = aes(x = mileage, y = price))
p_test65AMG + geom_point(aes(x = mileage, y = ypred_finalknn65AMG), color='red')

