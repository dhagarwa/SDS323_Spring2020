library(tidyverse)
library(mosaic)
library(FNN)
library(foreach)

data(SaratogaHouses)

summary(SaratogaHouses)

#Defining models

# Baseline model
lm_small = lm(price ~ bedrooms + bathrooms + lotSize, data=SaratogaHouses)

# 11 main effects
lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
                 fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=SaratogaHouses)

# Sometimes it's easier to name the variables we want to leave out
# The command below yields exactly the same model.
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
lm_medium2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=SaratogaHouses)

coef(lm_medium)
coef(lm_medium2)

# All interactions
# the ()^2 says "include all pairwise interactions"
lm_big = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=SaratogaHouses)


####
# Compare out-of-sample predictive performance
####

# Split into training and testing sets
n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
saratoga_train = SaratogaHouses[train_cases,] #Rows first then columns 
saratoga_test = SaratogaHouses[test_cases,]

# Fit to the training data
lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
lm2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
lm3 = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)

# Predictions out of sample
yhat_test1 = predict(lm1, saratoga_test)
yhat_test2 = predict(lm2, saratoga_test)
yhat_test3 = predict(lm3, saratoga_test)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

# Root mean-squared prediction error
rmse(saratoga_test$price, yhat_test1)
rmse(saratoga_test$price, yhat_test2)
rmse(saratoga_test$price, yhat_test3)


# easy averaging over train/test splits

n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

rmse_vals = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  # Fit to the training data
  lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
  lm2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
  lm3 = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)
  
  lm_dominate = lm(price ~ lotSize + age + livingArea + pctCollege + 
                     bedrooms + fireplaces + bathrooms + rooms + heating + fuel +
                     centralAir + lotSize:heating + livingArea:rooms + newConstruction + livingArea:newConstruction, data=saratoga_train)
  
  # Predictions out of sample
  yhat_test1 = predict(lm1, saratoga_test)
  yhat_test2 = predict(lm2, saratoga_test)
  yhat_test3 = predict(lm3, saratoga_test)
  yhat_test4 = predict(lm_dominate, saratoga_test)
  
  c(rmse(saratoga_test$price, yhat_test1),
    rmse(saratoga_test$price, yhat_test2),
    rmse(saratoga_test$price, yhat_test3),
    rmse(saratoga_test$price, yhat_test4))
}

rmse_vals
colMeans(rmse_vals)
boxplot(rmse_vals)
#########################################################################################################################################

str(SaratogaHouses)

# New bariables for "hand-built" model 

SaratogaHouses$NewBuilt <- ifelse(SaratogaHouses$age == 0, 1,0)
SaratogaHouses$NewBuilt

ConstructionCost <- SaratogaHouses$price - SaratogaHouses$landValue
head(ConstructionCost)

HeatingElectric <- SaratogaHouses[grep("electric", SaratogaHouses$heating), ]
head(HeatingElectric)

HeatingSteam <- SaratogaHouses[grep("hot water/steam", SaratogaHouses$heating), ]
head(HeatingSteam)

HeatingHotAir <- SaratogaHouses[grep("hot air", SaratogaHouses$heating), ]
head(HeatingHotAir)

FuelOil <- SaratogaHouses[grep("oil", SaratogaHouses$fuel), ]
head(FuelOil)

FuelGas <- SaratogaHouses[grep("gas", SaratogaHouses$fuel), ]
head(FuelGas)

FuelElectric <- SaratogaHouses[grep("electric", SaratogaHouses$fuel), ]
head(FuelElectric)

#Defining the models 
#Base model

BaseModel = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces +
               heating + bathrooms + rooms + fuel + centralAir + NewBuilt, data = SaratogaHouses)

#Hand Built Model
HandBuiltModel = lm(price ~ lotSize + pctCollege  + heating + bathrooms + bedrooms 
            + rooms + fuel + centralAir + NewBuilt + landValue + NewBuilt*lotSize
            + centralAir*heating + pctCollege*age + landValue*fuel + heating*bedrooms
            , data = SaratogaHouses)

#Define only the numerics of the train-test data sets 
N = nrow(SaratogaHouses)
train = round(0.8*N)
test = (N-train)
#Define the fution
rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

#Rmse iterations
rmse1 <- NULL
rmse2 <- NULL

for (i in seq(1:200)){
  #Picking data up for training and testing
  train_cases = sample.int(N, train, replace=FALSE)
  test_cases = setdiff(1:N, train_cases)
  
  #Define the train-test data sets (for all X's and Y)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  #Training
  #Base Model
  lm1 = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces +
             heating + bathrooms + rooms + fuel + centralAir + NewBuilt , data=saratoga_train)
  #Hand-built Model
  lm2 = lm(price ~ lotSize + pctCollege  + heating  + bathrooms + bedrooms 
           + rooms + fuel + centralAir + NewBuilt + landValue + NewBuilt*lotSize
           + centralAir*heating + pctCollege*age + landValue*fuel + heating*bedrooms
           , data=saratoga_train)
  
  #Testing 
  yhat_test1 = predict(lm1, saratoga_test)
  yhat_test2 = predict(lm2, saratoga_test)
  
  #Run it on the actual and the predicted values
  rmse1[i]= rmse(saratoga_test$price, yhat_test1)
  rmse2[i]= rmse(saratoga_test$price, yhat_test2)
}

mean(rmse1)
mean(rmse2)


# K-Nearest Neighbors Model

#Defining train-test sets for the hand-built regression model
KNNModel = do(100)*{
  N = nrow(SaratogaHouses)
  train = round(0.8*N)
  test = (N-train)
  
  train_cases = sample.int(N, train, replace=FALSE)
  test_cases = setdiff(1:N, train_cases)
  
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  Xtrain = model.matrix(~ lotSize + pctCollege  + heating  + bathrooms + bedrooms 
                        + rooms + fuel + centralAir + NewBuilt + landValue - 1, data=saratoga_train)
  Xtest = model.matrix(~ lotSize + pctCollege  + heating  + bathrooms + bedrooms 
                       + rooms + fuel + centralAir + NewBuilt + landValue - 1, data=saratoga_test)
  Ytrain = saratoga_train$price
  Ytest = saratoga_test$price
  
  #Scaling the features (Standardization)
  scale_train = apply(Xtrain, 2, sd) 
  Xtilde_train = scale(Xtrain, scale = scale_train)
  Xtilde_test = scale(Xtest, scale = scale_train) 
  
  #The for loop 
  library(foreach)
  k_grid = seq(2,100)
  rmse_grid = foreach(K = k_grid, .combine='c') %do% {
    knn_model = knn.reg(Xtilde_train, Xtilde_test, Ytrain, k=K)
    rmse(Ytest, knn_model$pred)
  }
}
knn_model_mean = colMeans(KNNModel)

#Plotting 
plot(k_grid, knn_model_mean)
abline(h=rmse(Ytest, yhat_test2)) 

#We conclude that variables giving the same data that is completely captured by another variable can be eliminated from the model. For example, the binary variable 'NewBuilt' is not essential because we can just look at the value of the age variable of the house and if its value is 0 then the house is newly built. When a variable does not completely capture all the information about the house then we should not eliminate it because then we will lose some important information.  For example, we should not eliminate bathrooms and bedrooms variables because knowing how many of bathrooms and bedrooms specifically is important for buyers which is not fully captured by the rooms variable. On the other hand, we cannot eliminate rooms and only have bedrooms and bathrooms because bedrooms and bathrooms are not the only type of rooms that effects house prices. Other types of rooms such as laundry room, storeroom, sunroom etc. are also included in rooms and how many rooms besides bathrooms and bedrooms are important in determining house prices.  
#Additionally, we have found that newer houses are bigger and are correlated with an increase in pricing. Also, it appears as if the age of the house is correlated with the percentage of college graduates living in the neighborhood and the higher the age and/or percent of college graduates, the higher is the predicted price. Furthermore, heating in terms of hot air affects the central air. Lastly, the fuel availability is correlated with the land value and which also affects the price.

save.image("/Users/shris/OneDrive/Documents/GitHub/SDS323_Spring2020/Homework 2/Question 2")


