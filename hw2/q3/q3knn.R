library(tidyverse)
library(mosaic)
library(FNN)
library(caret)
data(online_news)

summary(online_news)

#Define the function
threshold = function(y) {
  ifelse(y > log(1400), 1, 0)
}

#Define the function
threshold_bog = function(y) {
  ifelse(y > log(1400), 1, 1)
}


# K-Nearest Neighbors Model

#Defining train-test sets for the hand-built regression model
KNNModel = do(1)*{
  N = nrow(online_news)
  train = round(0.8*N)
  test = (N-train)
  
  train_cases = sample.int(N, train, replace=FALSE)
  test_cases = setdiff(1:N, train_cases)
  
  online_train = online_news[train_cases,]
  online_test = online_news[test_cases,]
  
  Xtrain = model.matrix(~  is_weekend + num_imgs + num_videos + num_hrefs +  n_tokens_title + num_keywords +  self_reference_avg_sharess    + data_channel_is_socmed  + data_channel_is_lifestyle + data_channel_is_bus + data_channel_is_entertainment + data_channel_is_world   - 1, data=online_train)
  Xtest = model.matrix(~ is_weekend + num_imgs + num_videos + num_hrefs + n_tokens_title + num_keywords +   self_reference_avg_sharess  +  data_channel_is_socmed  + data_channel_is_lifestyle + data_channel_is_bus + data_channel_is_entertainment + data_channel_is_world    - 1 , data=online_test)
  Ytrain = log(online_train$shares)
  Ytest = log(online_test$shares)
  
  #Scaling the features (Standardization)
  scale_train = apply(Xtrain, 2, sd)
  Xtilde_train = scale(Xtrain, scale = scale_train)
  Xtilde_test = scale(Xtest, scale = scale_train)
  
  #The for loop
  library(foreach)
  k_grid = 5:15  %>% round %>% unique 
  rmse_grid = foreach(K = k_grid, .combine='c') %do% {
    knn_model = knn.reg(Xtilde_train, Xtilde_test, Ytrain, k=K)
    rmse(Ytest, knn_model$pred)
    actual <- factor(threshold(Ytest))
    predicted <- factor(threshold(knn_model$pred))
    results <- confusionMatrix(data=predicted, reference=actual)
    print(results)
    }
  }


#Plotting
#plot(k_grid, rmse_grid, log='x')
#abline(h=rmse(Ytest, yhat_test2)) 


