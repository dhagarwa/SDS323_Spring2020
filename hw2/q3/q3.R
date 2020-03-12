library(tidyverse)
library(mosaic)
library(FNN)
data(online_news)

summary(online_news)


ggplot(data = online_news) +
  geom_point(mapping = aes(x = n_tokens_title , y = shares))


lm_t = lm(shares ~  n_tokens_title, data=online_news)
summary(lm_t)

 # Baseline model
 lm_small = lm(log(shares) ~ num_hrefs + num_imgs + num_videos + num_keywords + n_tokens_content + num_self_hrefs + n_tokens_title  + data_channel_is_socmed + data_channel_is_tech + data_channel_is_lifestyle + data_channel_is_bus + data_channel_is_entertainment + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday   , data=online_news)
summary(lm_small)

 #Define only the numerics of the train-test data sets
 N = nrow(online_news)
 train = round(0.8*N)
 test = (N-train)
 #Define the function
 rmse = function(y, yhat) {
   sqrt( mean( (y - yhat)^2 ) )
 }

 #Rmse iterations
 rmse1 <- NULL
 rmse2 <- NULL

 for (i in 1:1){

   #Picking data up for training and testing
   train_cases = sample.int(N, train, replace=FALSE)
   test_cases = setdiff(1:N, train_cases)

   #Define the train-test data sets (for all X's and Y)
   online_train = online_news[train_cases,]
   online_test = online_news[test_cases,]

   #Training
   #Base Model
   lm1 = lm(log(shares) ~  num_imgs + num_videos + num_keywords +  n_tokens_title + n_tokens_content + average_token_length , data=online_train)

   print(online_train)
   #lm1 = lm(log(shares) ~ avg_negative_polarity*global_rate_negative_words + self_reference_max_shares*self_reference_avg_sharess + title_subjectivity + is_weekend + data_channel_is_socmed + data_channel_is_tech + data_channel_is_lifestyle + data_channel_is_bus + data_channel_is_entertainment + data_channel_is_world + num_keywords+ n_tokens_title +  num_self_hrefs +  global_rate_positive_words*avg_positive_polarity + num_imgs*n_tokens_content + num_videos*n_tokens_content + n_tokens_title*title_sentiment_polarity, data=online_train)
   print(summary(lm1))
   # #Hand-built Model
   # lm2 = lm(price ~ lotSize + pctCollege  + heating  + bathrooms + bedrooms
   #          + rooms + fuel + centralAir + NewBuilt + landValue + NewBuilt*lotSize
   #          + centralAir*heating + pctCollege*age + landValue*fuel + heating*bedrooms
   #          , data=saratoga_train)

   #Testing
   yhat_test1 = predict(lm1, online_test)
   #print(yhat_test1)
   #yhat_test2 = predict(lm2, saratoga_test)
   viral_test = ifelse(online_test$shares > 1400, 1, 0)
   viral_pred = ifelse(log(yhat_test1) > log(1400), 1, 0)
   #print(viral_pred)
   #Run it on the actual and the predicted values
   rmse1[i]= rmse(online_test$shares, yhat_test1)
   rmse2[i]= rmse(viral_test, viral_pred)

 }

 mean(rmse1)
 mean(rmse2)
 
 # K-Nearest Neighbors Model

 # #Defining train-test sets for the hand-built regression model
 # KNNModel = do(1)*{
 #   N = nrow(online_news)
 #   train = round(0.8*N)
 #   test = (N-train)
 # 
 #   train_cases = sample.int(N, train, replace=FALSE)
 #   test_cases = setdiff(1:N, train_cases)
 # 
 #   online_train = online_news[train_cases,]
 #   online_test = online_news[test_cases,]
 # 
 #   Xtrain = model.matrix(~  num_hrefs + num_imgs + num_videos + num_keywords + n_tokens_content + num_self_hrefs + n_tokens_title  + data_channel_is_socmed + data_channel_is_tech + data_channel_is_lifestyle + data_channel_is_bus + data_channel_is_entertainment + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday -1 , data=online_train)
 #   Xtest = model.matrix(~  num_hrefs + num_imgs + num_videos + num_keywords + n_tokens_content + num_self_hrefs + n_tokens_title  + data_channel_is_socmed + data_channel_is_tech + data_channel_is_lifestyle + data_channel_is_bus + data_channel_is_entertainment + data_channel_is_world + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday + weekday_is_saturday + weekday_is_sunday -1 , data=online_test)
 #   Ytrain = online_train$shares
 #   Ytest = online_test$shares
 # 
 #   #Scaling the features (Standardization)
 #   scale_train = apply(Xtrain, 2, sd)
 #   Xtilde_train = scale(Xtrain, scale = scale_train)
 #   Xtilde_test = scale(Xtest, scale = scale_train)
 # 
 #   #The for loop
 #   library(foreach)
 #   k_grid = seq(2,10)
 #   rmse_grid = foreach(K = k_grid, .combine='c') %do% {
 #     knn_model = knn.reg(Xtilde_train, Xtilde_test, Ytrain, k=K)
 #     rmse(Ytest, knn_model$pred)
 #   }
 # }
 # knn_model_mean = colMeans(knn_model)
 # 
 # #Plotting
 # plot(k_grid, knn_model_mean)
 #abline(h=rmse(Ytest, yhat_test2)) 
 
 
 