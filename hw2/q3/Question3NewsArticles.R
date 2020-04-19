---
  title: "Predicting When Articles Go Viral"
author: "Dhwanit, Shristi"
date: "March 12, 2020"
output: pdf_document
---
  


##### Importiing, viewing, and analizing data
online_news <- read.csv("~/GitHub/SDS323_Spring2020/hw2/q3/online_news.csv")
#View(online_news)
str(online_news)

##### Model 1 Regress first and threshold second

hist(online_news$shares) 
# We should apply the log transformation since shares is very skewed 

# After log transformation
hist(log(online_news$shares))

# Fitting lasso regression and doing cross validation of K=10 folds to automate finding independent variables and training and testing my data multiple times simultaneously 

library(gamlr) 

# Creating a matrix of all the independent varaibles exculuding url from online_news data using the sparse.model.matrix function
x = sparse.model.matrix(log(shares) ~ . - url, data=online_news)[,-1] # -1 drops intercept

y = log(online_news$shares) # Pulling out `y' for convenience and taking the log of the dependent variable shares

# Fiting lasso regression to the data and doing cross validation of k=10 folds using the cv.gamlr command which does both tasks
# Verb = TRUE prints progress
cvl = cv.gamlr(x, y, nfold=10, verb=TRUE)

# Plotting out-of-sample deviance as a function of log lambda
plot(cvl, bty="n")

## CV minimum deviance selection
b.min = coef(cvl, select="min")
# value of lamda:
log(cvl$lambda.min)

sum(b.min!=0) # this gives the coefficent not 0

##########

# Predict number of shares
lhat_shares = predict(cvl, x) # log value of shares
hat_shares = exp(lhat_shares) # predicted values of shares
head (hat_shares, 50)

# Changing predicted number of shares into viral prediction(t_viral)
threshold_viral = ifelse(hat_shares > 1400, 1, 0)
head(threshold_viral, 50)

# Creating new variable "viral"
viral = ifelse(online_news$shares > 1400, 1, 0)
head(viral, 20)

# Creating confusion matrix
confusion_1= table(y = viral, yhat = threshold_viral)
print(confusion_1)
sum(diag(confusion_1))/sum(confusion_1) # This gives the sample accuracy for model 1

##### Model 2 Threshold first and regress/classify second.

# Running logistic lasso regression and cross validate with viral as the dependent variable
# family = "binomial" in this code is used to do a logistic regression instead of normal regression
#(verb just prints progress)
viral_cvl = cv.gamlr(x, viral, nfold=10, family="binomial", verb=TRUE)

# Plotting  the out-of-sample deviance as a function of log lambda
plot(viral_cvl, bty="n")

## CV minimum deviance selection
b.min = coef(viral_cvl, select="min")
print(b.min)
log(viral_cvl$lambda.min)
sum(b.min!=0) # This is random because of the CV randomness.

# Predicting number of viral
hat_viral = predict(viral_cvl, x, select = "min", type="response")
plot(hat_viral)
head (hat_viral, 50)

# Changing hat_viral to true/false prediction
b_hat_viral = ifelse(hat_viral > 0.5, 1, 0)
head(b_hat_viral, 50)

# Creating confusion matirx
confusion_2= table(y = viral, yhat = b_hat_viral)
print(confusion_2)
sum(diag(confusion_2))/sum(confusion_2) # This is the sample accuracy of model 2


##### Comparison of models

table(viral) # The actual number of viral or not viral articles
20082/39644  # 50.66 percent of articles were not viral which is the null hypothesis

print(confusion_1)
sum(diag(confusion_1))/sum(confusion_1) # The sample accuracy for model 1 is 56.8 percent
# Hence model 1 is (56.8-50.66) about a 6 percent improvement to the null model


print(confusion_2)
sum(diag(confusion_2))/sum(confusion_2) # The sample accuracy of model 2 is 63 percent
# Hence model 2 is 12.5 percent improvement to null model and about 6.2 percent improvement to model 1


# In conclusion based on True Positive Rate, False Positve Rate, False Discovery Rate, and general acuracy Model 2 does better than Model 1.

