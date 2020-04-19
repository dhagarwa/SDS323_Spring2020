# import data and examine it

greenbuildings <- read.csv("~/GitHub/SDS323_Spring2020/hw3/q1/greenbuildings.csv")
View(greenbuildings)
ok <- complete.cases(greenbuildings)
greenbuildings <- greenbuildings[ok,]

# note that shares is hugely skewed
# probably want a log transformation here
hist(greenbuildings$Rent)
summary(greenbuildings$Rent)

# much nicer :-)
hist(log(greenbuildings$Rent))

#### lasso (glmnet does L1-L2, gamlr does L0-L1) 
# I want to fit a lasso regression and do cross validation of K=10 folds 
# inorder to automate finiding independent variables and training & testing my data multiple times.
# cv.gamlr command in the gamlr does it for me.
# download gamlr library
library(gamlr) 

# i create a matrix of all my independent varaibles except for url from online_news data to make it easily readable for gamlr commands.
# the sparse.model.matrix function.
x = model.matrix(log(Rent) ~  . -CS_PropertyID - LEED -Energystar  , data=greenbuildings)[,-1] # do -1 to drop intercep

y = log(greenbuildings$Rent) # pull out `y' too just for convenience and do log(shares)- dependent variable


# Here I fit my lasso regression to the data and do my cross validation of k=10 n folds
# the cv.gamlr command does both things at once.
#(verb just prints progress)
cvl = cv.gamlr(x, y, nfold=5, verb=TRUE)

# plot the out-of-sample deviance as a function of log lambda
plot(cvl, bty="n")

