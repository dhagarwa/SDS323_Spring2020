library(ggplot2)
library(foreach)
library(LICORS)
library(lattice)
library(caret)
library(reshape2)
library(olsrr)
<<<<<<< HEAD
=======


confirmed <- read.csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
>>>>>>> 931229652d04ddac640d3ee645926c8b0497e80e

confirmed <- read.csv("C:/Users/shris/OneDrive/Documents/GitHub/SDS323_Spring2020/Final_Project/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

deaths <- read.csv("C:/Users/shris/OneDrive/Documents/GitHub/SDS323_Spring2020/Final_Project/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

recovered <- read.csv("C:/Users/shris/OneDrive/Documents/GitHub/SDS323_Spring2020/Final_Project/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

summary(confirmed)

countries = c("Italy", "Spain", "Germany", "India", "Sweden", "Singapore", "Iran", "Korea, South")

confirmed_countries = confirmed[confirmed$Country.Region %in% countries,]
deaths_countries = deaths[deaths$Country.Region %in% countries,]
recovered_countries = recovered[recovered$Country.Region %in% countries,]


country = "India"

#confirmed_italy = confirmed[confirmed$Country.Region == "Italy", ]

#confirmed_italy_melt = melt(confirmed_italy, id=c("Province.State", "Country.Region", "Lat", "Long"), variable.name = "Day")

#View(confirmed_italy_melt)

### EXTRACTING INFECTED DATA OF ITALY
confirmed_countries_melt = melt(confirmed_countries, id=c("Province.State", "Country.Region", "Lat", "Long"), variable.name = "Day")
confirmed_countries_melt = confirmed_countries_melt[which(confirmed_countries_melt$value > -1),]
#View(confirmed_countries_melt)
ggplot(confirmed_countries_melt, aes(x=Day, y=log(value), group=Country.Region, color=Country.Region)) +
  geom_point() +
  geom_line()

#Extract Italy data from countries data
italy_cnf_melt = confirmed_countries_melt[which(confirmed_countries_melt$Country.Region == country), ]

#Extract infected number cases as a vector
italy_cnf = italy_cnf_melt$value
italy_cnf

#Extract delta infected vector delta_inf(t) = I(t) - I(t-1)
italy_delta_cnf = diff(italy_cnf)
italy_delta_cnf

#Cumulative sum of delta_inf
italy_cum_delta_cnf = cumsum(italy_delta_cnf)
italy_cum_delta_cnf

#italy_inf_melt$cum_delta_inf <- italy_cum_delta_inf
#View(italy_inf_melt)

###EXTRACTING DEATHS DATA OF ITALY 
deaths_countries_melt = melt(deaths_countries, id=c("Province.State", "Country.Region", "Lat", "Long"), variable.name = "Day")
deaths_countries_melt = deaths_countries_melt[which(deaths_countries_melt$value > -1),]
#View(confirmed_countries_melt)
ggplot(deaths_countries_melt, aes(x=Day, y=log(value), group=Country.Region, color=Country.Region)) +
  geom_point() +
  geom_line()

#Extract Italy data from countries death data
italy_deaths_melt = deaths_countries_melt[which(deaths_countries_melt$Country.Region == country), ]

#Extract deaths number as a vector
italy_deaths = italy_deaths_melt$value

italy_deaths

#Extract delta deathsvector delta_deaths(t) = D(t) - D(t-1)
italy_delta_deaths = diff(italy_deaths)
italy_delta_deaths

#Cumulative sum of delta_inf
italy_cum_delta_deaths = cumsum(italy_delta_deaths)
italy_cum_delta_deaths

#italy_inf_melt$cum_delta_inf <- italy_cum_delta_inf
#View(italy_inf_melt)




###EXTRACTING RECOVERED DATA OF ITALY 
recovered_countries_melt = melt(recovered_countries, id=c("Province.State", "Country.Region", "Lat", "Long"), variable.name = "Day")
recovered_countries_melt = recovered_countries_melt[which(recovered_countries_melt$value > -1),]
#View(confirmed_countries_melt)
ggplot(recovered_countries_melt, aes(x=Day, y=log(value), group=Country.Region, color=Country.Region)) +
  geom_point() +
  geom_line()

#Extract Italy data from countries recovered data
italy_recovered_melt = recovered_countries_melt[which(recovered_countries_melt$Country.Region == country), ]

#Extract recovered number as a vector
italy_recovered = italy_recovered_melt$value

italy_recovered

#Extract delta recovered vector delta_recovered(t) = R(t) - R(t-1)
italy_delta_recovered = diff(italy_recovered)
italy_delta_recovered

#Cumulative sum of delta_recovered
italy_cum_delta_recovered = cumsum(italy_delta_recovered)
italy_cum_delta_recovered

### Caluclating infected numbers from confirmed cases
#Extract infected number cases as a vector
italy_inf = italy_cnf - italy_recovered - italy_deaths

italy_inf

#Extract delta infected vector delta_inf(t) = I(t) - I(t-1)
italy_delta_inf = diff(italy_inf)
italy_delta_inf

#Cumulative sum of delta_inf
italy_cum_delta_inf = cumsum(italy_delta_inf)
italy_cum_delta_inf

###ESTIMATING CASE FATALITY RATIO
cor(italy_cum_delta_inf, italy_cum_delta_deaths)
#high correlation 0.996

#Making data frame of cumulative data
italy_cum_data_full = data.frame(delta_inf= italy_delta_inf, cum_delta_inf=italy_cum_delta_inf, delta_recovered = italy_delta_recovered, cum_delta_recovered= italy_cum_delta_recovered, delta_deaths = italy_delta_deaths, cum_delta_deaths=italy_cum_delta_deaths)


##VARY THE NUMBER OF DAYS CHOSEN FOR ANALYSIS
ndays = 65:97
ndays
gamma_data <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("est", "lwr", "upr")
colnames(gamma_data) <- x

beta_data <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("est", "lwr", "upr")
colnames(beta_data) <- x

R0_data <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("est", "lwr", "upr")
colnames(R0_data) <- x

#loop over days window chosen
for (days in ndays) {
  
italy_cum_data = italy_cum_data_full[1:days, ]
#View(italy_cum_data)


#fitting a linear model for case fatality ratio
italy_gamma <- lm(cum_delta_deaths ~ cum_delta_inf  -1  , data=italy_cum_data)  # build linear regression model on full data
summary(italy_gamma)

###ESTIMATING CASE  RECOVERY RATIO
cor(italy_cum_delta_inf, italy_cum_delta_recovered)
#high correlation 0.92
#fitting a linear model for case recovery ratio
italy_beta <- lm(cum_delta_recovered ~ cum_delta_inf -1 , data=italy_cum_data)  # build linear regression model on full data with no intercept

###ESTIMATING R0
#fitting a linear model for case basic reproducibility number R0
italy_R0 <- lm(cum_delta_deaths + cum_delta_recovered + cum_delta_inf  ~ I(cum_delta_recovered + cum_delta_deaths) - 1  , data=italy_cum_data)  # build linear regression model on full data


##Storing estimations and conf intervals

conf = confint(italy_gamma)
gamma_row <- list(est = summary(italy_gamma)$coefficients[1], lwr = conf[1], upr = conf[2])

gamma_row
gamma_data = rbind(gamma_data, gamma_row)


conf = confint(italy_beta)
beta_row <- list(est = summary(italy_beta)$coefficients[1], lwr = conf[1], upr = conf[2])

beta_row
beta_data = rbind(beta_data, beta_row)


conf = confint(italy_R0)
R0_row <- list(est = summary(italy_R0)$coefficients[1], lwr = conf[1], upr = conf[2])

R0_row
R0_data = rbind(R0_data, R0_row)

}

ggplot(gamma_data, aes(ndays, est)) + geom_point() + geom_line(aes(ndays, est))

ggplot(beta_data, aes(ndays, est)) + geom_point() + geom_line(aes(ndays, est))

ggplot(R0_data, aes(ndays, est)) + geom_point() + geom_line(aes(ndays, est))

View(R0_data)
View(beta_data)
View(gamma_data)

###PRINTING model and diagnostic test for gamma = case mortality rate
print(italy_gamma)
summary(italy_gamma)  # italy gamma model summary
confint(italy_gamma)
ols_plot_resid_qq(italy_gamma)
ols_test_correlation(italy_gamma)
ols_plot_resid_fit(italy_gamma)



###PRINTING model and diagnostic test for beta = case recovery ratio
print(italy_beta)
summary(italy_beta)  # italy gamma model summary
confint(italy_beta)
ols_plot_resid_qq(italy_beta)
ols_test_correlation(italy_beta)
ols_plot_resid_fit(italy_beta)

###PRINTING model and diagnostic test for R0
print(italy_R0)
summary(italy_R0)  # R0 model summary
confint(italy_R0)
ols_plot_resid_qq(italy_R0)
ols_test_correlation(italy_R0)
ols_plot_resid_fit(italy_R0)

##Predicted gamma using model
# add 'fit', 'lwr', and 'upr' columns to dataframe (generated by predict)
gamma_predict <- cbind(italy_cum_data, predict(italy_gamma, interval = 'confidence'))
gamma_prediction = predict(italy_gamma)
# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(gamma_predict, aes(cum_delta_inf, cum_delta_deaths))
p <- p + geom_point()
p <- p + geom_line(aes(cum_delta_inf, gamma_prediction))
p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.5)
p




##Predicted beta using model
# add 'fit', 'lwr', and 'upr' columns to dataframe (generated by predict)
beta_predict <- cbind(italy_cum_data, predict(italy_beta, interval = 'confidence'))
beta_prediction = predict(italy_beta)
# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(beta_predict, aes(cum_delta_inf, cum_delta_recovered))
p <- p + geom_point()
p <- p + geom_line(aes(cum_delta_inf, beta_prediction))
p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.5)
p




##Predicted R0 using model
# add 'fit', 'lwr', and 'upr' columns to dataframe (generated by predict)
R0_predict <- cbind(italy_cum_data, predict(italy_R0, interval = 'confidence'))
R0_prediction = predict(italy_R0)
# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(R0_predict, aes(cum_delta_recovered + cum_delta_deaths, cum_delta_recovered + cum_delta_deaths + cum_delta_inf))
p <- p + geom_point()
p <- p + geom_line(aes(cum_delta_deaths + cum_delta_recovered, R0_prediction))
p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.5)
p




####
####
####



library(deSolve)
library(RColorBrewer)

Infected <- italy_inf[50:97]
Infected
Recovered <- italy_recovered[50:97]
Deaths <- italy_deaths[50:97]
Confirmed <- italy_cnf[50:97]
day <- 0:(length(Infected)-1)
#N <- 830000 #pop of germany
N <- 1000000 #pop of India

###edit 1: use different boundary condition
###init <- c(S = N-1, I = 1, R = 0)
init <- c(S = N-Infected[1] - Recovered[1] - Deaths[1], I = Infected[1], R = Recovered[1], D = Deaths[1])


SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  ####edit 2; use equally scaled variables 
  with(par, { dS <- -alpha * (S/N) * I
  dI <- alpha * (S/N) * I - beta * I - gamma * I
  dR <- beta * I
  dD <- gamma * I
  list(c(dS, dI, dR, dD))
  })
}
# 
# SIR2 <- function(time, state, parameters) {
#   par <- as.list(c(state, parameters))
#   ####
#   #### use as change of variables variable
#   #### const = (beta-gamma)
#   #### delta = gamma/beta
#   #### R0 = beta/gamma > 1 
#   #### 
#   #### beta-gamma = beta*(1-delta)
#   #### beta-gamma = beta*(1-1/R0)
#   #### gamma = beta/R0
#   with(par, { 
#     beta  <- const/(1-1/R0)  
#     gamma <- const/(R0-1)  
#     dS <- -(beta * (S/N)      ) * I 
#     dI <-  (beta * (S/N)-gamma) * I 
#     dR <-  (             gamma) * I
#     list(c(dS, dI, dR))
#   })
# }
# 
# RSS.SIR2 <- function(parameters) {
#   names(parameters) <- c("const", "R0")
#   out <- ode(y = init, times = day, func = SIR2, parms = parameters)
#   fit <- out[ , 3]
#   RSS <- sum((Infected - fit)^2)
#   return(RSS)
# }
# 
# ### plotting different values R0
# 
# # use the ordinary exponential model to determine const = beta - gamma
# const <- coef(mod)[2]




RSS.SIR <- function(parameters) {
  names(parameters) <- c("alpha", "beta", "gamma")
  out <- ode(y = init, times = day, func = SIR, parms = parameters)
  fit <- out[ , 3] + out[ , 4] + out[ , 5]
  RSS <- sum((Confirmed- fit)^2)
  return(RSS)
}

lower = c(0, 0, 0)
upper = c(10, 1, 1)  ###adjust limit because different scale 1/N

### edit: get a good starting condition
#mod <- nls(Infected ~ a*exp(b*day),            start = list(a = Infected[20],  b = (1/20)*log(Infected[40]/Infected[40])))
#optimsstart <- c(2,1)*coef(mod)[2]

optimsstart <- c(0.7, 0.4,  0.2)

set.seed(12)
Opt <- optim(optimsstart, RSS.SIR, method = "L-BFGS-B", lower = lower, upper = upper,
             hessian = TRUE)
Opt


Opt_par <- Opt$par
names(Opt_par) = c("alpha", "beta", "gamma")
t <- 0:100
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
predict <- fit$I + fit$D + fit$R
plot(t, predict, col="green")
lines(day, Confirmed, col="red")
### estimated covariance matrix of coefficients
### note the large error, but also strong correlation (nearly 1)
## note scaling with estimate of sigma because we need to use Hessian of loglikelihood
sigest <- sqrt(Opt$value/(length(Infected)-1))
solve(1/(2*sigest^2)*Opt$hessian) 

####
####  using alternative parameters
####  for this we use the function SIR2
####

optimsstart <- c(coef(mod)[2],5)
lower = c(0, 1)
upper = c(1, 10^3)  ### adjust limit because we use R0 now which should be >1

set.seed(12)
Opt2 <- optim(optimsstart, RSS.SIR2, method = "L-BFGS-B",lower=lower, upper=upper,
              hessian = TRUE, control = list(maxit = 1000, 
                                             parscale = c(10^-3,1)))
Opt2

# now the estimated variance of the 1st parameter is small
# the 2nd parameter is still with large variance
#
# thus we can predict beta - gamma very well
# this beta - gamma is the initial growth coefficient
# but the individual values of beta and gamma are not very well known
#
# also note that hessian is not at the MLE since we hit the lower boundary
#
sigest <- sqrt(Opt2$value/(length(Infected)-1))
solve(1/(2*sigest^2)*Opt2$hessian)

#### We can also estimated variance by
#### Monte Carlo estimation
##
## assuming data to be distributed as mean +/- q mean
## with q such that mean RSS = 52030
##
## 
##


### Two functions RSS to do the optimization in a nested way
RSS.SIRMC2 <- function(const,R0) {
  parameters <- c(const=const, R0=R0)
  out <- ode(y = init, times = day, func = SIR2, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((Infected_MC - fit)^2)
  return(RSS)
}
RSS.SIRMC <- function(const) {
  optimize(RSS.SIRMC2, lower=1,upper=10^5,const=const)$objective
}

getOptim <- function() {
  opt1 <- optimize(RSS.SIRMC,lower=0,upper=1)
  opt2 <- optimize(RSS.SIRMC2, lower=1,upper=10^5,const=opt1$minimum)
  return(list(RSS=opt2$objective,const=opt1$minimum,R0=opt2$minimum))
}

# modeled data that we use to repeatedly generate data with noise
Opt_par <- Opt2$par
names(Opt_par) <- c("const", "R0")
modInfected <- data.frame(ode(y = init, times = day, func = SIR2, parms = Opt_par))$I

# doing the nested model to get RSS
set.seed(1)
Infected_MC <- Infected
modnested <- getOptim()

errrate <- modnested$RSS/sum(Infected) 


par <- c(0,0)
for (i in 1:100) {
  Infected_MC <- rnorm(length(modInfected),modInfected,(modInfected*errrate)^0.5)
  OptMC <- getOptim()
  par <- rbind(par,c(OptMC$const,OptMC$R0))
}
par <- par[-1,]

plot(par, xlab = "const",ylab="R0",ylim=c(1,1))
title("Monte Carlo simulation")
cov(par)


###conclusion: the parameter R0 can not be reliably estimated

##### End of Monte Carlo estimation


### plotting different values R0

# use the ordinary exponential model to determine const = beta - gamma
const <- coef(mod)[2]
R0 <- 1.1

# graph
plot(-100,-100, xlim=c(0,80), ylim = c(1,N), log="y", 
     ylab = "infected", xlab = "days", yaxt = "n")
axis(2, las=2, at=10^c(0:9),
     labels=c(expression(1),
              expression(10^1),
              expression(10^2),
              expression(10^3),
              expression(10^4),
              expression(10^5),
              expression(10^6),
              expression(10^7),
              expression(10^8),
              expression(10^9)))
axis(2, at=rep(c(2:9),9)*rep(10^c(0:8),each=8), labels=rep("",8*9),tck=-0.02)
title(bquote(paste("scenario's for different ", R[0])), cex.main = 1)

# time
t <- seq(0,60,0.1)

# plot model with different R0
for (R0 in c(1.1,1.2,1.5,2,3,5,10)) {
  fit <- data.frame(ode(y = init, times = t, func = SIR2, parms = c(const,R0)))$I
  lines(t,fit)
  text(t[601],fit[601],
       bquote(paste(R[0], " = ",.(R0))),
       cex=0.7,pos=4)
}

# plot observations
points(day,Infected)

