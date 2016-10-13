library("UsingR")
data(galton)
par(mfrow=c(1,2))
str(galton)
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)
boxplot(galton$parent,galton$child,na.rm=TRUE,range=0,horizontal=TRUE)
par(mfrow=c(1,1))
summary(galton)
library(manipulate)
myHist <- function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
mean(galton$child)
plot(galton$parent,galton$child,pch=19,col="blue")

## working with linear modeling from princeton.edu/R data
fpe <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat")
str(fpe)
head(fpe,5)
#fit an ordinary linear model 
attach(fpe)
lmfit=lm(change~setting+effort)
lmfit
summary(lmfit,correlation=TRUE)
anova(lmfit)
par(mfrow=c(2,2))
plot(lmfit)
coef(lmfit)
residuals(lmfit)
names(lmfit)

# simple regression analysis

    #http://rtutorialseries.blogspot.com/2009/11/r-tutorial-series-simple-linear.html

samplereg=read.csv("dataset_simpleRegression.csv")
str(samplereg)
head(samplereg,6)
# we are using the unemployement rate to predict the fall enrollment
names(samplereg)=tolower(names(samplereg))
lmfit=lm(roll~unem,samplereg)
lmfit
summary(lmfit)
#using multiple predictors- weight and volume of books
library(DAAG)
install.packages("DAAG")
data(allbacks)
mlr=lm(weight~volume+cover,data=allbacks)
summary(mlr)

# using a different data set
states=read.csv("http://bit.ly/dasi_states")
str(states)
povlm=lm(poverty~female_house,data=states)
summary(povlm)
## draw a scatter plot
library(ggplot2)
g=ggplot(states,aes(female_house,poverty))
k=g+geom_point()+geom_smooth(method="lm")
k
anova(povlm)
# let's add a percentage white to the model
pov2lm=lm(poverty~female_house+white,data=states)
summary(pov2lm)
g=ggplot(states,aes(white,poverty))
k=g+geom_point()+geom_smooth(method="lm")
k
anova(pov2lm)
# do model inference using R
cog=read.csv("http://bit.ly/dasi_cognitive")
#let's fill the full  model for prediction
coglm=lm(kid_score~mom_hs+mom_iq+mom_work+mom_age,data=cog)
summary(coglm)
library(ggplot2)
par(mfrow=c(1,1))
plot(coglm)
# go through the combinations to see if the adjusted R square increase
coglm2=lm(kid_score~mom_hs+mom_iq,data=cog)
summary(coglm2)
# the Rsquared value increases by 1
#let's try adding mom age now to the model
coglm3=lm(kid_score~mom_hs+mom_iq+mom_age,data=cog)
summary(coglm3)
#the adjusted R went down again
#let's add'mom work to the equation without age
coglm4=lm(kid_score~mom_hs+mom_iq+mom_work,data=cog)
summary(coglm4)
#coglm4 has been the best predictors so far
coglm5=lm(kid_score~mom_iq,data=cog)
summary(coglm5)
#just predicting on the mom_iq falls the score
anova(coglm3)
# important to check the condition of residuals 
plot(coglm4$residuals~cog$mom_iq)
# here the residuals are randomely scattered around the 0 position
# 2nd condtion to check is nearly normal residuals with mean 0
# check by making a histogram
hist(coglm4$residuals)
# also make a normal plot for the residuals
qqnorm(coglm4$residuals)
qqline(coglm4$residuals)
## since there is not much skew, the condition is fairly satisfied
## next condition to check is the constant variablity of residuals
## we plot the residuals against the predicted values(y)
plot(coglm4$residuals~coglm4$fitted)
#there doens't apprear to be any varibility in the residuals vs fitted so this condition is met as well
#check the condition of independent residuals
# we just plot the residuals to see if the order of data collection has anything to do with independence
plot(coglm4$residuals)
names(coglm4)

## http://www.statmethods.net/stats/regression.html
## multiple linear regression
summary(coglm4)
## other useful functions
coefficients(coglm4)# model coefficients
confint(coglm4,level=0.95)
fitted(coglm4)# predicted valued
residuals(coglm4)#residuals
anova(coglm4)
vcov(coglm4)#covariance matrix for model parameters
influence(coglm4)# regression diagnostics
## diagnostic plots
layout(matrix(c(1,2,3,4),2,2))
plot(coglm4)
# compare two fit models
anova(coglm4,coglm)

#linear regression page from R Cookbook
#http://www.cookbook-r.com/Statistical_analysis/Regression_and_correlation/

set.seed(955)
xvar <- 1:20 + rnorm(20,sd=3)
zvar <- 1:20/4 + rnorm(20,sd=2)
yvar <- -2*xvar + xvar*zvar/5 + 3 + rnorm(20,sd=4)
df <- data.frame(x=xvar, y=yvar, z=zvar)
df
# correlation coefficient

cor(df$x,df$y)
cor(df)

#linear regression with multiple predictors

fit2=lm(y~x+z,data=df)
fit2
summary(fit2)

##Multiple linear regression - Abbas Al Sharif

library(MASS)
head(Boston)
names(Boston)

##split the data to have first 400 training data and rest test data
train=1:400
test=-train
train_data=Boston[train,]
test_data=Boston[test,]

#correlation between age and mdev
cor(train_data$age,train_data$medv)
plot(train_data$age,train_data$medv)

model=lm(medv~log(lstat)+age,data=train_data)
model
summary(model)
# try to use all predictors in the model
model=lm(medv~.,data=train_data)
summary(model)
## we can substract lstat and add log(lstat) to the model
model=lm(medv~. -lstat+log(lstat),data=train_data)
summary(model)

#use pairs() to find relationship between different variables
a=pairs(train_data)
##selecting only few and not all variables

pairs(train_data[,c(5,6,14)])

# we can use the VIF() (variance Inflation Factor) to determine which predictors were uncorrelated
#VIF is found in the package called car
install.packages("car")
library(car)
vif(model)
#correlations can be estimated using cor
a=cor(train_data)
round(cor(train_data),2)
# interaction term
model=lm(medv~log(lstat)*age,data=train_data)
summary(model)
#predict the test model using this
# get the actual values from the model
y=test_data$medv
#compute the predicted value
y_hat=predict(model,test_data[,-14])

#let's find the mean square error

error=y-y_hat
error_sq=error^2
MSE=mean(error)
MSE
par(mfrow=c(1,1))
a=plot(sort(y),sort(y_hat),type="b")

#dealing with categorical variable
library(ISLR)
install.packages("ISLR")

model2=lm(Sales~.,data=Carseats)
model2
summary(model2)
# lets look at the contrast of the Shelve to understand what's happening
contrasts(Carseats$ShelveLoc)

## Regression with categorical variables
#http://rtutorialseries.blogspot.com/2010/02/r-tutorial-series-regression-with.html

team=read.csv("dataset.csv",header=TRUE)
str(team)
dCONF=as.numeric(team$CONF)-1
dCONF
attach(team)
par(mfrow=c(1,1))
plot(CONF,TOTAL,main="Team Salaries by Conference",range=0,horizontal=TRUE)
#correlation between CONF and TOTAL
cor(dCONF,TOTAL)
#very small linear correlation
#linear modeling example
model1=lm(TOTAL~QB+dCONF,data = team)
summary(model1)
anova(model1)

#logistic regression
library(MASS)
str(menarche)
summary(menarche)
plot(Menarche/Total~Age,data=menarche)
#plotting a logistic regression model
fit=glm(cbind(Menarche,Total-Menarche)~Age,binomial,data=menarche)
#our data frame does not contain a row for every case (i.e., every girl upon whom data were collected). Therefore, we do not have a binary (0,1) coded response variable. No problem! If we feed glm( ) a table (or matrix) in which the first column is number of successes and the second column is number of failures, R will take care of the coding for us. In the above analysis, we made that table on the fly inside the model formula by binding "Menarche" and "Total ??? Menarche" into the columns of a table using cbind( ).
#let's see how closely our fitted values match with the curve
plot(Menarche/Total~Age,data=menarche)
lines(menarche$Age,fit$fitted,type="l",col="red")
title(main="Menarche data with fitted logistic regression line")
summary(fit)
exp(coef(fit))

#some more examples of logistic regression
file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
gorilla=read.csv(file)
head(gorilla)
str(gorilla)
cor(gorilla)
pairs(gorilla)
model2=glm(seen~W*C*CW,binomial,data=gorilla)
summary(model2)
#categorical predictors
head(UCBAdmissions)
ftable(UCBAdmissions,col.vars="Admit")
prop.table(ftable(UCBAdmissions,col.vars="Admit"),1)
ucb=data.frame(UCBAdmissions)
